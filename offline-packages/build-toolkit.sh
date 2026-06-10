#!/usr/bin/env bash
# Build a deployable offline Emacs toolkit tarball.
# Run on the ONLINE build machine.
#
# Picks one of the per-version ELPA mirrors under the cache
# ($OFFLINE_MIRRORS_DIR, default ~/.cache/emacs-offline-toolkit/mirrors/), built
# by create-install.sh, and bundles it alongside the rendered init.el, the
# Emacs-vanilla and Emacs-DIYer literate configs, an early-init.el if present,
# and an installer (setup.sh). Optional GNU Emacs source tarball for rebuild
# on the target.
#
# Output: $OUT_DIR/emacs-offline-toolkit-<ver>-<os>-<arch>-<stamp>.tar.gz
#
# Usage: ./build-toolkit.sh [options]
#   -t, --target SPEC       Target "emacs-VER" (skip interactive pick)
#   -o, --out-dir DIR       Output directory (default: $HOME/.emacs.d)
#   -s, --with-source VER   Bundle GNU Emacs source tarball for VER.
#                           Use "auto" to read sources/LATEST_STABLE.
#                           Pass --with-source none to skip.
#       --local-configs     Copy Emacs-vanilla/DIYer from ~/.emacs.d/<d>/
#                           instead of pulling fresh from GitHub.
#       --xz                Use xz -9e (.tar.xz) instead of gzip (.tar.gz).
#                           Smaller output, needs more RAM for compression.
#       --tools             Include the tools/ drop-zone (language servers, debug
#                           adapters). Off by default since they're large (~420 MB).
#                           Pass --tools for the initial install; omit for much
#                           smaller incremental/update tarballs. Filename gets
#                           a "-tools" suffix when included.
#       --smoke-test        Run the post-staging "boot rendered init.el" check
#                           (disabled by default; only useful when the target
#                           emacs-<VER> binary is available on this machine).
#   -l, --list              List available targets and exit
#   -h, --help              This help

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
EMACS_D_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
# Mirrors live outside the repo by default (build artefacts, not source).
# Override with $OFFLINE_MIRRORS_DIR if you want them elsewhere.
MIRRORS_DIR="${OFFLINE_MIRRORS_DIR:-${XDG_CACHE_HOME:-$HOME/.cache}/emacs-offline-toolkit/mirrors}"
OUT_DIR="${HOME}/.emacs.d"
WITH_SOURCE=0
EMACS_SOURCE_VERSION=""
TARGET=""
LOCAL_CONFIGS=0
USE_XZ=0
SMOKE_TEST=0
INCLUDE_TOOLS=0

# Literate config sources — fetched from GitHub main by default.
VANILLA_REPO="captainflasmr/Emacs-vanilla"
DIYER_REPO="captainflasmr/Emacs-DIYer"
CONFIG_REF="main"

list_targets() {
  find "${MIRRORS_DIR}" -mindepth 1 -maxdepth 1 -type d -name 'emacs-*' 2>/dev/null \
    | while read -r d; do
        # only list dirs that actually contain a mirror tarball
        if compgen -G "${d}/elpa-mirror-*.tar.gz" >/dev/null; then
          echo "$(basename "$d")"
        fi
      done | sort -u
}

pick_target() {
  local targets
  mapfile -t targets < <(list_targets)
  if [[ "${#targets[@]}" -eq 0 ]]; then
    echo "No mirror tarballs found under ${SCRIPT_DIR}" >&2
    exit 1
  fi
  if command -v fzf >/dev/null 2>&1; then
    printf '%s\n' "${targets[@]}" | fzf --prompt='Target: ' --height=40% --reverse
  else
    {
      echo "Available targets:"
      local i=1
      for t in "${targets[@]}"; do
        echo "  [$i] $t"
        i=$((i + 1))
      done
    } >&2
    local choice
    read -r -p "Select target [1-${#targets[@]}]: " choice </dev/tty
    [[ "$choice" =~ ^[0-9]+$ && "$choice" -ge 1 && "$choice" -le "${#targets[@]}" ]] || {
      echo "Invalid selection: $choice" >&2; exit 1; }
    echo "${targets[$((choice - 1))]}"
  fi
}

usage() {
  sed -n 's/^# \{0,1\}//p' "$0" | awk '/^Build/{flag=1} flag{print} /^  -h/{exit}'
  echo
  echo "Available targets:"
  list_targets | sed 's/^/  /'
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    -t|--target)      TARGET="$2"; shift 2 ;;
    -o|--out-dir)     OUT_DIR="$2"; shift 2 ;;
    -s|--with-source) WITH_SOURCE=1; EMACS_SOURCE_VERSION="$2"; shift 2 ;;
    --local-configs)  LOCAL_CONFIGS=1; shift ;;
    --xz)             USE_XZ=1; shift ;;
    --tools)          INCLUDE_TOOLS=1; shift ;;
    --smoke-test)     SMOKE_TEST=1; shift ;;
    -l|--list)        list_targets; exit 0 ;;
    -h|--help)        usage; exit 0 ;;
    *) echo "Unknown option: $1" >&2; usage; exit 1 ;;
  esac
done

[[ -z "$TARGET" ]] && TARGET="$(pick_target)"

# Allow either "emacs-27.2" or plain "27.2".
[[ "$TARGET" == emacs-* ]] || TARGET="emacs-${TARGET}"

TARGET_DIR="${MIRRORS_DIR}/${TARGET}"
[[ -d "$TARGET_DIR" ]] || { echo "Target dir not found: $TARGET_DIR" >&2; exit 1; }

# Newest mirror tarball in the target dir wins.
MIRROR_TARBALL="$(ls -t "${TARGET_DIR}"/elpa-mirror-*.tar.gz 2>/dev/null | head -n1)"
[[ -n "$MIRROR_TARBALL" && -f "$MIRROR_TARBALL" ]] \
  || { echo "No elpa-mirror-*.tar.gz in $TARGET_DIR" >&2; exit 1; }

EMACS_VERSION="${TARGET#emacs-}"
# Derive OS_SLUG and ARCH from the mirror tarball filename.
# Pattern: elpa-mirror-emacs-<VER>-<OS>-<ARCH>-<STAMP>-<N>pkgs.tar.gz
MIRROR_BASENAME="$(basename "$MIRROR_TARBALL" .tar.gz)"
_mtail="${MIRROR_BASENAME#elpa-mirror-emacs-${EMACS_VERSION}-}"
_mtail="${_mtail%-*-*pkgs}"      # strip -<STAMP>-<N>pkgs
ARCH="${_mtail##*-}"
OS_SLUG="${_mtail%-${ARCH}}"

# --with-source auto: read sources/LATEST_STABLE (single canonical version
# bundled regardless of the target mirror version).
if [[ "$WITH_SOURCE" -eq 1 && "$EMACS_SOURCE_VERSION" == "auto" ]]; then
  _srcfile="${SCRIPT_DIR}/sources/LATEST_STABLE"
  if [[ -f "$_srcfile" ]]; then
    EMACS_SOURCE_VERSION="$(head -n1 "$_srcfile" | tr -d '[:space:]')"
    echo ">> Auto source version from ${_srcfile}: ${EMACS_SOURCE_VERSION}"
  else
    echo "!! --with-source auto but ${_srcfile} missing; skipping source bundle" >&2
    WITH_SOURCE=0
  fi
fi
STAMP="$(date -u +%Y%m%dT%H%M%SZ)"
_TOOLS_SUFFIX=""
[[ "$INCLUDE_TOOLS" -eq 1 ]] && _TOOLS_SUFFIX="-tools"
TOOLKIT_NAME="emacs-offline-toolkit-${EMACS_VERSION}-${OS_SLUG}-${ARCH}-${STAMP}${_TOOLS_SUFFIX}"
STAGING="${OUT_DIR}/.${TOOLKIT_NAME}-staging-$$"
mkdir -p "$STAGING"
trap 'rm -rf "$STAGING"' EXIT

echo ">> Staging at: $STAGING"

# --- Core files ---
# Prefer the rendered per-version init.el; fall back to the template only if
# the per-version one hasn't been created yet.
if [[ -f "${TARGET_DIR}/init.el" ]]; then
  cp "${TARGET_DIR}/init.el" "${STAGING}/init.el"
else
  echo "!! No rendered init.el in ${TARGET_DIR} — re-run create-install.sh ${EMACS_VERSION}" >&2
  exit 1
fi
cp "${MIRROR_TARBALL}" "${STAGING}/"
[[ -f "${EMACS_D_DIR}/early-init.el" ]] && cp "${EMACS_D_DIR}/early-init.el" "${STAGING}/"
[[ -f "${EMACS_D_DIR}/abbrev_defs" ]] && cp "${EMACS_D_DIR}/abbrev_defs" "${STAGING}/"
[[ -f "${SCRIPT_DIR}/README.org" ]] && cp "${SCRIPT_DIR}/README.org" "${STAGING}/"

# Surface PACKAGES.txt at the toolkit root so users can inspect the bundled
# package list without having to extract the nested mirror tarball twice.
MIRROR_STEM="$(basename "$MIRROR_TARBALL" .tar.gz)"
if tar -xzOf "$MIRROR_TARBALL" "${MIRROR_STEM}/PACKAGES.txt" \
     > "${STAGING}/PACKAGES.txt" 2>/dev/null; then
  echo ">> Extracted PACKAGES.txt ($(wc -l < "${STAGING}/PACKAGES.txt" | tr -d ' ') packages)"
else
  rm -f "${STAGING}/PACKAGES.txt"
  echo "!! Mirror tarball has no PACKAGES.txt — skipping surface copy" >&2
fi

# --- Literate configs: Emacs-vanilla + Emacs-DIYer ---
# By default pulled fresh from GitHub; pass --local-configs to copy from
# ~/.emacs.d/<d>/ instead (useful when testing uncommitted edits offline).

_tar_excludes=(
  --exclude='.git' --exclude='.gitmodules' --exclude='.gitignore'
  --exclude='.claude' --exclude='.github'
  --exclude='*.elc' --exclude='*~' --exclude='.#*' --exclude='#*#'
  --exclude='eln-cache' --exclude='auto-save-list'
)

if [[ "$LOCAL_CONFIGS" -eq 1 ]]; then
  for d in Emacs-vanilla Emacs-DIYer; do
    if [[ -d "${EMACS_D_DIR}/${d}" ]]; then
      echo ">> Copying ${d} from local ${EMACS_D_DIR}/${d}..."
      tar -C "${EMACS_D_DIR}" "${_tar_excludes[@]}" -cf - "$d" \
        | tar -C "$STAGING" -xf -
    else
      echo "!! ${d} not found locally, skipping" >&2
    fi
  done
else
  for spec in "Emacs-vanilla:${VANILLA_REPO}" "Emacs-DIYer:${DIYER_REPO}"; do
    d="${spec%%:*}"
    repo="${spec#*:}"
    url="https://github.com/${repo}/archive/refs/heads/${CONFIG_REF}.tar.gz"
    echo ">> Fetching ${d} from ${repo}@${CONFIG_REF}..."
    tmp="$(mktemp "${TMPDIR:-/tmp}/${d}-XXXXXX.tar.gz")"
    if ! curl -fL --progress-bar "$url" -o "$tmp"; then
      echo "!! Failed to fetch ${url}" >&2
      rm -f "$tmp"; exit 1
    fi
    tar -xzf "$tmp" -C "$STAGING" "${_tar_excludes[@]}"
    rm -f "$tmp"
    # GitHub extracts as <repo-basename>-<ref>/; rename to <d>/.
    extracted="${STAGING}/$(basename "$repo")-${CONFIG_REF}"
    [[ -d "$extracted" ]] || { echo "!! extracted dir missing: $extracted" >&2; exit 1; }
    mv "$extracted" "${STAGING}/${d}"
  done
fi

# --- Starter init snippet (optional) ---
# starters/emacs-<VER>.el is an example init snippet, distilled from the
# author's live init.el. Installed on the target as
# ~/.emacs.d/init-starter.el and left unloaded. The user opts in by adding
#   (load (expand-file-name "init-starter" user-emacs-directory) t t)
# to their init.el, or by cherry-picking blocks from it.
STARTER_SRC="${SCRIPT_DIR}/starters/emacs-${EMACS_VERSION}.el"
if [[ -f "$STARTER_SRC" ]]; then
  echo ">> Copying starter from $(basename "$STARTER_SRC")..."
  cp "$STARTER_SRC" "${STAGING}/init-starter.el"
fi

# --- Coding companion starter (optional) ---
# starters/coding.el is a language-agnostic snippet (eglot hooks, flymake
# M-n/M-p, eldoc, corfu, dape F5-F11, cmake/typescript mode associations).
# Installed on the target as ~/.emacs.d/init-starter-coding.el, not auto-loaded.
# All starter files share the init-starter- prefix so they group in listings.
# Opt in by adding to init.el (on top of, or instead of, init-starter.el):
#   (load (expand-file-name "init-starter-coding" user-emacs-directory) t t)
CODING_SRC="${SCRIPT_DIR}/starters/coding.el"
if [[ -f "$CODING_SRC" ]]; then
  echo ">> Copying coding companion starter from $(basename "$CODING_SRC")..."
  cp "$CODING_SRC" "${STAGING}/init-starter-coding.el"
fi

# --- Local packages ---
# Drop user-created or locally-modified packages into offline-packages/local-packages/.
# Either foo.el at the top level, or foo/foo.el (+ siblings) in a subdirectory.
# Installed on the target at ~/.emacs.d/local-packages/ and wired onto load-path
# by init.el; not compiled (done on first load, version-agnostic).
LOCAL_PKG_SRC="${SCRIPT_DIR}/local-packages"
if [[ -d "$LOCAL_PKG_SRC" ]] \
   && find "$LOCAL_PKG_SRC" -mindepth 1 ! -name '.gitkeep' -print -quit | grep -q .; then
  echo ">> Copying local-packages from ${LOCAL_PKG_SRC}..."
  mkdir -p "${STAGING}/local-packages"
  tar -C "$LOCAL_PKG_SRC" "${_tar_excludes[@]}" --exclude='.gitkeep' -cf - . \
    | tar -C "${STAGING}/local-packages" -xf -
fi

# --- Tools drop-zone ---
# Non-Emacs binaries (language servers, debug adapters, formatters) dropped
# into offline-packages/tools/. Installed on the target at ~/.emacs.d/bin/.
# `cp -a' preserves executable bits. The coding starter expects JDTLS at
# ~/.emacs.d/bin/jdtls/bin/jdtls if present.
TOOLS_SRC="${SCRIPT_DIR}/tools"
if [[ "$INCLUDE_TOOLS" -eq 1 ]] \
   && [[ -d "$TOOLS_SRC" ]] \
   && find "$TOOLS_SRC" -mindepth 1 ! -name '.gitkeep' ! -name 'README.md' \
        -print -quit | grep -q .; then
  echo ">> Copying tools/ drop-zone from ${TOOLS_SRC}..."
  mkdir -p "${STAGING}/tools"
  tar -C "$TOOLS_SRC" --exclude='.gitkeep' --exclude='README.md' -cf - . \
    | tar -C "${STAGING}/tools" -xf -
fi

# --- Docs drop-zone ---
# Reference material (coding guide, cheat sheets). Shipped as-is so the
# target user has the same docs offline. Installed at ~/.emacs.d/docs/.
DOCS_SRC="${SCRIPT_DIR}/docs"
if [[ -d "$DOCS_SRC" ]] \
   && find "$DOCS_SRC" -mindepth 1 ! -name '.gitkeep' -print -quit | grep -q .; then
  echo ">> Copying docs/ drop-zone from ${DOCS_SRC}..."
  mkdir -p "${STAGING}/docs"
  tar -C "$DOCS_SRC" "${_tar_excludes[@]}" --exclude='.gitkeep' -cf - . \
    | tar -C "${STAGING}/docs" -xf -
fi

# --- Optional GNU Emacs source tarball ---
# Pulled from / cached in sources/ (shared with fetch-source.sh). A download
# failure (offline machine, corporate TLS interception, etc.) is non-fatal —
# the toolkit is built without the source bundle and a warning is surfaced.
SOURCE_SKIP_REASON=""
if [[ "$WITH_SOURCE" -eq 1 ]]; then
  SOURCES_DIR="${SCRIPT_DIR}/sources"
  mkdir -p "$SOURCES_DIR"
  SRC_NAME="emacs-${EMACS_SOURCE_VERSION}.tar.xz"
  SRC_PATH="${SOURCES_DIR}/${SRC_NAME}"
  if [[ ! -f "$SRC_PATH" ]]; then
    echo ">> Downloading ${SRC_NAME} into sources/..."
    if ! curl -fL --progress-bar \
         "https://ftpmirror.gnu.org/emacs/${SRC_NAME}" -o "$SRC_PATH"; then
      echo "!! Source download failed — continuing without the source bundle." >&2
      echo "   Pre-fetch on a machine with working TLS:" >&2
      echo "     ./fetch-source.sh ${EMACS_SOURCE_VERSION}" >&2
      rm -f "$SRC_PATH"
      WITH_SOURCE=0
      SOURCE_SKIP_REASON="download failed (${SRC_NAME})"
    fi
  else
    echo ">> Using sources/${SRC_NAME}"
  fi
  [[ "$WITH_SOURCE" -eq 1 ]] && cp "$SRC_PATH" "$STAGING/"
fi

# --- Installer: setup.sh ---
# Instead of littering the target directory with sibling .pre-toolkit-<STAMP>
# entries, all backups for each install now go into a single timestamped
# directory:  ~/.emacs.d/.backups/<STAMP>/
# That keeps the main areas clean and makes rollback a single directory swap.
# Mirror extractions under $HOME are stashed under
#  ~/.emacs.d/.backups/<STAMP>/home-mirrors/
#   so they are grouped with the rest of the install's backups.
cat > "${STAGING}/setup.sh" <<'SETUPEOF'
#!/usr/bin/env bash
# setup.sh — install the bundled Emacs toolkit on an offline machine.
# Run from inside the extracted toolkit directory.
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
EMACS_D="${HOME}/.emacs.d"
STAMP="$(date +%Y%m%dT%H%M%S)"
BACKUP_DIR="${EMACS_D}/.backups/${STAMP}"

echo ">> Target: ${EMACS_D}"
mkdir -p "$EMACS_D"

# Back up existing files/directories into a single timestamped directory.
# This keeps ~/.emacs.d and ~/ clean — all backups for this install live
# under ~/.emacs.d/.backups/<STAMP>/, and rollback just swaps that set back.
backup_item() {
  local src="$1" dest_base="$2"
  if [[ ! -e "$src" ]]; then
    return 0
  fi
  mkdir -p "$(dirname "$dest_base")"
  echo "   backing up $(basename "$src") -> .backups/${STAMP}/$(basename "$dest_base")"
  mv "$src" "$dest_base"
}

mkdir -p "$BACKUP_DIR"

# Back up existing init files so we don't silently clobber an existing setup.
for f in init.el early-init.el abbrev_defs init-starter.el init-starter-coding.el; do
  if [[ -e "${EMACS_D}/${f}" && ! -L "${EMACS_D}/${f}" ]]; then
    backup_item "${EMACS_D}/${f}" "${BACKUP_DIR}/${f}"
  fi
done

# Move any existing ~/.emacs.d/elpa/ aside so `my/ensure-package' sees every
# package as not-installed on next launch and reinstalls from the freshly
# extracted mirror. Without this, stale .elc/.eln from the old mirror stay put.
if [[ -d "${EMACS_D}/elpa" ]]; then
  backup_item "${EMACS_D}/elpa" "${BACKUP_DIR}/elpa"
fi

# Move any pre-existing mirror extractions aside too — same reason, and avoids
# the init.el's `string>' sort silently picking up stale mirrors after upgrades.
# They go into a subdirectory so they don't collide with .emacs.d items.
_mirror_count=0
for old in "${HOME}"/elpa-mirror-emacs-*; do
  [[ -d "$old" ]] || continue
  _mirror_count=$(( _mirror_count + 1 ))
  backup_item "$old" "${BACKUP_DIR}/home-mirrors/$(basename "$old")"
done

# Extract ELPA mirror into $HOME (init.el auto-detects ~/elpa-mirror-emacs-*).
for m in "$HERE"/elpa-mirror-*.tar.gz; do
  [[ -f "$m" ]] || continue
  echo ">> Extracting $(basename "$m") into $HOME"
  tar -xzf "$m" -C "$HOME"
done

# Install the literate config directories.
for d in Emacs-vanilla Emacs-DIYer; do
  if [[ -d "${HERE}/${d}" ]]; then
    if [[ -d "${EMACS_D}/${d}" ]]; then
      backup_item "${EMACS_D}/${d}" "${BACKUP_DIR}/${d}"
    fi
    echo ">> Installing ${d} to ${EMACS_D}/${d}"
    mkdir -p "${EMACS_D}/${d}"
    cp -a "${HERE}/${d}/." "${EMACS_D}/${d}/"
  fi
done

# Install local-packages if bundled. Source only, no byte-compilation — the
# target Emacs will compile on first load if it wants to.
if [[ -d "${HERE}/local-packages" ]]; then
  if [[ -d "${EMACS_D}/local-packages" ]]; then
    backup_item "${EMACS_D}/local-packages" "${BACKUP_DIR}/local-packages"
  fi
  echo ">> Installing local-packages to ${EMACS_D}/local-packages"
  mkdir -p "${EMACS_D}/local-packages"
  cp -a "${HERE}/local-packages/." "${EMACS_D}/local-packages/"
fi

# Install init.el and (optional) early-init.el.
cp "${HERE}/init.el" "${EMACS_D}/init.el"
[[ -f "${HERE}/early-init.el" ]] && cp "${HERE}/early-init.el" "${EMACS_D}/early-init.el"
[[ -f "${HERE}/abbrev_defs" ]] && cp "${HERE}/abbrev_defs" "${EMACS_D}/abbrev_defs"

# Install starter snippet if bundled. Does NOT auto-load — opt in from init.el
# with: (load (expand-file-name "init-starter" user-emacs-directory) t t)
if [[ -f "${HERE}/init-starter.el" ]]; then
  cp "${HERE}/init-starter.el" "${EMACS_D}/init-starter.el"
  echo ">> Starter snippet installed at ${EMACS_D}/init-starter.el (not auto-loaded)"
fi

# Install coding companion starter if bundled. Also NOT auto-loaded — opt in
# with: (load (expand-file-name "init-starter-coding" user-emacs-directory) t t)
if [[ -f "${HERE}/init-starter-coding.el" ]]; then
  cp "${HERE}/init-starter-coding.el" "${EMACS_D}/init-starter-coding.el"
  echo ">> Coding companion installed at ${EMACS_D}/init-starter-coding.el (not auto-loaded)"
fi

# Install tools/ drop-zone (language servers, debug adapters) to ~/.emacs.d/bin/.
# `cp -a' keeps the executable bits. Users can either add ~/.emacs.d/bin to PATH
# or reference binaries by absolute path from their eglot-server-programs.
if [[ -d "${HERE}/tools" ]]; then
  if [[ -d "${EMACS_D}/bin" ]]; then
    backup_item "${EMACS_D}/bin" "${BACKUP_DIR}/bin"
  fi
  echo ">> Installing tools/ to ${EMACS_D}/bin"
  mkdir -p "${EMACS_D}/bin"
  cp -a "${HERE}/tools/." "${EMACS_D}/bin/"
fi

# Install docs/ drop-zone (coding guide, reference material).
if [[ -d "${HERE}/docs" ]]; then
  if [[ -d "${EMACS_D}/docs" ]]; then
    backup_item "${EMACS_D}/docs" "${BACKUP_DIR}/docs"
  fi
  echo ">> Installing docs/ to ${EMACS_D}/docs"
  mkdir -p "${EMACS_D}/docs"
  cp -a "${HERE}/docs/." "${EMACS_D}/docs/"
fi

# Report on optional Emacs source.
SRC="$(ls "${HERE}"/emacs-*.tar.xz 2>/dev/null | head -n1 || true)"
if [[ -n "$SRC" && -f "$SRC" ]]; then
  echo
  echo ">> Bundled Emacs source: $(basename "$SRC")"
  echo "   To build from source:"
  echo "     tar -xf '$SRC' -C /tmp"
  echo "     cd /tmp/$(basename "$SRC" .tar.xz)"
  echo "     ./configure --prefix=\"\$HOME/.local\" && make -j\$(nproc) && make install"
fi

# Summarise backups so the user knows where to look.
_items="$(find "$BACKUP_DIR" -maxdepth 1 -mindepth 1 | wc -l)"
if [[ "$_items" -gt 0 ]]; then
  echo ">> Previous state backed up to ${EMACS_D}/.backups/${STAMP}/"
fi
# Old flat backups (pre-toolkit-<STAMP> suffix) are still present if setup
# was run with an older version of this script. List them as a reminder.
_old_count=0
for _old in "${EMACS_D}"/*.pre-toolkit-*; do
  [[ -e "$_old" ]] || continue
  _old_count=$(( _old_count + 1 ))
done
for _old in "${HOME}"/elpa-mirror-emacs-*.pre-toolkit-*; do
  [[ -e "$_old" ]] || continue
  _old_count=$(( _old_count + 1 ))
done
if [[ "$_old_count" -gt 0 ]]; then
  echo "!! ${_old_count} old .pre-toolkit-* backup(s) found under ${EMACS_D} and ${HOME}."
  echo "   To clean up: rm -rf ${EMACS_D}/*.pre-toolkit-* ${HOME}/elpa-mirror-emacs-*.pre-toolkit-*"
fi

echo
echo "Done. Launch Emacs; *Messages* should report:"
echo "  my/offline-packages=t  dir=/home/you/elpa-mirror-emacs-..."
SETUPEOF
chmod +x "${STAGING}/setup.sh"

# --- Windows installer: setup.bat ---
# Installs the bundled Emacs toolkit on a Windows offline machine.
# Mirrors the logic of setup.sh using cmd.exe / batch conventions.
cat > "${STAGING}/setup.bat" <<'SETUPBAT'
@echo off
setlocal enabledelayedexpansion

set HERE=%~dp0

:: Use Windows' bundled bsdtar explicitly. The MSYS/Git-bash GNU tar that is
:: often first on PATH treats a "z:\..." archive path as a remote host ("z:")
:: and fails with "Cannot connect to z: resolve failed".
set "TAR=%SystemRoot%\System32\tar.exe"
if not exist "%TAR%" set "TAR=tar"

:: Accept either EMACS_D path (ending with .emacs.d) or HOME path.
:: Falls back to %%HOME%% or %%USERPROFILE%%.
if not "%1"=="" (
  set "TARGET=%1"
  :: Strip trailing separator if present
  if "!TARGET:~-1!"=="\" set "TARGET=!TARGET:~0,-1!"
  if "!TARGET:~-1!"=="/" set "TARGET=!TARGET:~0,-1!"
  :: Check if it's an .emacs.d path (last 8 chars == ".emacs.d")
  if /i "!TARGET:~-8!"==".emacs.d" (
    set EMACS_D=!TARGET!
    set MY_HOME=!TARGET:~0,-8!
    :: Strip any separator left at the end of MY_HOME
    if "!MY_HOME:~-1!"=="\" set MY_HOME=!MY_HOME:~0,-1!
    if "!MY_HOME:~-1!"=="/" set MY_HOME=!MY_HOME:~0,-1!
  ) else (
    set MY_HOME=!TARGET!
    set EMACS_D=!TARGET!\.emacs.d
  )
) else (
  rem Use %HOME% only if it is an absolute Windows path (drive-letter form,
  rem e.g. C-colon-backslash). Under MSYS/Git-bash HOME is often "." or
  rem "/home/user", which would produce invalid relative targets.
  set "MY_HOME="
  if not "%HOME%"=="" (
    set "H=%HOME%"
    if "!H:~1,1!"==":" set "MY_HOME=%HOME%"
  )
  if "!MY_HOME!"=="" set "MY_HOME=%USERPROFILE%"
  set "EMACS_D=!MY_HOME!\.emacs.d"
)
for /f %%I in ('powershell -Command "Get-Date -Format 'yyyyMMddTHHmmss'"') do set STAMP=%%I
set BACKUP_DIR=%EMACS_D%\.backups\%STAMP%

echo ^>^> Target: %EMACS_D%
echo ^>^> HOME:     %MY_HOME%
if not exist "%EMACS_D%" mkdir "%EMACS_D%"
if not exist "%BACKUP_DIR%" mkdir "%BACKUP_DIR%"

:: Back up existing init files so we don't silently clobber an existing setup.
for %%f in (init.el early-init.el abbrev_defs init-starter.el init-starter-coding.el) do (
  if exist "%EMACS_D%\%%f" (
    echo    backing up %%f -^> .backups\%STAMP%\%%f
    move "%EMACS_D%\%%f" "%BACKUP_DIR%\%%f" >nul
  )
)

:: Move any existing elpa aside so packages reinstall from the fresh mirror.
if exist "%EMACS_D%\elpa" (
  echo    backing up elpa -^> .backups\%STAMP%\elpa
  move "%EMACS_D%\elpa" "%BACKUP_DIR%\elpa" >nul
)

:: Move any pre-existing mirror extractions aside too.
for /d %%d in ("%MY_HOME%\elpa-mirror-emacs-*") do (
  if exist "%%d" (
    if not exist "%BACKUP_DIR%\home-mirrors" mkdir "%BACKUP_DIR%\home-mirrors"
    echo    backing up %%~nxd -^> .backups\%STAMP%\home-mirrors\%%~nxd
    move "%%d" "%BACKUP_DIR%\home-mirrors\%%~nxd" >nul
  )
)

:: Extract ELPA mirror into %MY_HOME% (init.el globs ~/elpa-mirror-emacs-*).
for %%m in ("%HERE%elpa-mirror-*.tar.gz") do (
  if exist "%%m" (
    echo ^>^> Extracting %%~nxm into %MY_HOME%
    "%TAR%" -xzf "%%m" -C "%MY_HOME%"
  )
)

:: Install the literate config directories.
for %%d in (Emacs-vanilla Emacs-DIYer) do (
  if exist "%HERE%%%d" (
    if exist "%EMACS_D%\%%d" (
      echo    backing up %%d -^> .backups\%STAMP%\%%d
      move "%EMACS_D%\%%d" "%BACKUP_DIR%\%%d" >nul
    )
    echo ^>^> Installing %%d to %EMACS_D%\%%d
    if not exist "%EMACS_D%\%%d" mkdir "%EMACS_D%\%%d"
    xcopy "%HERE%%%d\." "%EMACS_D%\%%d\" /E /I /H /Y >nul
  )
)

:: Install local-packages if bundled.
if exist "%HERE%local-packages" (
  if exist "%EMACS_D%\local-packages" (
    echo    backing up local-packages -^> .backups\%STAMP%\local-packages
    move "%EMACS_D%\local-packages" "%BACKUP_DIR%\local-packages" >nul
  )
  echo ^>^> Installing local-packages to %EMACS_D%\local-packages
  if not exist "%EMACS_D%\local-packages" mkdir "%EMACS_D%\local-packages"
  xcopy "%HERE%local-packages\." "%EMACS_D%\local-packages\" /E /I /H /Y >nul
)

:: Install init.el and optional early-init.el.
copy "%HERE%init.el" "%EMACS_D%\init.el" >nul
if exist "%HERE%early-init.el" copy "%HERE%early-init.el" "%EMACS_D%\early-init.el" >nul
if exist "%HERE%abbrev_defs" copy "%HERE%abbrev_defs" "%EMACS_D%\abbrev_defs" >nul

:: Install starter snippets (not auto-loaded).
if exist "%HERE%init-starter.el" (
  copy "%HERE%init-starter.el" "%EMACS_D%\init-starter.el" >nul
  echo ^>^> Starter snippet installed at %EMACS_D%\init-starter.el (not auto-loaded^)
)
if exist "%HERE%init-starter-coding.el" (
  copy "%HERE%init-starter-coding.el" "%EMACS_D%\init-starter-coding.el" >nul
  echo ^>^> Coding companion installed at %EMACS_D%\init-starter-coding.el (not auto-loaded^)
)

:: Install tools/ drop-zone (language servers, debug adapters) to ~/.emacs.d/bin/.
if exist "%HERE%tools" (
  if exist "%EMACS_D%\bin" (
    echo    backing up bin -^> .backups\%STAMP%\bin
    move "%EMACS_D%\bin" "%BACKUP_DIR%\bin" >nul
  )
  echo ^>^> Installing tools to %EMACS_D%\bin
  if not exist "%EMACS_D%\bin" mkdir "%EMACS_D%\bin"
  xcopy "%HERE%tools\." "%EMACS_D%\bin\" /E /I /H /Y >nul
)

:: Install docs/ drop-zone (coding guide, reference material).
if exist "%HERE%docs" (
  if exist "%EMACS_D%\docs" (
    echo    backing up docs -^> .backups\%STAMP%\docs
    move "%EMACS_D%\docs" "%BACKUP_DIR%\docs" >nul
  )
  echo ^>^> Installing docs to %EMACS_D%\docs
  if not exist "%EMACS_D%\docs" mkdir "%EMACS_D%\docs"
  xcopy "%HERE%docs\." "%EMACS_D%\docs\" /E /I /H /Y >nul
)

:: Report on optional Emacs source.
dir "%HERE%emacs-*.tar.xz" 2>nul >nul
if !errorlevel! equ 0 (
  echo.
  echo ^>^> Bundled Emacs source found.
  echo    Extract and build using MSYS2 or Cygwin.
)

:: Summarise backups.
dir "%BACKUP_DIR%" /b 2>nul | findstr /r . >nul
if !errorlevel! equ 0 (
  echo ^>^> Previous state backed up to %EMACS_D%\.backups\%STAMP%\
)

echo.
echo Done. Launch Emacs; *Messages* should report:
echo   my/offline-packages=t  dir=%MY_HOME%\elpa-mirror-emacs-...
echo.
echo Note: If tar failed above with "Cannot connect to" a drive letter,
echo a stray GNU tar was used. Extract the mirror with Windows' bsdtar:
echo   "%SystemRoot%\System32\tar.exe" -xzf "%HERE%elpa-mirror-*.tar.gz" -C "%MY_HOME%"
SETUPBAT

# --- Installer: rollback.sh ---
# Reverses a setup.sh install by swapping the contents of a
# ~/.emacs.d/.backups/<STAMP>/ directory back into place. Supports both the
# new directory-based layout and the legacy flat .pre-toolkit-<STAMP> suffixes
# (so a rollback script from a newer toolkit can still undo older installs).
cat > "${STAGING}/rollback.sh" <<'ROLLBACKEOF'
#!/usr/bin/env bash
# rollback.sh — restore a setup.sh backup. Works with both:
#   (1) Directory-based backups under ~/.emacs.d/.backups/<STAMP>/
#   (2) Legacy flat .pre-toolkit-<STAMP> suffixes (from older setup.sh)
#
# Scans for both formats, picks the newest stamp, and swaps everything under
# that stamp back to its original name. The current (post-install) state is
# archived under .backups/<NOW>-rolledback/ so a re-rollforward is possible.
set -euo pipefail

EMACS_D="${HOME}/.emacs.d"
BACKUPS_DIR="${EMACS_D}/.backups"
NOW="$(date +%Y%m%dT%H%M%S)"

# Collect stamps from directory-based backups (.backups/<STAMP>/ dirs).
dir_stamps() {
  find "$BACKUPS_DIR" -maxdepth 1 -mindepth 1 -type d 2>/dev/null \
    | xargs -r -I{} basename '{}' \
    | grep -E '^[0-9]{8}T[0-9]{6}$' || true
}

# Collect stamps from legacy flat .pre-toolkit-<STAMP> suffixes.
legacy_stamps() {
  {
    find "$EMACS_D" -maxdepth 1 -mindepth 1 -name '*.pre-toolkit-*' 2>/dev/null
    find "$HOME"    -maxdepth 1 -mindepth 1 -name '*.pre-toolkit-*' 2>/dev/null
  } | sed -E 's/.*\.pre-toolkit-([0-9TZ]+).*/\1/' | sort -u || true
}

mapfile -t DIR_STAMPS < <(dir_stamps)
mapfile -t LEGACY_STAMPS < <(legacy_stamps)

ALL_STAMPS=()
for s in "${DIR_STAMPS[@]+"${DIR_STAMPS[@]}"}" "${LEGACY_STAMPS[@]+"${LEGACY_STAMPS[@]}"}"; do
  [[ -n "$s" ]] && ALL_STAMPS+=("$s")
done

if [[ "${#ALL_STAMPS[@]}" -eq 0 ]]; then
  echo "No backups found (neither .backups/ dirs nor .pre-toolkit-* entries)." >&2
  echo "Checked: ${BACKUPS_DIR}/, ${EMACS_D}/*.pre-toolkit-*, ~/elpa-mirror-emacs-*.pre-toolkit-*" >&2
  exit 1
fi

# Deduplicate and sort newest first.
mapfile -t SORTED < <(printf '%s\n' "${ALL_STAMPS[@]}" | sort -u -r)
LATEST="${SORTED[0]}"

echo "Available backup stamps (newest first):"
printf '  %s%s\n' "${SORTED[0]}" "  <- default" "${SORTED[@]:1}"
echo
read -r -p "Roll back to ${LATEST}? [y/N] " ans </dev/tty
[[ "$ans" =~ ^[yY] ]] || { echo "Aborted."; exit 0; }

# Archive current state so we can roll forward again if needed.
ROLLFORWARD_DIR="${BACKUPS_DIR}/${NOW}-rolledback"
mkdir -p "$ROLLFORWARD_DIR"

rollback_dir() {
  local stamp="$1"
  local bak_dir="${BACKUPS_DIR}/${stamp}"
  [[ -d "$bak_dir" ]] || return 0

  echo ">> Restoring from directory backup: ${bak_dir}"

  # Restore items that live directly under ~/.emacs.d/
  for item in init.el early-init.el abbrev_defs init-starter.el init-starter-coding.el \
              elpa local-packages bin docs Emacs-vanilla Emacs-DIYer; do
    local src="${bak_dir}/${item}"
    [[ -e "$src" ]] || continue
    local dest="${EMACS_D}/${item}"
    if [[ -e "$dest" ]]; then
      echo "   archiving current ${item} -> .backups/${NOW}-rolledback/${item}"
      mv "$dest" "${ROLLFORWARD_DIR}/${item}"
    fi
    echo "   restoring ${item}"
    mv "$src" "$dest"
  done

  # Restore mirror extractions from the home-mirrors/ subdirectory.
  local mirror_dir="${bak_dir}/home-mirrors"
  if [[ -d "$mirror_dir" ]]; then
    for mirror in "$mirror_dir"/elpa-mirror-emacs-*; do
      [[ -e "$mirror" ]] || continue
      local mirror_name="$(basename "$mirror")"
      local dest="${HOME}/${mirror_name}"
      if [[ -e "$dest" ]]; then
        echo "   archiving current ${mirror_name} -> .backups/${NOW}-rolledback/home-mirrors/${mirror_name}"
        mkdir -p "${ROLLFORWARD_DIR}/home-mirrors"
        mv "$dest" "${ROLLFORWARD_DIR}/home-mirrors/${mirror_name}"
      fi
      echo "   restoring ${mirror_name}"
      mv "$mirror" "$dest"
    done
  fi

  # Remove the now-empty backup directory.
  rmdir "$bak_dir" 2>/dev/null || true
}

rollback_legacy() {
  local stamp="$1"

  rollback_in() {
    local dir="$1"
    shopt -s nullglob
    for bak in "$dir"/*.pre-toolkit-"$stamp"; do
      [[ -e "$bak" ]] || continue
      local orig="${bak%.pre-toolkit-$stamp}"
      if [[ -e "$orig" ]]; then
        echo "   archiving current $(basename "$orig") -> $(basename "$orig").rolled-back-${NOW}"
        mv "$orig" "${orig}.rolled-back-${NOW}"
      fi
      echo "   restoring $(basename "$bak") -> $(basename "$orig")"
      mv "$bak" "$orig"
    done
  }

  echo ">> Restoring from legacy flat backup (stamp: ${stamp})"
  rollback_in "$EMACS_D"
  rollback_in "$HOME"
}

# Try directory-based first; fall back to legacy flat if no .backups/ dir.
if [[ -d "${BACKUPS_DIR}/${LATEST}" ]]; then
  rollback_dir "$LATEST"
else
  rollback_legacy "$LATEST"
fi

echo
echo "Rollback complete. Post-install state archived under ${ROLLFORWARD_DIR}."
echo "To roll forward again: restore items from that directory."
ROLLBACKEOF
chmod +x "${STAGING}/rollback.sh"

# --- Windows rollback: rollback.bat ---
# Reverses a setup.bat install by swapping a .backups/<STAMP>/ directory back.
cat > "${STAGING}/rollback.bat" <<'ROLLBACKBAT'
@echo off
setlocal enabledelayedexpansion

:: Accept either EMACS_D path (ending with .emacs.d) or HOME path.
:: Must match the same resolution as setup.bat for correct rollback.
if not "%1"=="" (
  set "TARGET=%1"
  :: Strip trailing separator if present
  if "!TARGET:~-1!"=="\" set "TARGET=!TARGET:~0,-1!"
  if "!TARGET:~-1!"=="/" set "TARGET=!TARGET:~0,-1!"
  :: Check if it's an .emacs.d path (last 8 chars == ".emacs.d")
  if /i "!TARGET:~-8!"==".emacs.d" (
    set EMACS_D=!TARGET!
    set MY_HOME=!TARGET:~0,-8!
    :: Strip any separator left at the end of MY_HOME
    if "!MY_HOME:~-1!"=="\" set MY_HOME=!MY_HOME:~0,-1!
    if "!MY_HOME:~-1!"=="/" set MY_HOME=!MY_HOME:~0,-1!
  ) else (
    set MY_HOME=!TARGET!
    set EMACS_D=!TARGET!\.emacs.d
  )
) else (
  rem Use %HOME% only if it is an absolute Windows path (drive-letter form,
  rem e.g. C-colon-backslash). Under MSYS/Git-bash HOME is often "." or
  rem "/home/user", which would produce invalid relative targets.
  set "MY_HOME="
  if not "%HOME%"=="" (
    set "H=%HOME%"
    if "!H:~1,1!"==":" set "MY_HOME=%HOME%"
  )
  if "!MY_HOME!"=="" set "MY_HOME=%USERPROFILE%"
  set "EMACS_D=!MY_HOME!\.emacs.d"
)
set BACKUPS_DIR=%EMACS_D%\.backups
for /f %%I in ('powershell -Command "Get-Date -Format 'yyyyMMddTHHmmss'"') do set NOW=%%I

:: Find newest backup stamp from directory-based backups.
set LATEST=
for /f %%s in ('dir "%BACKUPS_DIR%" /b /o-d 2^>nul ^| findstr /r "^[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]T[0-9][0-9][0-9][0-9][0-9][0-9]$"') do (
  if not defined LATEST set LATEST=%%s
)

if not defined LATEST (
  echo No backups found under %BACKUPS_DIR%\
  goto :eof
)

echo Available backup stamps (newest first^):
for /f %%s in ('dir "%BACKUPS_DIR%" /b /o-d 2^>nul ^| findstr /r "^[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]T[0-9][0-9][0-9][0-9][0-9][0-9]$"') do (
  if defined LATEST (
    if "%%s"=="!LATEST!" (
      echo   %%s ^<- default
    ) else (
      echo   %%s
    )
  )
)
echo.
set /p ans="Roll back to !LATEST!? [y/N] "
if /i not "!ans!"=="y" (
  echo Aborted.
  goto :eof
)

:: Archive current state so re-rollforward is possible.
set ROLLFORWARD_DIR=%BACKUPS_DIR%\!LATEST!-rolledback
mkdir "!ROLLFORWARD_DIR!" 2>nul

:: Restore from directory backup.
set bak_dir=%BACKUPS_DIR%\!LATEST!
if exist "!bak_dir!" (
  echo ^>^> Restoring from directory backup: !LATEST!

  for %%i in (init.el early-init.el abbrev_defs init-starter.el init-starter-coding.el elpa local-packages bin docs Emacs-vanilla Emacs-DIYer) do (
    if exist "!bak_dir!\%%i" (
      if exist "%EMACS_D%\%%i" (
        echo    archiving current %%i -^> !NOW!-rolledback\%%i
        move "%EMACS_D%\%%i" "!ROLLFORWARD_DIR!\%%i" >nul
      )
      echo    restoring %%i
      move "!bak_dir!\%%i" "%EMACS_D%\%%i" >nul
    )
  )

  :: Restore mirror extractions from the home-mirrors/ subdirectory.
  if exist "!bak_dir!\home-mirrors" (
    for /d %%d in ("!bak_dir!\home-mirrors\elpa-mirror-emacs-*") do (
      if exist "%%d" (
        set mirror_name=%%~nxd
        if exist "%MY_HOME%\!mirror_name!" (
          if not exist "!ROLLFORWARD_DIR!\home-mirrors" mkdir "!ROLLFORWARD_DIR!\home-mirrors"
          echo    archiving current !mirror_name! -^> !NOW!-rolledback\home-mirrors\!mirror_name!
          move "%MY_HOME%\!mirror_name!" "!ROLLFORWARD_DIR!\home-mirrors\!mirror_name!" >nul
        )
        echo    restoring !mirror_name!
        move "%%d" "%MY_HOME%\!mirror_name!" >nul
      )
    )
  )

  rmdir "!bak_dir!" 2>nul
)

echo.
echo Rollback complete. Post-install state archived under !ROLLFORWARD_DIR!.
ROLLBACKBAT

# --- Smoke test: boot the rendered init.el under the target Emacs ---
# Before compressing, launch emacs-<VER> in batch mode against the staged
# init.el with HOME set to a throwaway dir containing the extracted mirror.
# Catches: bad template renders, packages listed but missing from the mirror,
# syntax errors in starters, etc. Pass --smoke-test to enable.
if [[ "$SMOKE_TEST" -eq 1 ]]; then
  # Prefer the same per-version binary create-install.sh uses.
  SMOKE_EMACS=""
  for cand in "${HOME}/emacs-versions/emacs-${EMACS_VERSION}/bin/emacs" \
              "${HOME}/bin/emacs-${EMACS_VERSION}"; do
    [[ -x "$cand" ]] && { SMOKE_EMACS="$cand"; break; }
  done
  [[ -z "$SMOKE_EMACS" ]] && SMOKE_EMACS="$(command -v "emacs-${EMACS_VERSION}" 2>/dev/null || true)"

  if [[ -z "$SMOKE_EMACS" ]]; then
    echo "!! Smoke test skipped: no emacs-${EMACS_VERSION} binary on this machine." >&2
  else
    SMOKE_HOME="$(mktemp -d "${TMPDIR:-/tmp}/toolkit-smoke-${EMACS_VERSION}-XXXXXX")"
    _cleanup_smoke() { rm -rf "$SMOKE_HOME"; }
    echo ">> Smoke test: booting ${SMOKE_EMACS} against staged init.el..."
    # Extract mirror so init.el's `my/offline-packages-dir' glob finds it.
    tar -xzf "${STAGING}"/elpa-mirror-*.tar.gz -C "$SMOKE_HOME"
    # Give the smoke-test HOME a minimal .emacs.d so init.el's `load-file' of
    # Emacs-vanilla (if missing) doesn't matter — it's guarded by file-exists-p.
    mkdir -p "${SMOKE_HOME}/.emacs.d"
    if HOME="$SMOKE_HOME" "$SMOKE_EMACS" --batch \
         --eval '(setq byte-compile-warnings nil)' \
         --eval '(setq warning-minimum-log-level :error)' \
         -l "${STAGING}/init.el" \
         --eval '(kill-emacs 0)' 2>&1 | sed 's/^/     /'; then
      echo ">> Smoke test passed."
    else
      _cleanup_smoke
      echo "!! Smoke test FAILED — init.el does not load cleanly." >&2
      echo "   Re-run with --smoke-test to test once the issue is fixed." >&2
      exit 1
    fi
    _cleanup_smoke
  fi
fi

# --- MANIFEST.txt ---
{
  echo "Built:         $(date -Iseconds)"
  echo "Timestamp:     ${STAMP}"
  echo "Host:          $(hostname)"
  echo "User:          $(id -un)"
  echo "Kernel:        $(uname -srm)"
  echo "Target:        ${TARGET}"
  echo "Emacs ver:     ${EMACS_VERSION}"
  echo "OS slug:       ${OS_SLUG}"
  echo "Architecture:  ${ARCH}"
  echo "Mirror:        $(basename "$MIRROR_TARBALL") ($(du -h "$MIRROR_TARBALL" | cut -f1))"
  if [[ -f "${STAGING}/PACKAGES.txt" ]]; then
    echo "Packages:      $(wc -l < "${STAGING}/PACKAGES.txt" | tr -d ' ') (see PACKAGES.txt)"
  fi
  if [[ -f "${STAGING}/init-starter.el" ]]; then
    echo "Starter:       init-starter.el (optional, not auto-loaded)"
  fi
  if [[ -f "${STAGING}/init-starter-coding.el" ]]; then
    echo "Coding starter: init-starter-coding.el (optional, not auto-loaded)"
  fi
  if [[ -d "${STAGING}/local-packages" ]]; then
    _lp_count="$(find "${STAGING}/local-packages" -maxdepth 1 -mindepth 1 \
                  \( -type d -o -name '*.el' \) | wc -l)"
    echo "Local pkgs:    ${_lp_count} entries under local-packages/"
  fi
  if [[ -d "${STAGING}/tools" ]]; then
    _t_count="$(find "${STAGING}/tools" -maxdepth 1 -mindepth 1 | wc -l)"
    _t_size="$(du -sh "${STAGING}/tools" | cut -f1)"
    echo "Tools:         ${_t_count} entries under tools/ (${_t_size})"
  else
    echo "Tools:         (skipped — pass --tools to include)"
  fi
  if [[ -d "${STAGING}/docs" ]]; then
    _d_count="$(find "${STAGING}/docs" -maxdepth 1 -mindepth 1 | wc -l)"
    echo "Docs:          ${_d_count} entries under docs/"
  fi
  echo "Windows:       setup.bat + rollback.bat included"
  if [[ "$WITH_SOURCE" -eq 1 ]]; then
    echo "Emacs src:     emacs-${EMACS_SOURCE_VERSION}.tar.xz"
  elif [[ -n "$SOURCE_SKIP_REASON" ]]; then
    echo "Emacs src:     (skipped — ${SOURCE_SKIP_REASON})"
  fi
  echo
  echo "Contents:"
  (cd "$STAGING" && find . -mindepth 1 -maxdepth 2 -printf '  %p  %s bytes\n' 2>/dev/null \
     | sed 's|\./||' | sort)
} > "${STAGING}/MANIFEST.txt"

# --- Package: gzip (default) or xz -9e (--xz) ---
FINAL="${OUT_DIR}/${TOOLKIT_NAME}"
mv "$STAGING" "$FINAL"
trap 'rm -rf "$FINAL"' EXIT

if [[ "$USE_XZ" -eq 1 ]]; then
  OUT_TAR="${OUT_DIR}/${TOOLKIT_NAME}.tar.xz"
  TAR_CAT_FLAG=-xJOf
  TAR_LIST_FLAG=-xJf
  echo ">> Compressing to ${OUT_TAR} (xz -9e)..."
  XZ_OPT='-9e -T1' tar -C "$OUT_DIR" --sort=name -cJf "$OUT_TAR" "$TOOLKIT_NAME"
else
  OUT_TAR="${OUT_DIR}/${TOOLKIT_NAME}.tar.gz"
  TAR_CAT_FLAG=-xzOf
  TAR_LIST_FLAG=-xzf
  echo ">> Compressing to ${OUT_TAR} (gzip)..."
  tar -C "$OUT_DIR" --sort=name -czf "$OUT_TAR" "$TOOLKIT_NAME"
fi

rm -rf "$FINAL"
trap - EXIT

SIZE="$(du -h "$OUT_TAR" | cut -f1)"
cat <<EOF

Built: ${OUT_TAR}  (${SIZE})

--- MANIFEST.txt ---
$(tar "$TAR_CAT_FLAG" "$OUT_TAR" "${TOOLKIT_NAME}/MANIFEST.txt")

On the offline machine (Linux):
  tar ${TAR_LIST_FLAG} $(basename "$OUT_TAR")
  cd ${TOOLKIT_NAME}
  ./setup.sh

On Windows (extract with 7-Zip or tar):
  cd ${TOOLKIT_NAME}
  setup.bat
EOF
