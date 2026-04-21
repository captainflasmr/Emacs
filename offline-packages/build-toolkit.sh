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
# Output: $OUT_DIR/emacs-offline-toolkit-<ver>-<os>-<arch>-<stamp>.tar.xz
#
# Usage: ./build-toolkit.sh [options]
#   -t, --target SPEC       Target "emacs-VER" (skip interactive pick)
#   -o, --out-dir DIR       Output directory (default: $HOME/.emacs.d)
#   -s, --with-source VER   Bundle GNU Emacs source tarball for VER.
#                           Use "auto" to read sources/LATEST_STABLE.
#                           Pass --with-source none to skip.
#       --local-configs     Copy Emacs-vanilla/DIYer from ~/.emacs.d/<d>/
#                           instead of pulling fresh from GitHub.
#       --gzip              Use gzip (.tar.gz) instead of xz -9e (.tar.xz).
#                           Faster compress/decompress, less RAM, larger output.
#       --no-tools          Skip the tools/ drop-zone (language servers, debug
#                           adapters). Produces a much smaller "update" tarball
#                           after the initial big install; target keeps whatever
#                           it already has under ~/.emacs.d/bin. Filename gets
#                           a "-notools" suffix.
#       --no-smoke-test     Skip the post-staging "boot rendered init.el" check.
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
USE_GZIP=0
SMOKE_TEST=1
INCLUDE_TOOLS=1

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
    --gzip)           USE_GZIP=1; shift ;;
    --no-tools)       INCLUDE_TOOLS=0; shift ;;
    --no-smoke-test)  SMOKE_TEST=0; shift ;;
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
_NOTOOLS_SUFFIX=""
[[ "$INCLUDE_TOOLS" -eq 0 ]] && _NOTOOLS_SUFFIX="-notools"
TOOLKIT_NAME="emacs-offline-toolkit-${EMACS_VERSION}-${OS_SLUG}-${ARCH}-${STAMP}${_NOTOOLS_SUFFIX}"
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
# Installed on the target as ~/.emacs.d/coding-starter.el, not auto-loaded.
# Opt in by adding to init.el (on top of, or instead of, init-starter.el):
#   (load (expand-file-name "coding-starter" user-emacs-directory) t t)
CODING_SRC="${SCRIPT_DIR}/starters/coding.el"
if [[ -f "$CODING_SRC" ]]; then
  echo ">> Copying coding companion starter from $(basename "$CODING_SRC")..."
  cp "$CODING_SRC" "${STAGING}/coding-starter.el"
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
if [[ "$INCLUDE_TOOLS" -eq 0 ]]; then
  echo ">> --no-tools: skipping tools/ drop-zone (update-only tarball)."
elif [[ -d "$TOOLS_SRC" ]] \
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
cat > "${STAGING}/setup.sh" <<'SETUPEOF'
#!/usr/bin/env bash
# setup.sh — install the bundled Emacs toolkit on an offline machine.
# Run from inside the extracted toolkit directory.
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
EMACS_D="${HOME}/.emacs.d"
BACKUP_SUFFIX=".pre-toolkit-$(date +%Y%m%dT%H%M%S)"

echo ">> Target: ${EMACS_D}"
mkdir -p "$EMACS_D"

# Back up existing init files so we don't silently clobber an existing setup.
for f in init.el early-init.el; do
  if [[ -e "${EMACS_D}/${f}" && ! -L "${EMACS_D}/${f}" ]]; then
    echo "   backing up existing ${f} -> ${f}${BACKUP_SUFFIX}"
    mv "${EMACS_D}/${f}" "${EMACS_D}/${f}${BACKUP_SUFFIX}"
  fi
done

# Move any existing ~/.emacs.d/elpa/ aside so `my/ensure-package' sees every
# package as not-installed on next launch and reinstalls from the freshly
# extracted mirror. Without this, stale .elc/.eln from the old mirror stay put.
if [[ -d "${EMACS_D}/elpa" ]]; then
  echo "   backing up existing elpa/ -> elpa${BACKUP_SUFFIX}/"
  mv "${EMACS_D}/elpa" "${EMACS_D}/elpa${BACKUP_SUFFIX}"
fi

# Move any pre-existing mirror extractions aside too — same reason, and avoids
# the init.el's `string>' sort silently picking up stale mirrors after upgrades.
for old in "${HOME}"/elpa-mirror-emacs-*; do
  [[ -d "$old" ]] || continue
  echo "   backing up existing $(basename "$old")/ -> $(basename "$old")${BACKUP_SUFFIX}/"
  mv "$old" "${old}${BACKUP_SUFFIX}"
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
      echo "   backing up existing ${d}/ -> ${d}${BACKUP_SUFFIX}/"
      mv "${EMACS_D}/${d}" "${EMACS_D}/${d}${BACKUP_SUFFIX}"
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
    echo "   backing up existing local-packages/ -> local-packages${BACKUP_SUFFIX}/"
    mv "${EMACS_D}/local-packages" "${EMACS_D}/local-packages${BACKUP_SUFFIX}"
  fi
  echo ">> Installing local-packages to ${EMACS_D}/local-packages"
  mkdir -p "${EMACS_D}/local-packages"
  cp -a "${HERE}/local-packages/." "${EMACS_D}/local-packages/"
fi

# Install init.el and (optional) early-init.el.
cp "${HERE}/init.el" "${EMACS_D}/init.el"
[[ -f "${HERE}/early-init.el" ]] && cp "${HERE}/early-init.el" "${EMACS_D}/early-init.el"

# Install starter snippet if bundled. Does NOT auto-load — opt in from init.el
# with: (load (expand-file-name "init-starter" user-emacs-directory) t t)
if [[ -f "${HERE}/init-starter.el" ]]; then
  if [[ -e "${EMACS_D}/init-starter.el" && ! -L "${EMACS_D}/init-starter.el" ]]; then
    echo "   backing up existing init-starter.el -> init-starter.el${BACKUP_SUFFIX}"
    mv "${EMACS_D}/init-starter.el" "${EMACS_D}/init-starter.el${BACKUP_SUFFIX}"
  fi
  cp "${HERE}/init-starter.el" "${EMACS_D}/init-starter.el"
  echo ">> Starter snippet installed at ${EMACS_D}/init-starter.el (not auto-loaded)"
fi

# Install coding companion starter if bundled. Also NOT auto-loaded — opt in
# with: (load (expand-file-name "coding-starter" user-emacs-directory) t t)
if [[ -f "${HERE}/coding-starter.el" ]]; then
  if [[ -e "${EMACS_D}/coding-starter.el" && ! -L "${EMACS_D}/coding-starter.el" ]]; then
    echo "   backing up existing coding-starter.el -> coding-starter.el${BACKUP_SUFFIX}"
    mv "${EMACS_D}/coding-starter.el" "${EMACS_D}/coding-starter.el${BACKUP_SUFFIX}"
  fi
  cp "${HERE}/coding-starter.el" "${EMACS_D}/coding-starter.el"
  echo ">> Coding companion installed at ${EMACS_D}/coding-starter.el (not auto-loaded)"
fi

# Install tools/ drop-zone (language servers, debug adapters) to ~/.emacs.d/bin/.
# `cp -a' keeps the executable bits. Users can either add ~/.emacs.d/bin to PATH
# or reference binaries by absolute path from their eglot-server-programs.
if [[ -d "${HERE}/tools" ]]; then
  if [[ -d "${EMACS_D}/bin" ]]; then
    echo "   backing up existing bin/ -> bin${BACKUP_SUFFIX}/"
    mv "${EMACS_D}/bin" "${EMACS_D}/bin${BACKUP_SUFFIX}"
  fi
  echo ">> Installing tools/ to ${EMACS_D}/bin"
  mkdir -p "${EMACS_D}/bin"
  cp -a "${HERE}/tools/." "${EMACS_D}/bin/"
fi

# Install docs/ drop-zone (coding guide, reference material).
if [[ -d "${HERE}/docs" ]]; then
  if [[ -d "${EMACS_D}/docs" ]]; then
    echo "   backing up existing docs/ -> docs${BACKUP_SUFFIX}/"
    mv "${EMACS_D}/docs" "${EMACS_D}/docs${BACKUP_SUFFIX}"
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

echo
echo "Done. Launch Emacs; *Messages* should report:"
echo "  my/offline-packages=t  dir=/home/you/elpa-mirror-emacs-..."
SETUPEOF
chmod +x "${STAGING}/setup.sh"

# --- Installer: rollback.sh ---
# Reverses the most recent setup.sh install by swapping `.pre-toolkit-<STAMP>'
# entries back into place. Run on the offline target from ~/.emacs.d/ (or from
# anywhere — it operates on $HOME and $HOME/.emacs.d directly).
cat > "${STAGING}/rollback.sh" <<'ROLLBACKEOF'
#!/usr/bin/env bash
# rollback.sh — restore the most recent `.pre-toolkit-<STAMP>' backup created
# by setup.sh. Scans ~/.emacs.d/ and ~/ for backup entries, picks the newest
# stamp, and swaps everything under that stamp back to its original name. The
# current (post-install) state is not discarded — it's archived with a
# `.rolled-back-<STAMP>' suffix so a re-rollforward is still possible.
set -euo pipefail

EMACS_D="${HOME}/.emacs.d"
NOW="$(date +%Y%m%dT%H%M%S)"

collect_stamps() {
  {
    find "$EMACS_D" -maxdepth 1 -mindepth 1 -name '*.pre-toolkit-*' 2>/dev/null
    find "$HOME"    -maxdepth 1 -mindepth 1 -name '*.pre-toolkit-*' 2>/dev/null
  } | sed -E 's/.*\.pre-toolkit-([0-9TZ]+).*/\1/' | sort -u
}

mapfile -t STAMPS < <(collect_stamps)
if [[ "${#STAMPS[@]}" -eq 0 ]]; then
  echo "No .pre-toolkit-* backups found under $HOME or $EMACS_D." >&2
  exit 1
fi

# Newest first — lexical sort works because stamps are YYYYmmddTHHMMSS.
LATEST="$(printf '%s\n' "${STAMPS[@]}" | sort -r | head -n1)"

echo "Backup stamps available (newest first):"
printf '%s\n' "${STAMPS[@]}" | sort -r | sed 's/^/  /'
echo
read -r -p "Roll back to ${LATEST}? [y/N] " ans </dev/tty
[[ "$ans" =~ ^[yY] ]] || { echo "Aborted."; exit 0; }

rollback_in() {
  local dir="$1"
  shopt -s nullglob
  for bak in "$dir"/*.pre-toolkit-"$LATEST"; do
    [[ -e "$bak" ]] || continue
    local orig="${bak%.pre-toolkit-$LATEST}"
    if [[ -e "$orig" ]]; then
      echo "   archiving current $(basename "$orig") -> $(basename "$orig").rolled-back-${NOW}"
      mv "$orig" "${orig}.rolled-back-${NOW}"
    fi
    echo "   restoring $(basename "$bak") -> $(basename "$orig")"
    mv "$bak" "$orig"
  done
}

rollback_in "$EMACS_D"
rollback_in "$HOME"

echo
echo "Rollback complete. Post-install state preserved under *.rolled-back-${NOW}."
ROLLBACKEOF
chmod +x "${STAGING}/rollback.sh"

# --- Smoke test: boot the rendered init.el under the target Emacs ---
# Before compressing, launch emacs-<VER> in batch mode against the staged
# init.el with HOME set to a throwaway dir containing the extracted mirror.
# Catches: bad template renders, packages listed but missing from the mirror,
# syntax errors in starters, etc. Skipped with --no-smoke-test.
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
      echo "   Re-run with --no-smoke-test to bypass, or fix the init/package list." >&2
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
  if [[ -f "${STAGING}/coding-starter.el" ]]; then
    echo "Coding starter: coding-starter.el (optional, not auto-loaded)"
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
  elif [[ "$INCLUDE_TOOLS" -eq 0 ]]; then
    echo "Tools:         (skipped — --no-tools; update-only tarball)"
  fi
  if [[ -d "${STAGING}/docs" ]]; then
    _d_count="$(find "${STAGING}/docs" -maxdepth 1 -mindepth 1 | wc -l)"
    echo "Docs:          ${_d_count} entries under docs/"
  fi
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

# --- Package: tar + xz -9e (default) or gzip (--gzip) ---
FINAL="${OUT_DIR}/${TOOLKIT_NAME}"
mv "$STAGING" "$FINAL"
trap 'rm -rf "$FINAL"' EXIT

if [[ "$USE_GZIP" -eq 1 ]]; then
  OUT_TAR="${OUT_DIR}/${TOOLKIT_NAME}.tar.gz"
  TAR_CAT_FLAG=-xzOf
  TAR_LIST_FLAG=-xzf
  echo ">> Compressing to ${OUT_TAR} (gzip)..."
  tar -C "$OUT_DIR" --sort=name -czf "$OUT_TAR" "$TOOLKIT_NAME"
else
  OUT_TAR="${OUT_DIR}/${TOOLKIT_NAME}.tar.xz"
  TAR_CAT_FLAG=-xJOf
  TAR_LIST_FLAG=-xJf
  echo ">> Compressing to ${OUT_TAR} (xz -9e)..."
  XZ_OPT='-9e -T1' tar -C "$OUT_DIR" --sort=name -cJf "$OUT_TAR" "$TOOLKIT_NAME"
fi

rm -rf "$FINAL"
trap - EXIT

SIZE="$(du -h "$OUT_TAR" | cut -f1)"
cat <<EOF

Built: ${OUT_TAR}  (${SIZE})

--- MANIFEST.txt ---
$(tar "$TAR_CAT_FLAG" "$OUT_TAR" "${TOOLKIT_NAME}/MANIFEST.txt")

On the offline machine:
  tar ${TAR_LIST_FLAG} $(basename "$OUT_TAR")
  cd ${TOOLKIT_NAME}
  ./setup.sh
EOF
