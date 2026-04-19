#!/usr/bin/env bash
# Build a deployable offline Emacs toolkit tarball.
# Run on the ONLINE build machine.
#
# Picks one of the per-version ELPA mirrors under mirrors/emacs-<VER>/ (built
# by create-install.sh), bundles it alongside the rendered init.el, the
# Emacs-vanilla and Emacs-DIYer literate configs, an early-init.el if present,
# and an installer (setup.sh). Optional GNU Emacs source tarball for rebuild
# on the target.
#
# Output: $OUT_DIR/emacs-offline-toolkit-<ver>-<os>-<arch>-<stamp>.tar.xz
#
# Usage: ./build-toolkit.sh [options]
#   -t, --target SPEC       Target "emacs-VER" (skip interactive pick)
#   -o, --out-dir DIR       Output directory (default: $HOME)
#   -s, --with-source VER   Bundle GNU Emacs source tarball for VER.
#                           Use "auto" to read sources/LATEST_STABLE.
#                           Pass --with-source none to skip.
#       --local-configs     Copy Emacs-vanilla/DIYer from ~/.emacs.d/<d>/
#                           instead of pulling fresh from GitHub.
#       --gzip              Use gzip (.tar.gz) instead of xz -9e (.tar.xz).
#                           Faster compress/decompress, less RAM, larger output.
#   -l, --list              List available targets and exit
#   -h, --help              This help

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
EMACS_D_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
MIRRORS_DIR="${SCRIPT_DIR}/mirrors"
OUT_DIR="${HOME}"
WITH_SOURCE=0
EMACS_SOURCE_VERSION=""
TARGET=""
LOCAL_CONFIGS=0
USE_GZIP=0

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
TOOLKIT_NAME="emacs-offline-toolkit-${EMACS_VERSION}-${OS_SLUG}-${ARCH}-${STAMP}"
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

# Install init.el and (optional) early-init.el.
cp "${HERE}/init.el" "${EMACS_D}/init.el"
[[ -f "${HERE}/early-init.el" ]] && cp "${HERE}/early-init.el" "${EMACS_D}/early-init.el"

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
