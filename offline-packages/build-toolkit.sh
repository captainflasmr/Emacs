#!/usr/bin/env bash
# Build a deployable offline Emacs toolkit tarball.
# Run on the ONLINE build machine.
#
# Picks one of the per-target ELPA mirrors under offline-packages/, bundles
# it alongside the shared init.el, the Emacs-vanilla and Emacs-DIYer literate
# configs, an early-init.el if present, and an installer (setup.sh). Optional
# GNU Emacs source tarball for rebuild on the target.
#
# Output: $OUT_DIR/emacs-offline-toolkit-<ver>-<os>-<arch>-<stamp>.tar.xz
#
# Usage: ./build-toolkit.sh [options]
#   -t, --target SPEC       Target "emacs-VER/OS/ARCH" (skip interactive pick)
#   -o, --out-dir DIR       Output directory (default: $HOME)
#   -s, --with-source VER   Bundle GNU Emacs source tarball for VER
#   -l, --list              List available targets and exit
#   -h, --help              This help

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
EMACS_D_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
OUT_DIR="${HOME}"
WITH_SOURCE=0
EMACS_SOURCE_VERSION=""
TARGET=""

list_targets() {
  find "${SCRIPT_DIR}" -mindepth 4 -maxdepth 4 -name 'elpa-mirror-*.tar.gz' 2>/dev/null \
    | while read -r f; do
        dir="$(dirname "$f")"
        echo "${dir#${SCRIPT_DIR}/}"
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
    -l|--list)        list_targets; exit 0 ;;
    -h|--help)        usage; exit 0 ;;
    *) echo "Unknown option: $1" >&2; usage; exit 1 ;;
  esac
done

[[ -z "$TARGET" ]] && TARGET="$(pick_target)"

TARGET_DIR="${SCRIPT_DIR}/${TARGET}"
[[ -d "$TARGET_DIR" ]] || { echo "Target dir not found: $TARGET_DIR" >&2; exit 1; }

# Newest mirror tarball in the target dir wins.
MIRROR_TARBALL="$(ls -t "${TARGET_DIR}"/elpa-mirror-*.tar.gz 2>/dev/null | head -n1)"
[[ -n "$MIRROR_TARBALL" && -f "$MIRROR_TARBALL" ]] \
  || { echo "No elpa-mirror-*.tar.gz in $TARGET_DIR" >&2; exit 1; }

IFS='/' read -r VER_DIR OS_SLUG ARCH <<< "$TARGET"
EMACS_VERSION="${VER_DIR#emacs-}"
STAMP="$(date -u +%Y%m%dT%H%M%SZ)"
TOOLKIT_NAME="emacs-offline-toolkit-${EMACS_VERSION}-${OS_SLUG}-${ARCH}-${STAMP}"
STAGING="${OUT_DIR}/.${TOOLKIT_NAME}-staging-$$"
mkdir -p "$STAGING"
trap 'rm -rf "$STAGING"' EXIT

echo ">> Staging at: $STAGING"

# --- Core files ---
cp "${SCRIPT_DIR}/init.el" "${STAGING}/init.el"
cp "${MIRROR_TARBALL}" "${STAGING}/"
[[ -f "${EMACS_D_DIR}/early-init.el" ]] && cp "${EMACS_D_DIR}/early-init.el" "${STAGING}/"
[[ -f "${SCRIPT_DIR}/README.org" ]] && cp "${SCRIPT_DIR}/README.org" "${STAGING}/"

# --- Literate configs: Emacs-vanilla + Emacs-DIYer (minus VCS/caches) ---
for d in Emacs-vanilla Emacs-DIYer; do
  if [[ -d "${EMACS_D_DIR}/${d}" ]]; then
    echo ">> Copying ${d}..."
    tar -C "${EMACS_D_DIR}" \
        --exclude='.git' --exclude='.gitmodules' --exclude='.gitignore' \
        --exclude='.claude' --exclude='.github' \
        --exclude='*.elc' --exclude='*~' --exclude='.#*' --exclude='#*#' \
        --exclude='eln-cache' --exclude='auto-save-list' \
        -cf - "$d" | tar -C "$STAGING" -xf -
  else
    echo "!! ${d} not found, skipping" >&2
  fi
done

# --- Optional GNU Emacs source tarball ---
if [[ "$WITH_SOURCE" -eq 1 ]]; then
  CACHE_DIR="${SCRIPT_DIR}/.cache"
  mkdir -p "$CACHE_DIR"
  SRC_NAME="emacs-${EMACS_SOURCE_VERSION}.tar.xz"
  SRC_PATH="${CACHE_DIR}/${SRC_NAME}"
  if [[ ! -f "$SRC_PATH" ]]; then
    echo ">> Downloading ${SRC_NAME} to cache..."
    curl -fL --progress-bar \
      "https://ftpmirror.gnu.org/emacs/${SRC_NAME}" -o "$SRC_PATH"
  else
    echo ">> Using cached ${SRC_NAME}"
  fi
  cp "$SRC_PATH" "$STAGING/"
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
  [[ "$WITH_SOURCE" -eq 1 ]] && echo "Emacs src:     emacs-${EMACS_SOURCE_VERSION}.tar.xz"
  echo
  echo "Contents:"
  (cd "$STAGING" && find . -mindepth 1 -maxdepth 2 -printf '  %p  %s bytes\n' 2>/dev/null \
     | sed 's|\./||' | sort)
} > "${STAGING}/MANIFEST.txt"

# --- Package: tar + xz -9e for maximum compression of text content ---
FINAL="${OUT_DIR}/${TOOLKIT_NAME}"
mv "$STAGING" "$FINAL"
trap 'rm -rf "$FINAL"' EXIT

OUT_TAR="${OUT_DIR}/${TOOLKIT_NAME}.tar.xz"
echo ">> Compressing to ${OUT_TAR} (xz -9e)..."
# --sort=name for deterministic ordering; XZ_OPT=-9e for extreme compression.
XZ_OPT='-9e -T1' tar -C "$OUT_DIR" --sort=name -cJf "$OUT_TAR" "$TOOLKIT_NAME"

rm -rf "$FINAL"
trap - EXIT

SIZE="$(du -h "$OUT_TAR" | cut -f1)"
cat <<EOF

Built: ${OUT_TAR}  (${SIZE})

--- MANIFEST.txt ---
$(tar -xJOf "$OUT_TAR" "${TOOLKIT_NAME}/MANIFEST.txt")

On the offline machine:
  tar -xJf $(basename "$OUT_TAR")
  cd ${TOOLKIT_NAME}
  ./setup.sh
EOF
