#!/usr/bin/env bash
# create-install.sh — build an offline ELPA mirror and (by default) chain into
# build-toolkit.sh to produce a deployable toolkit tarball. Wraps
# build-mirror.sh + build-toolkit.sh.
#
# Flow:
#   1. Detect emacs-<VER> binary; if missing, offer to build via
#      build-emacs-versions.sh.
#   2. Read packages/emacs-<VER>.el for the package list.
#   3. Render init.el.in -> $STAGE/.emacs.d/init.el (packages injected).
#   4. Run emacs-<VER> under an isolated HOME to download every package.
#   5. Run build-mirror.sh to tar the resulting ELPA into mirrors/emacs-<VER>/
#      and copy the rendered init.el alongside it.
#   6. Chain into build-toolkit.sh --target emacs-<VER> --with-source auto
#      (unless --mirror-only). Source version comes from sources/LATEST_STABLE.
#
# Usage: ./create-install.sh <VER> [options]
#   <VER>                  Emacs version to target (e.g. 27.2, 30.2)
#       --mirror-only      Stop after the mirror; skip build-toolkit.sh chain
#       --out-dir DIR      Passed through to build-toolkit.sh
#       --gzip             Passed through: .tar.gz instead of .tar.xz (faster)
#   -l, --list             List available packages/emacs-<VER>.el configs
#   -h, --help             This help

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PKG_DIR="${SCRIPT_DIR}/packages"
MIRRORS_DIR="${SCRIPT_DIR}/mirrors"
BUILD_EMACS_SCRIPT="${HOME}/.emacs.d/Emacs-vanilla/scripts/build-emacs-versions.sh"

list_configs() {
  find "${PKG_DIR}" -maxdepth 1 -name 'emacs-*.el' 2>/dev/null \
    | sed -E 's|.*/emacs-(.*)\.el|\1|' | sort
}

usage() {
  sed -n 's/^# \{0,1\}//p' "$0" | awk '/^create-install/{flag=1} flag{print} /^  -h/{exit}'
  echo
  echo "Available package configs:"
  list_configs | sed 's/^/  emacs-/'
}

# --- args ---
VER=""
CHAIN_TOOLKIT=1
TOOLKIT_OUT_DIR=""
USE_GZIP=0
while [[ $# -gt 0 ]]; do
  case "$1" in
    --mirror-only) CHAIN_TOOLKIT=0; shift ;;
    --out-dir)     TOOLKIT_OUT_DIR="$2"; shift 2 ;;
    --gzip)        USE_GZIP=1; shift ;;
    -l|--list)     list_configs; exit 0 ;;
    -h|--help)     usage; exit 0 ;;
    -*) echo "Unknown option: $1" >&2; usage; exit 1 ;;
    *) VER="${1#emacs-}"; shift ;;
  esac
done

if [[ -z "$VER" ]]; then
  echo "Error: specify an Emacs version." >&2
  usage
  exit 1
fi

PKG_FILE="${PKG_DIR}/emacs-${VER}.el"
[[ -f "$PKG_FILE" ]] \
  || { echo "No package config for emacs-${VER}: ${PKG_FILE}" >&2; exit 1; }

# --- locate emacs-<VER> binary ---
find_emacs() {
  local v="$1"
  local candidates=(
    "${HOME}/emacs-versions/emacs-${v}/bin/emacs"
    "${HOME}/bin/emacs-${v}"
  )
  for c in "${candidates[@]}"; do
    [[ -x "$c" ]] && { echo "$c"; return 0; }
  done
  command -v "emacs-${v}" 2>/dev/null || true
}

list_installed_emacsen() {
  # versions with an install dir under $HOME/emacs-versions
  [[ -d "${HOME}/emacs-versions" ]] && \
    find "${HOME}/emacs-versions" -maxdepth 1 -mindepth 1 -type d \
      -printf '%f\n' 2>/dev/null \
    | sed 's/^emacs-//' | sort -u
}

EMACS_BIN="$(find_emacs "$VER")"

if [[ -z "$EMACS_BIN" ]]; then
  echo ">> emacs-${VER} not found on this system." >&2
  echo ">> Installed Emacs versions:" >&2
  installed="$(list_installed_emacsen)"
  if [[ -n "$installed" ]]; then
    printf '   %s\n' $installed >&2
  else
    echo "   (none under \$HOME/emacs-versions)" >&2
  fi
  echo >&2
  read -r -p "Build emacs-${VER} now via ${BUILD_EMACS_SCRIPT}? [y/N] " ans </dev/tty
  case "$ans" in
    y|Y|yes|YES)
      [[ -x "$BUILD_EMACS_SCRIPT" ]] \
        || { echo "Build script not found: $BUILD_EMACS_SCRIPT" >&2; exit 1; }
      echo ">> Invoking build-emacs-versions.sh ${VER} (method=direct)..."
      BUILD_METHOD=1 SOURCES_DIR="${SCRIPT_DIR}/sources" \
        "$BUILD_EMACS_SCRIPT" "$VER"
      EMACS_BIN="$(find_emacs "$VER")"
      [[ -n "$EMACS_BIN" ]] \
        || { echo "Build appeared to finish but emacs-${VER} still not found." >&2; exit 1; }
      ;;
    *)
      echo "Aborting. Install emacs-${VER} manually and rerun." >&2
      exit 1
      ;;
  esac
fi

echo ">> Using Emacs binary: ${EMACS_BIN}"
"$EMACS_BIN" --version | head -n1

# --- parse packages/emacs-<VER>.el ---
# Package list: every non-comment, non-blank line is one symbol.
PACKAGES="$(grep -vE '^\s*(;|$)' "$PKG_FILE" | tr '\n' ' ' | sed 's/[[:space:]]\+$//')"
[[ -n "$PACKAGES" ]] \
  || { echo "No packages found in $PKG_FILE" >&2; exit 1; }
PKG_COUNT_CFG="$(wc -w <<< "$PACKAGES")"
echo ">> Packages (${PKG_COUNT_CFG}): ${PACKAGES}"

# --- render init.el from template ---
TEMPLATE="${SCRIPT_DIR}/init.el.in"
[[ -f "$TEMPLATE" ]] || { echo "Missing template: $TEMPLATE" >&2; exit 1; }

STAMP="$(date -u +%Y%m%dT%H%M%SZ)"
STAGE="$(mktemp -d "${TMPDIR:-/tmp}/emacs-offline-build-${VER}-XXXXXX")"
trap 'rm -rf "$STAGE"' EXIT
mkdir -p "${STAGE}/.emacs.d"

RENDERED="${STAGE}/.emacs.d/init.el"
sed \
    -e "s|@@EMACS_VERSION@@|${VER}|g" \
    -e "s|@@PACKAGES@@|${PACKAGES}|g" \
    "$TEMPLATE" > "$RENDERED"

echo ">> Staging HOME: ${STAGE}"
echo ">> Rendered init.el: ${RENDERED}"

# --- run emacs batch to download packages into the isolated HOME ---
# Silence byte-compile warnings from third-party packages — they're not
# actionable in a mirror-build context and dominate the log otherwise.
echo ">> Downloading packages (this may take a few minutes)..."
HOME="$STAGE" "$EMACS_BIN" --batch \
  --eval '(setq byte-compile-warnings nil)' \
  --eval '(setq warning-minimum-log-level :error)' \
  -l "$RENDERED" 2>&1 \
  | sed 's/^/   /'

# --- run build-mirror.sh against the isolated HOME -> mirrors/emacs-<VER>/ ---
mkdir -p "$MIRRORS_DIR"
echo ">> Invoking build-mirror.sh (OUT_DIR=${MIRRORS_DIR})..."
HOME="$STAGE" EMACS="$EMACS_BIN" "${SCRIPT_DIR}/build-mirror.sh" "$MIRRORS_DIR" 2>&1 \
  | sed 's/^/   /'

# --- drop the rendered init.el alongside the tarball ---
OUT_VER_DIR="${MIRRORS_DIR}/emacs-${VER}"
mkdir -p "$OUT_VER_DIR"
cp "$RENDERED" "${OUT_VER_DIR}/init.el"

echo
echo "Done. Artefacts in: ${OUT_VER_DIR}"
ls -lh "$OUT_VER_DIR"

# --- optional: chain into build-toolkit.sh ---
if [[ "$CHAIN_TOOLKIT" -eq 1 ]]; then
  TOOLKIT_SCRIPT="${SCRIPT_DIR}/build-toolkit.sh"
  [[ -x "$TOOLKIT_SCRIPT" ]] \
    || { echo "build-toolkit.sh not found/executable: $TOOLKIT_SCRIPT" >&2; exit 1; }
  toolkit_args=(--target "emacs-${VER}" --with-source auto)
  [[ -n "$TOOLKIT_OUT_DIR" ]] && toolkit_args+=(--out-dir "$TOOLKIT_OUT_DIR")
  [[ "$USE_GZIP" -eq 1 ]] && toolkit_args+=(--gzip)
  echo
  echo ">> Chaining into build-toolkit.sh ${toolkit_args[*]}..."
  "$TOOLKIT_SCRIPT" "${toolkit_args[@]}"
fi
