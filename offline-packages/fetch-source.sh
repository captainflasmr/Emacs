#!/usr/bin/env bash
# fetch-source.sh — download a GNU Emacs source tarball into sources/.
#
# With no argument, reads sources/LATEST_STABLE for the version.
# Idempotent: skips the download if the tarball is already present.
#
# Usage: ./fetch-source.sh [VER]
#   VER        Emacs version (e.g. 30.1). Defaults to contents of LATEST_STABLE.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SOURCES_DIR="${SCRIPT_DIR}/sources"
LATEST_FILE="${SOURCES_DIR}/LATEST_STABLE"

VER="${1:-}"
if [[ -z "$VER" ]]; then
  [[ -f "$LATEST_FILE" ]] \
    || { echo "No version given and $LATEST_FILE missing." >&2; exit 1; }
  VER="$(head -n1 "$LATEST_FILE" | tr -d '[:space:]')"
fi
VER="${VER#emacs-}"

mkdir -p "$SOURCES_DIR"
TARBALL="emacs-${VER}.tar.xz"
OUT="${SOURCES_DIR}/${TARBALL}"

if [[ -f "$OUT" ]]; then
  echo ">> ${TARBALL} already present (skipping download)"
  ls -lh "$OUT"
  exit 0
fi

URL="https://ftpmirror.gnu.org/emacs/${TARBALL}"
echo ">> Downloading ${URL}"
curl -fL --progress-bar "$URL" -o "$OUT"
ls -lh "$OUT"
