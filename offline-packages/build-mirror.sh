#!/usr/bin/env bash
# Build a local ELPA mirror for offline use via elpa-mirror.
# Run on the ONLINE machine. Emacs version should match the offline target.
#
# Usage: ./build-mirror.sh [OUT_DIR]
#   OUT_DIR defaults to $HOME.
#
# Output layout:
#   OUT_DIR/emacs-<ver>/<os-slug>/<arch>/<mirror-base>.tar.gz
# where <mirror-base> is elpa-mirror-emacs-<ver>-<os-slug>-<arch>-<stamp>-<N>pkgs.
# The tarball also embeds MANIFEST.txt (build provenance) and PACKAGES.txt.

set -euo pipefail

OUT_DIR="${1:-$HOME}"
EMACS_VERSION="$(emacs --batch --eval '(princ emacs-version)' 2>&1 | tail -n1)"
ARCH="$(uname -m)"
STAMP="$(date -u +%Y%m%dT%H%M%SZ)"

# OS slug: prefer ID[-VERSION_ID] from /etc/os-release (e.g. "arch", "ubuntu-22.04")
OS_SLUG="unknown"
OS_NAME="unknown"
if [[ -r /etc/os-release ]]; then
  # shellcheck disable=SC1091
  . /etc/os-release
  OS_NAME="${PRETTY_NAME:-${NAME:-unknown}}"
  OS_SLUG="${ID:-unknown}"
  [[ -n "${VERSION_ID:-}" ]] && OS_SLUG="${OS_SLUG}-${VERSION_ID}"
fi

# Build into a staging dir; rename once we know the package count.
STAGING="${OUT_DIR}/.elpa-mirror-staging-$$"
trap 'rm -rf "$STAGING"' EXIT

emacs --batch \
  --eval "(require 'package)" \
  --eval "(package-initialize)" \
  --eval "(unless (package-installed-p 'elpa-mirror)
            (add-to-list 'package-archives
              '(\"melpa\" . \"https://melpa.org/packages/\") t)
            (package-refresh-contents)
            (package-install 'elpa-mirror))" \
  --eval "(require 'elpa-mirror)" \
  --eval "(elpamr-create-mirror-for-installed \"${STAGING}\" t)"

# PACKAGES.txt — sorted list of installed packages with versions
emacs --batch \
  --eval "(require 'package)" \
  --eval "(package-initialize)" \
  --eval "(with-temp-file \"${STAGING}/PACKAGES.txt\"
            (dolist (pkg (sort (mapcar #'car package-alist) #'string<))
              (let* ((desc (cadr (assq pkg package-alist)))
                     (ver  (package-desc-version desc))
                     (sum  (package-desc-summary desc)))
                (insert (format \"%-35s %-15s %s\n\"
                                pkg
                                (package-version-join ver)
                                (or sum \"\"))))))"

PKG_COUNT="$(wc -l < "${STAGING}/PACKAGES.txt" | tr -d ' ')"

MIRROR_BASE="elpa-mirror-emacs-${EMACS_VERSION}-${OS_SLUG}-${ARCH}-${STAMP}-${PKG_COUNT}pkgs"
TARGET_DIR="${OUT_DIR}/emacs-${EMACS_VERSION}/${OS_SLUG}/${ARCH}"
OUT_TARBALL="${TARGET_DIR}/${MIRROR_BASE}.tar.gz"

# MANIFEST.txt — source-system provenance
{
  echo "Built:         $(date -Iseconds)"
  echo "Timestamp:     ${STAMP}"
  echo "Host:          $(hostname)"
  echo "User:          $(id -un)"
  echo "Kernel:        $(uname -srm)"
  echo "OS:            ${OS_NAME}"
  echo "OS slug:       ${OS_SLUG}"
  echo "Architecture:  ${ARCH}"
  echo "Emacs:         ${EMACS_VERSION}"
  echo "Emacs binary:  $(command -v emacs)"
  echo "Package count: ${PKG_COUNT}"
} > "${STAGING}/MANIFEST.txt"

# Rename staging to final internal name, tar into hierarchy, then clean up.
MIRROR_DIR="${OUT_DIR}/${MIRROR_BASE}"
mv "$STAGING" "$MIRROR_DIR"
trap 'rm -rf "$MIRROR_DIR"' EXIT

mkdir -p "$TARGET_DIR"
tar -C "$OUT_DIR" -czf "$OUT_TARBALL" "$MIRROR_BASE"

rm -rf "$MIRROR_DIR"
trap - EXIT

cat <<EOF

Built: ${OUT_TARBALL}

--- MANIFEST.txt ---
$(tar -xzOf "$OUT_TARBALL" "${MIRROR_BASE}/MANIFEST.txt")

${PKG_COUNT} packages — see ${MIRROR_BASE}/PACKAGES.txt inside the tarball.

On the offline machine:
  tar -xzf ${MIRROR_BASE}.tar.gz -C ~/
  # then in init.el, before (package-initialize):
  (setq package-archives '(("local" . "~/${MIRROR_BASE}/")))
EOF
