#!/usr/bin/env bash
# prep-linux-offline.sh — pre-download Linux x86_64 tools for air-gapped targets
#
# Downloads Linux x86_64 binaries for language servers and debug adapters
# (JDTLS, netcoredbg, csharp-ls, ada_language_server, buf,
#  kotlin-language-server, typescript) and packages them into a
# self-contained archive with an offline installer.
#
# Usage:
#   ./prep-linux-offline.sh                       # output to ./
#   ./prep-linux-offline.sh /path/to/output        # explicit output dir
#
# Output:  <OUT_DIR>/linux-tools-offline-<STAMP>.tar.gz
#          Extracted contents:
#            install-offline.sh  — run on air-gapped Linux target
#            tools/              — pre-downloaded archives + pre-extracted dirs

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
OUT_DIR="$(cd "${1:-.}" && pwd)"
STAMP="$(date +%Y%m%dT%H%M%S)"
BUNDLE_NAME="linux-tools-offline-${STAMP}"
STAGING="$(mktemp -d "/tmp/${BUNDLE_NAME}-XXXXXX")"
trap 'rm -rf "$STAGING"' EXIT

TOOLS_DIR="${STAGING}/tools"
mkdir -p "$TOOLS_DIR"

echo "========================================================================"
echo "Pre-download Linux x86_64 tools for air-gapped deployment"
echo "========================================================================"
echo "Output bundle: ${OUT_DIR}/${BUNDLE_NAME}.tar.gz"
echo ""

download() {
  local url="$1" dest="$2"
  echo "   Downloading: $(basename "$dest")"
  if curl -fL# --connect-timeout 30 --retry 3 "$url" -o "$dest"; then
    echo "   $(du -h "$dest" | cut -f1)  saved"
    return 0
  else
    echo "   FAILED: $url" >&2
    return 1
  fi
}

FAILED=0

# ---------- 1. JDTLS ----------
echo ""
echo "[1/7] JDTLS (Eclipse JDT Language Server)"
mkdir -p "${TOOLS_DIR}/archives"
download \
  "https://download.eclipse.org/jdtls/snapshots/jdt-language-server-latest.tar.gz" \
  "${TOOLS_DIR}/archives/jdt-language-server-latest.tar.gz" \
  || FAILED=$((FAILED + 1))

# ---------- 2. netcoredbg ----------
echo ""
echo "[2/7] netcoredbg"
download \
  "https://github.com/Samsung/netcoredbg/releases/download/3.2.0-1092/netcoredbg-linux-amd64.tar.gz" \
  "${TOOLS_DIR}/archives/netcoredbg-linux-amd64.tar.gz" \
  || FAILED=$((FAILED + 1))

# ---------- 3. csharp-ls — no pre-built Linux binary ----------
echo ""
echo "[3/7] csharp-ls (requires .NET SDK on target)"
echo "   No pre-built Linux binary available."
echo "   Install on the target via: dotnet tool install --global csharp-ls"
echo "   NuGet: https://www.nuget.org/packages/csharp-ls"

# ---------- 4. ada_language_server ----------
echo ""
echo "[4/7] Ada Language Server"
download \
  "https://github.com/AdaCore/ada_language_server/releases/download/2026.3.202607051/als-2026.3.202607051-linux-x64.tar.gz" \
  "${TOOLS_DIR}/archives/als-2026.3.202607051-linux-x64.tar.gz" \
  || FAILED=$((FAILED + 1))

# ---------- 5. buf ----------
echo ""
echo "[5/7] buf"
download \
  "https://github.com/bufbuild/buf/releases/download/v1.50.0/buf-Linux-x86_64.tar.gz" \
  "${TOOLS_DIR}/archives/buf-Linux-x86_64.tar.gz" \
  || FAILED=$((FAILED + 1))

# ---------- 6. kotlin-language-server ----------
echo ""
echo "[6/7] Kotlin Language Server"
download \
  "https://github.com/fwcd/kotlin-language-server/releases/download/1.3.13/server.zip" \
  "${TOOLS_DIR}/archives/kotlin-language-server-server.zip" \
  || FAILED=$((FAILED + 1))

# ---------- 7. npm/typescript ----------
echo ""
echo "[7/7] TypeScript + typescript-language-server"
download \
  "https://registry.npmjs.org/typescript/-/typescript-5.8.3.tgz" \
  "${TOOLS_DIR}/archives/typescript-5.8.3.tgz" \
  || FAILED=$((FAILED + 1))
download \
  "https://registry.npmjs.org/typescript-language-server/-/typescript-language-server-4.3.4.tgz" \
  "${TOOLS_DIR}/archives/typescript-language-server-4.3.4.tgz" \
  || FAILED=$((FAILED + 1))

# ---------- Write the offline installer ----------
echo ""
echo ">> Writing install-offline.sh..."
cat > "${STAGING}/install-offline.sh" <<'INSTALLEOF'
#!/usr/bin/env bash
# install-offline.sh — install pre-downloaded Linux x86_64 tools (air-gapped)
# Run from the directory containing this file and the tools/ subdirectory.
# Installs to ~/.emacs.d/bin/ by default.
# Pass a different root:  install-offline.sh /path/to/tools
set -euo pipefail

ROOT="${1:-${HOME}/.emacs.d/bin}"
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ARCHIVES="${HERE}/tools/archives"
mkdir -p "$ROOT"

echo "========================================================================"
echo "Offline Linux Tools Installer"
echo "========================================================================"
echo "Installing to: ${ROOT}"
echo ""

installed() {
  local marker="$1"
  if [[ -f "${ROOT}/${marker}" ]]; then
    echo "   Already installed (delete ${ROOT}/${marker} to reinstall)"
    return 0
  fi
  return 1
}

# 1. JDTLS
echo "[1/7] JDTLS..."
if ! installed "jdtls/.installed"; then
  if [[ -f "${ARCHIVES}/jdt-language-server-latest.tar.gz" ]]; then
    mkdir -p "${ROOT}/jdtls"
    tar -xzf "${ARCHIVES}/jdt-language-server-latest.tar.gz" -C "${ROOT}/jdtls" --strip-components=1
    touch "${ROOT}/jdtls/.installed"
    echo "   Installed."
  else echo "   Skipped (archive not found)."; fi
fi

# 2. netcoredbg
echo "[2/7] netcoredbg..."
if ! installed "netcoredbg/.installed"; then
  if [[ -f "${ARCHIVES}/netcoredbg-linux-amd64.tar.gz" ]]; then
    mkdir -p "${ROOT}/netcoredbg"
    tar -xzf "${ARCHIVES}/netcoredbg-linux-amd64.tar.gz" -C "${ROOT}/netcoredbg"
    chmod +x "${ROOT}/netcoredbg/netcoredbg" 2>/dev/null || true
    touch "${ROOT}/netcoredbg/.installed"
    echo "   Installed."
  else echo "   Skipped (archive not found)."; fi
fi

# 3. csharp-ls — requires .NET SDK on target
echo "[3/7] csharp-ls..."
echo "   No pre-built Linux binary bundled. Install on target:"
echo "   dotnet tool install --global csharp-ls"

# 4. ada_language_server
echo "[4/7] Ada Language Server..."
if ! installed "ada_language_server/.installed"; then
  if [[ -f "${ARCHIVES}/als-2026.3.202607051-linux-x64.tar.gz" ]]; then
    mkdir -p "${ROOT}/ada_language_server"
    tar -xzf "${ARCHIVES}/als-2026.3.202607051-linux-x64.tar.gz" -C "${ROOT}/ada_language_server"
    chmod +x "${ROOT}/ada_language_server/bin/ada_language_server" 2>/dev/null || true
    touch "${ROOT}/ada_language_server/.installed"
    echo "   Installed."
  else echo "   Skipped (archive not found)."; fi
fi

# 5. buf
echo "[5/7] buf..."
if ! installed "buf/.installed"; then
  if [[ -f "${ARCHIVES}/buf-Linux-x86_64.tar.gz" ]]; then
    mkdir -p "${ROOT}/buf/bin"
    tar -xzf "${ARCHIVES}/buf-Linux-x86_64.tar.gz" -C "${ROOT}/buf" --strip-components=1
    chmod +x "${ROOT}/buf/bin/buf"
    touch "${ROOT}/buf/.installed"
    echo "   Installed."
  else echo "   Skipped (archive not found)."; fi
fi

# 6. kotlin-language-server
echo "[6/7] Kotlin Language Server..."
if ! installed "kotlin-language-server/.installed"; then
  if [[ -f "${ARCHIVES}/kotlin-language-server-server.zip" ]]; then
    mkdir -p "${ROOT}/kotlin-language-server"
    unzip -qo "${ARCHIVES}/kotlin-language-server-server.zip" -d "${ROOT}/kotlin-language-server"
    chmod +x "${ROOT}/kotlin-language-server/bin/kotlin-language-server" 2>/dev/null || true
    touch "${ROOT}/kotlin-language-server/.installed"
    echo "   Installed."
  else echo "   Skipped (archive not found)."; fi
fi

# 7. npm/typescript
echo "[7/7] TypeScript..."
if ! installed "npm/.installed"; then
  if [[ -f "${ARCHIVES}/typescript-5.8.3.tgz" ]]; then
    mkdir -p "${ROOT}/npm"
    cp "${ARCHIVES}/typescript-5.8.3.tgz" "${ROOT}/npm/"
    cp "${ARCHIVES}/typescript-language-server-4.3.4.tgz" "${ROOT}/npm/"
    echo "   Archives copied to ${ROOT}/npm/. Install with: npm install -g ..."
    touch "${ROOT}/npm/.installed"
  else echo "   Skipped (archive not found)."; fi
fi

echo ""
echo "========================================================================"
echo "Installation complete!"
echo "Tools installed to: ${ROOT}"
echo "========================================================================"
echo ""
echo "Add to your PATH:"
echo "  export PATH=\"\${PATH}:${ROOT}/jdtls/bin:${ROOT}/netcoredbg:${ROOT}/csharp-ls:${ROOT}/ada_language_server/bin:${ROOT}/buf/bin:${ROOT}/kotlin-language-server/bin\""
echo ""
INSTALLEOF
chmod +x "${STAGING}/install-offline.sh"

# ---------- Write checksum manifest ----------
echo ""
echo ">> Writing checksums..."
(cd "$TOOLS_DIR/archives" && sha256sum *) > "${TOOLS_DIR}/checksums.sha256" 2>/dev/null || true

# ---------- Package ----------
echo ""
echo ">> Packaging ${OUT_DIR}/${BUNDLE_NAME}.tar.gz..."
mkdir -p "$OUT_DIR"
tar -C "$STAGING" -czf "${OUT_DIR}/${BUNDLE_NAME}.tar.gz" \
  --transform="s|^\./|${BUNDLE_NAME}/|" \
  "$(basename "${TOOLS_DIR}")" \
  "$(basename "${STAGING}/install-offline.sh")"

SIZE="$(du -h "${OUT_DIR}/${BUNDLE_NAME}.tar.gz" | cut -f1)"
echo ""
[[ "$FAILED" -gt 0 ]] && echo "WARNING: ${FAILED} download(s) failed."
echo "======================================"
echo "Bundle: ${OUT_DIR}/${BUNDLE_NAME}.tar.gz"
echo "Size:   ${SIZE}"
echo "======================================"
echo ""
echo "Deploy to air-gapped Linux machine:"
echo "  1. Copy the .tar.gz via USB"
echo "  2. tar -xzf ${BUNDLE_NAME}.tar.gz"
echo "  3. cd ${BUNDLE_NAME} && ./install-offline.sh"
