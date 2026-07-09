#!/usr/bin/env bash
# prep-windows-offline.sh — pre-download Windows tool binaries for air-gapped targets
#
# Downloads the same Windows-native tools that setup-windows-tools.bat installs,
# then packages them into a self-contained archive with an offline installer.
# Run on the ONLINE build machine (Linux or Windows with bash).
#
# Usage:
#   ./prep-windows-offline.sh                         # output to ./windows-tools-offline/
#   ./prep-windows-offline.sh /path/to/output          # explicit output dir
#
# Output:  <OUT_DIR>/windows-tools-offline-<STAMP>.tar.gz
#          Extracted contents:
#            install-offline.bat    — run on air-gapped Windows target
#            tools/                — pre-downloaded archives + pre-extracted dirs
#
# The resulting tarball can be carried via USB to the air-gapped Windows
# machine, extracted, and installed by running install-offline.bat.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
OUT_DIR="$(cd "${1:-.}" && pwd)"
STAMP="$(date +%Y%m%dT%H%M%S)"
BUNDLE_NAME="windows-tools-offline-${STAMP}"
STAGING="$(mktemp -d "/tmp/${BUNDLE_NAME}-XXXXXX")"
trap 'rm -rf "$STAGING"' EXIT

TOOLS_DIR="${STAGING}/tools"
mkdir -p "$TOOLS_DIR"

echo "========================================================================"
echo "Pre-download Windows tools for air-gapped deployment"
echo "========================================================================"
echo "Output bundle: ${OUT_DIR}/${BUNDLE_NAME}.tar.gz"
echo ""

# Helper: download with progress bar
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

# ---------- 1. PortableGit ----------
echo ""
echo "[1/16] PortableGit (Git + coreutils)"
mkdir -p "${TOOLS_DIR}/archives"
download \
  "https://github.com/git-for-windows/git/releases/download/v2.50.0.windows.1/PortableGit-2.50.0-64-bit.7z.exe" \
  "${TOOLS_DIR}/archives/PortableGit-2.50.0-64-bit.7z.exe" \
  || FAILED=$((FAILED + 1))

# ---------- 2. ripgrep ----------
echo ""
echo "[2/16] ripgrep"
mkdir -p "${TOOLS_DIR}/archives"
download \
  "https://github.com/BurntSushi/ripgrep/releases/download/14.1.1/ripgrep-14.1.1-x86_64-pc-windows-gnu.zip" \
  "${TOOLS_DIR}/archives/ripgrep-14.1.1-x86_64-pc-windows-gnu.zip" \
  || FAILED=$((FAILED + 1))

# ---------- 3. Hunspell ----------
echo ""
echo "[3/16] Hunspell"
download \
  "https://github.com/iquiw/hunspell-binary/releases/download/v1.7.3/hunspell-v1.7.3.7z" \
  "${TOOLS_DIR}/archives/hunspell-v1.7.3.7z" \
  || FAILED=$((FAILED + 1))
# Pre-extract on Linux so Windows install is a plain file copy
if [ -f "${TOOLS_DIR}/archives/hunspell-v1.7.3.7z" ]; then
  echo "   Pre-extracting Hunspell for offline install..."
  mkdir -p "${TOOLS_DIR}/hunspell"
  if command -v 7z &>/dev/null; then
    7z x "${TOOLS_DIR}/archives/hunspell-v1.7.3.7z" \
      -o"${TOOLS_DIR}/hunspell" -y >/dev/null
  elif command -v bsdtar &>/dev/null; then
    bsdtar -xf "${TOOLS_DIR}/archives/hunspell-v1.7.3.7z" \
      -C "${TOOLS_DIR}/hunspell"
  else
    tar -xf "${TOOLS_DIR}/archives/hunspell-v1.7.3.7z" \
      -C "${TOOLS_DIR}/hunspell"
  fi
  echo "   Pre-extracted."
fi
# Download English dictionary alongside pre-extracted binaries
echo "   Downloading English dictionary..."
mkdir -p "${TOOLS_DIR}/hunspell/share/hunspell"
download \
  "https://raw.githubusercontent.com/wooorm/dictionaries/main/dictionaries/en-GB/index.dic" \
  "${TOOLS_DIR}/hunspell/share/hunspell/en_GB.dic" \
  || FAILED=$((FAILED + 1))
download \
  "https://raw.githubusercontent.com/wooorm/dictionaries/main/dictionaries/en-GB/index.aff" \
  "${TOOLS_DIR}/hunspell/share/hunspell/en_GB.aff" \
  || FAILED=$((FAILED + 1))

# ---------- 4. netcoredbg ----------
echo ""
echo "[4/16] netcoredbg"
download \
  "https://github.com/Samsung/netcoredbg/releases/download/3.2.0-1092/netcoredbg-win64.zip" \
  "${TOOLS_DIR}/archives/netcoredbg-win64.zip" \
  || FAILED=$((FAILED + 1))

# ---------- 5. ffmpeg ----------
echo ""
echo "[5/16] ffmpeg"
download \
  "https://www.gyan.dev/ffmpeg/builds/ffmpeg-release-essentials.zip" \
  "${TOOLS_DIR}/archives/ffmpeg-release-essentials.zip" \
  || FAILED=$((FAILED + 1))

# ---------- 6. ImageMagick (7z; pre-extracted to avoid 7z dep on Windows) ----------
echo ""
echo "[6/16] ImageMagick (7z)"
download \
  "https://github.com/ImageMagick/ImageMagick/releases/download/7.1.2-27/ImageMagick-7.1.2-27-portable-Q16-x64.7z" \
  "${TOOLS_DIR}/archives/ImageMagick-7.1.2-27-portable-Q16-x64.7z" \
  || FAILED=$((FAILED + 1))
# Pre-extract on Linux so Windows install is a plain file copy
if [ -f "${TOOLS_DIR}/archives/ImageMagick-7.1.2-27-portable-Q16-x64.7z" ]; then
  echo "   Pre-extracting ImageMagick for offline install..."
  mkdir -p "${TOOLS_DIR}/ImageMagick-7.1.2-27-portable-Q16-x64"
  # Prefer p7zip if available; fall back to bsdtar (libarchive)
  if command -v 7z &>/dev/null; then
    7z x "${TOOLS_DIR}/archives/ImageMagick-7.1.2-27-portable-Q16-x64.7z" \
      -o"${TOOLS_DIR}/ImageMagick-7.1.2-27-portable-Q16-x64" -y >/dev/null
  elif command -v bsdtar &>/dev/null; then
    bsdtar -xf "${TOOLS_DIR}/archives/ImageMagick-7.1.2-27-portable-Q16-x64.7z" \
      -C "${TOOLS_DIR}/ImageMagick-7.1.2-27-portable-Q16-x64"
  else
    tar -xf "${TOOLS_DIR}/archives/ImageMagick-7.1.2-27-portable-Q16-x64.7z" \
      -C "${TOOLS_DIR}/ImageMagick-7.1.2-27-portable-Q16-x64"
  fi
  echo "   Pre-extracted."
fi

# ---------- 7. CMake ----------
echo ""
echo "[7/16] CMake"
download \
  "https://github.com/Kitware/CMake/releases/download/v4.2.0/cmake-4.2.0-windows-x86_64.zip" \
  "${TOOLS_DIR}/archives/cmake-4.2.0-windows-x86_64.zip" \
  || FAILED=$((FAILED + 1))

# ---------- 8. Clang/LLVM ----------
echo ""
echo "[8/16] Clang/LLVM"
download \
  "https://github.com/llvm/llvm-project/releases/download/llvmorg-20.1.0/LLVM-20.1.0-win64.exe" \
  "${TOOLS_DIR}/archives/LLVM-20.1.0-win64.exe" \
  || FAILED=$((FAILED + 1))

# ---------- 9. ada_language_server ----------
echo ""
echo "[9/16] Ada Language Server"
download \
  "https://github.com/AdaCore/ada_language_server/releases/download/2026.3.202607051/als-2026.3.202607051-win32-x64.tar.gz" \
  "${TOOLS_DIR}/archives/als-2026.3.202607051-win32-x64.tar.gz" \
  || FAILED=$((FAILED + 1))

# ---------- 10. buf ----------
echo ""
echo "[10/16] buf (protobuf)"
download \
  "https://github.com/bufbuild/buf/releases/download/v1.50.0/buf-Windows-x86_64.exe" \
  "${TOOLS_DIR}/archives/buf-Windows-x86_64.exe" \
  || FAILED=$((FAILED + 1))

# ---------- 11. protoc ----------
echo ""
echo "[11/16] protoc"
download \
  "https://github.com/protocolbuffers/protobuf/releases/download/v31.1/protoc-31.1-win64.zip" \
  "${TOOLS_DIR}/archives/protoc-31.1-win64.zip" \
  || FAILED=$((FAILED + 1))

# ---------- 12. JDTLS (just the Windows launcher + config) ----------
echo ""
echo "[12/16] JDTLS (Eclipse JDT Language Server — Windows launcher)"
# The full JDTLS tarball includes the launcher; the JARs are already in the
# Linux toolkit tools/ and work cross-platform.
download \
  "https://download.eclipse.org/jdtls/snapshots/jdt-language-server-latest.tar.gz" \
  "${TOOLS_DIR}/archives/jdt-language-server-latest.tar.gz" \
  || FAILED=$((FAILED + 1))

# ---------- 13. kotlin-language-server (Windows launcher) ----------
echo ""
echo "[13/16] Kotlin Language Server (Windows launcher)"
# The JARs are cross-platform and already in the Linux toolkit tools/;
# download the full release for the .bat launcher.
download \
  "https://github.com/fwcd/kotlin-language-server/releases/download/1.3.13/server.zip" \
  "${TOOLS_DIR}/archives/kotlin-language-server-server.zip" \
  || FAILED=$((FAILED + 1))

# ---------- 14. Apache-Subversion ----------
echo ""
echo "[14/16] Apache-Subversion (svn)"
download \
  "https://sliksvn.com/pub/Slik-Subversion-1.14.5-x64.zip" \
  "${TOOLS_DIR}/archives/Slik-Subversion-1.14.5-x64.zip" \
  || FAILED=$((FAILED + 1))

# ---------- 15. omnisharp-win-x64 ----------
echo ""
echo "[15/16] omnisharp-win-x64 (OmniSharp C# LSP)"
download \
  "https://github.com/OmniSharp/omnisharp-roslyn/releases/download/v1.39.11/omnisharp-win-x64.zip" \
  "${TOOLS_DIR}/archives/omnisharp-win-x64.zip" \
  || FAILED=$((FAILED + 1))

# ---------- 16. exiftool ----------
echo ""
echo "[16/16] exiftool"
download \
  "https://sourceforge.net/projects/exiftool/files/exiftool-13.59_64.zip/download" \
  "${TOOLS_DIR}/archives/exiftool-13.59_64.zip" \
  || FAILED=$((FAILED + 1))

# ---------- Write the offline installer ----------
echo ""
echo ">> Writing install-offline.bat..."
cat > "${STAGING}/install-offline.bat" <<'INSTALLEOF'
@echo off
:: install-offline.bat — install pre-downloaded Windows tools (air-gapped)
:: Run from the directory containing this file and the tools/ subdirectory.
:: Installs to %APPDATA%\.emacs.d\bin\ by default.
:: Pass a different root:  install-offline.bat D:\tools
setlocal enabledelayedexpansion

:: Ensure System32 and common Windows binary locations are on PATH.
set "WIN_PATH=%SystemRoot%\System32;%SystemRoot%;%SystemRoot%\System32\Wbem;%SystemRoot%\System32\WindowsPowerShell\v1.0\;%SystemRoot%\System32\OpenSSH\"
set "PATH=%WIN_PATH%;%PATH%"

set ROOT=%~1
if "%ROOT%"=="" set ROOT=%APPDATA%\.emacs.d\bin
set HERE=%~dp0
set ARCHIVES=%HERE%tools\archives
mkdir "%ROOT%" 2>nul

echo =================================================================
echo Offline Windows Tools Installer
echo =================================================================
echo Installing to: %ROOT%
echo[

:install_portablegit
if exist "%ROOT%\PortableGit\.done" goto :skip_portablegit
  echo [1/16] PortableGit...
  if exist "%ARCHIVES%\PortableGit-2.50.0-64-bit.7z.exe" (
    "%ARCHIVES%\PortableGit-2.50.0-64-bit.7z.exe" -o"%ROOT%\PortableGit" -y >nul
    copy nul "%ROOT%\PortableGit\.done" >nul
    echo    Installed.
  ) else (
    echo    Skipped ^(archive not found^).
  )
:skip_portablegit

:install_ripgrep
if exist "%ROOT%\find\rg.exe" goto :skip_ripgrep
  echo [2/16] ripgrep...
  if exist "%ARCHIVES%\ripgrep-14.1.1-x86_64-pc-windows-gnu.zip" (
    mkdir "%ROOT%\find" 2>nul
    tar -xf "%ARCHIVES%\ripgrep-14.1.1-x86_64-pc-windows-gnu.zip" -C "%TMP%" --strip-components=1
    move "%TMP%\rg.exe" "%ROOT%\find\rg.exe" >nul
    echo    Installed.
  ) else (
    echo    Skipped ^(archive not found^).
  )
:skip_ripgrep

:install_hunspell
if exist "%ROOT%\hunspell\.done" goto :skip_hunspell
  echo [3/16] Hunspell...
  set "HUN_PREEXTRACTED=%HERE%tools\hunspell"
  if exist "!HUN_PREEXTRACTED!" (
    xcopy /E /I /Y "!HUN_PREEXTRACTED!" "%ROOT%\hunspell" >nul
  ) else if exist "%ARCHIVES%\hunspell-v1.7.3.7z" (
    mkdir "%ROOT%\hunspell" 2>nul
    tar -xf "%ARCHIVES%\hunspell-v1.7.3.7z" -C "%ROOT%\hunspell" --strip-components=1
  ) else (
    echo    Skipped ^(no pre-extracted dir or .7z archive found^).
    goto :skip_hunspell
  )
  :: Download English dictionary
  if not exist "%ROOT%\hunspell\share\hunspell" mkdir "%ROOT%\hunspell\share\hunspell"
  if not exist "%ROOT%\hunspell\share\hunspell\en_GB.dic" (
    echo    Downloading dictionary...
    powershell -Command "Invoke-WebRequest -Uri 'https://raw.githubusercontent.com/wooorm/dictionaries/main/dictionaries/en-GB/index.dic' -OutFile '%ROOT%\hunspell\share\hunspell\en_GB.dic'" 2>nul
    powershell -Command "Invoke-WebRequest -Uri 'https://raw.githubusercontent.com/wooorm/dictionaries/main/dictionaries/en-GB/index.aff' -OutFile '%ROOT%\hunspell\share\hunspell\en_GB.aff'" 2>nul
  )
  :: Copy dictionaries to C:\Hunspell\ (in hunspell's built-in search path)
  if not exist "C:\Hunspell" mkdir "C:\Hunspell"
  if exist "%ROOT%\hunspell\share\hunspell\en_GB.dic" (
    copy /Y "%ROOT%\hunspell\share\hunspell\en_GB.dic" "C:\Hunspell\en_GB.dic" >nul
    copy /Y "%ROOT%\hunspell\share\hunspell\en_GB.aff" "C:\Hunspell\en_GB.aff" >nul
    copy /Y "%ROOT%\hunspell\share\hunspell\en_GB.dic" "C:\Hunspell\default.dic" >nul
    copy /Y "%ROOT%\hunspell\share\hunspell\en_GB.aff" "C:\Hunspell\default.aff" >nul
  )
  copy nul "%ROOT%\hunspell\.done" >nul
  echo    Installed.
:skip_hunspell

:install_netcoredbg
if exist "%ROOT%\netcoredbg\.done" goto :skip_netcoredbg
  echo [4/16] netcoredbg...
  if exist "%ARCHIVES%\netcoredbg-win64.zip" (
    mkdir "%ROOT%\netcoredbg" 2>nul
    tar -xf "%ARCHIVES%\netcoredbg-win64.zip" -C "%ROOT%\netcoredbg"
    copy nul "%ROOT%\netcoredbg\.done" >nul
    echo    Installed.
  ) else (
    echo    Skipped ^(archive not found^).
  )
:skip_netcoredbg

:install_ffmpeg
if exist "%ROOT%\ffmpeg-*-essentials_build\.done" goto :skip_ffmpeg
  echo [5/16] ffmpeg...
  if exist "%ARCHIVES%\ffmpeg-release-essentials.zip" (
    mkdir "%TMP%\ffmpeg-extract" 2>nul
    tar -xf "%ARCHIVES%\ffmpeg-release-essentials.zip" -C "%TMP%\ffmpeg-extract"
    for /d %%d in ("%TMP%\ffmpeg-extract\ffmpeg-*-essentials_build") do (
      if exist "%ROOT%\%%~nxd" rmdir /s /q "%ROOT%\%%~nxd"
      move "%%d" "%ROOT%\" >nul
    )
    rmdir /s /q "%TMP%\ffmpeg-extract" 2>nul
    for /d %%d in ("%ROOT%\ffmpeg-*-essentials_build") do (
      copy nul "%%d\.done" >nul
    )
    echo    Installed.
  ) else (
    echo    Skipped ^(archive not found^).
  )
:skip_ffmpeg

:install_imagemagick
if exist "%ROOT%\ImageMagick-7.1.2-27-portable-Q16-x64\.done" goto :skip_imagemagick
  echo [6/16] ImageMagick...
  set "IM_PREEXTRACTED=%HERE%tools\ImageMagick-7.1.2-27-portable-Q16-x64"
  if exist "!IM_PREEXTRACTED!" (
    xcopy /E /I /Y "!IM_PREEXTRACTED!" "%ROOT%\ImageMagick-7.1.2-27-portable-Q16-x64" >nul
    copy nul "%ROOT%\ImageMagick-7.1.2-27-portable-Q16-x64\.done" >nul
    echo    Installed ^(from pre-extracted^).
  ) else if exist "%ARCHIVES%\ImageMagick-7.1.2-27-portable-Q16-x64.7z" (
    tar -xf "%ARCHIVES%\ImageMagick-7.1.2-27-portable-Q16-x64.7z" -C "%ROOT%"
    if exist "%ROOT%\ImageMagick-7.1.2-27-portable-Q16-x64" (
      copy nul "%ROOT%\ImageMagick-7.1.2-27-portable-Q16-x64\.done" >nul
      echo    Installed.
    ) else (
      echo    Failed to extract .7z. Install manually from:
      echo    https://imagemagick.org/script/download.php
    )
  ) else (
    echo    Skipped ^(neither pre-extracted dir nor .7z archive found^).
  )
:skip_imagemagick

:install_cmake
if exist "%ROOT%\cmake\.done" goto :skip_cmake
  echo [7/16] CMake...
  if exist "%ARCHIVES%\cmake-4.2.0-windows-x86_64.zip" (
    mkdir "%ROOT%\cmake" 2>nul
    tar -xf "%ARCHIVES%\cmake-4.2.0-windows-x86_64.zip" -C "%TMP%"
    xcopy /E /I /Y "%TMP%\cmake-4.2.0-windows-x86_64\bin" "%ROOT%\cmake\bin\" >nul 2>nul
    rmdir /s /q "%TMP%\cmake-4.2.0-windows-x86_64" 2>nul
    copy nul "%ROOT%\cmake\.done" >nul
    echo    Installed.
  ) else (
    echo    Skipped ^(archive not found^).
  )
:skip_cmake

:install_clang
if exist "%ROOT%\clang\.done" goto :skip_clang
  echo [8/16] Clang/LLVM...
  if exist "%ARCHIVES%\LLVM-20.1.0-win64.exe" (
    mkdir "%ROOT%\clang" 2>nul
    "%ARCHIVES%\LLVM-20.1.0-win64.exe" /S /D="%ROOT%\clang"
    copy nul "%ROOT%\clang\.done" >nul
    echo    Installed.
  ) else (
    echo    Skipped ^(archive not found^).
  )
:skip_clang

:install_ada
if exist "%ROOT%\ada_language_server\.done" goto :skip_ada
  echo [9/16] Ada Language Server...
  for %%f in ("%ARCHIVES%\als-*-win32-x64.tar.gz") do if exist "%%f" (
    mkdir "%ROOT%\ada_language_server" 2>nul
    tar -xf "%%f" -C "%ROOT%\ada_language_server"
    if exist "%ROOT%\ada_language_server\*.debug" del "%ROOT%\ada_language_server\*.debug"
    copy nul "%ROOT%\ada_language_server\.done" >nul
    echo    Installed.
  ) else (
    echo    Skipped ^(archive not found^).
  )
:skip_ada

:install_buf
if exist "%ROOT%\buf\.done" goto :skip_buf
  echo [10/16] buf...
  if exist "%ARCHIVES%\buf-Windows-x86_64.exe" (
    mkdir "%ROOT%\buf" 2>nul
    mkdir "%ROOT%\buf\bin" 2>nul
    copy "%ARCHIVES%\buf-Windows-x86_64.exe" "%ROOT%\buf\buf.exe" >nul
    copy "%ROOT%\buf\buf.exe" "%ROOT%\buf\bin\buf.exe" >nul
    copy nul "%ROOT%\buf\.done" >nul
    echo    Installed.
  ) else (
    echo    Skipped ^(archive not found^).
  )
:skip_buf

:install_protoc
if exist "%ROOT%\protoc\.done" goto :skip_protoc
  echo [11/16] protoc...
  if exist "%ARCHIVES%\protoc-31.1-win64.zip" (
    mkdir "%ROOT%\protoc" 2>nul
    mkdir "%TMP%\protoc-extract" 2>nul
    tar -xf "%ARCHIVES%\protoc-31.1-win64.zip" -C "%TMP%\protoc-extract" 2>nul
    for /r "%TMP%\protoc-extract" %%f in (protoc.exe) do (
      if exist "%%f" move "%%f" "%ROOT%\protoc\" >nul
    ) 2>nul
    rmdir /s /q "%TMP%\protoc-extract" 2>nul
    if exist "%ROOT%\protoc\protoc.exe" (
      copy nul "%ROOT%\protoc\.done" >nul
      echo    Installed.
    ) else (
      echo    Failed to extract. Try installing protoc manually.
    )
  ) else (
    echo    Skipped ^(archive not found^).
  )
:skip_protoc

:install_jdtls
echo [12/16] JDTLS...
if exist "%ARCHIVES%\jdt-language-server-latest.tar.gz" (
  mkdir "%ROOT%\jdtls" 2>nul
  tar -xzf "%ARCHIVES%\jdt-language-server-latest.tar.gz" -C "%ROOT%\jdtls" --strip-components=1
  echo    Installed.
) else (
  echo    Skipped ^(archive not found^).
)
:skip_jdtls

:install_kotlin
echo [13/16] Kotlin Language Server...
if exist "%ARCHIVES%\kotlin-language-server-server.zip" (
  mkdir "%ROOT%\kotlin-language-server" 2>nul
  tar -xf "%ARCHIVES%\kotlin-language-server-server.zip" -C "%ROOT%\kotlin-language-server"
  echo    Installed.
) else (
  echo    Skipped ^(archive not found^).
)
:skip_kotlin

:install_svn
if exist "%ROOT%\svn\.done" goto :skip_svn
  echo [14/16] Apache-Subversion (svn)...
  if exist "%ARCHIVES%\Slik-Subversion-1.14.5-x64.zip" (
    mkdir "%ROOT%\svn" 2>nul
    tar -xf "%ARCHIVES%\Slik-Subversion-1.14.5-x64.zip" -C "%ROOT%\svn" --strip-components=1
    copy nul "%ROOT%\svn\.done" >nul
    echo    Installed.
  ) else (
    echo    Skipped ^(archive not found^).
  )
:skip_svn

:install_omnisharp
if exist "%ROOT%\omnisharp\.done" goto :skip_omnisharp
  echo [15/16] omnisharp-win-x64 (OmniSharp C# LSP)...
  if exist "%ARCHIVES%\omnisharp-win-x64.zip" (
    mkdir "%ROOT%\omnisharp" 2>nul
    tar -xf "%ARCHIVES%\omnisharp-win-x64.zip" -C "%ROOT%\omnisharp" --strip-components=1
    copy nul "%ROOT%\omnisharp\.done" >nul
    echo    Installed.
  ) else (
    echo    Skipped ^(archive not found^).
  )
:skip_omnisharp

:install_exiftool
if exist "%ROOT%\exiftool\.done" goto :skip_exiftool
  echo [16/16] exiftool...
  if exist "%ARCHIVES%\exiftool-13.59_64.zip" (
    mkdir "%ROOT%\exiftool" 2>nul
    tar -xf "%ARCHIVES%\exiftool-13.59_64.zip" -C "%ROOT%\exiftool" --strip-components=1
    if exist "%ROOT%\exiftool\exiftool(-k).exe" rename "%ROOT%\exiftool\exiftool(-k).exe" exiftool.exe
    copy nul "%ROOT%\exiftool\.done" >nul
    echo    Installed.
  ) else (
    echo    Skipped ^(archive not found^).
  )
:skip_exiftool

:install_csharpls
echo(
echo [extra] csharp-ls requires .NET SDK: dotnet tool install --global csharp-ls

echo(
echo =================================================================
echo Installation complete!
echo Tools installed to: %ROOT%
echo =================================================================
echo(
echo Add to your Windows init.el PATH configuration:
echo   (when (eq system-type 'windows-nt^)
echo     (let* ((bin-root "%ROOT:\=\\%"^)^)
echo            ...
echo            ,(concat bin-root "/PortableGit/bin"^)
echo            ,(concat bin-root "/find"^)
echo            ,(concat bin-root "/netcoredbg"^)
echo            ,(concat bin-root "/csharp-ls"^)
echo            ...^)^))
echo(
pause
INSTALLEOF

# ---------- Write checksum manifest ----------
echo ""
echo ">> Writing checksums..."
(cd "$TOOLS_DIR/archives" && sha256sum *) > "${TOOLS_DIR}/checksums.sha256" 2>/dev/null || true

# ---------- Copy setup-windows-tools.bat for reference ----------
cp "${SCRIPT_DIR}/setup-windows-tools.bat" "${TOOLS_DIR}/" 2>/dev/null || true

# ---------- Package ----------
echo ""
echo ">> Packaging ${OUT_DIR}/${BUNDLE_NAME}.tar.gz..."
mkdir -p "$OUT_DIR"
tar -C "$STAGING" -czf "${OUT_DIR}/${BUNDLE_NAME}.tar.gz" \
  --transform="s|^\./|${BUNDLE_NAME}/|" \
  "$(basename "${TOOLS_DIR}")" \
  "$(basename "${STAGING}/install-offline.bat")"

SIZE="$(du -h "${OUT_DIR}/${BUNDLE_NAME}.tar.gz" | cut -f1)"
echo ""
if [[ "$FAILED" -gt 0 ]]; then
  echo "WARNING: ${FAILED} download(s) failed. Bundle may be incomplete."
fi
echo "======================================"
echo "Bundle: ${OUT_DIR}/${BUNDLE_NAME}.tar.gz"
echo "Size:   ${SIZE}"
echo "======================================"
echo ""
echo "Deploy to air-gapped Windows machine:"
echo "  1. Copy the .tar.gz via USB"
echo "  2. Extract: tar -xzf ${BUNDLE_NAME}.tar.gz"
echo "  3. Run:     cd ${BUNDLE_NAME} && install-offline.bat"
