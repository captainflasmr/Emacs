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
echo "[1/17] PortableGit (Git + coreutils)"
mkdir -p "${TOOLS_DIR}/archives"
download \
  "https://github.com/git-for-windows/git/releases/download/v2.50.0.windows.1/PortableGit-2.50.0-64-bit.7z.exe" \
  "${TOOLS_DIR}/archives/PortableGit-2.50.0-64-bit.7z.exe" \
  || FAILED=$((FAILED + 1))

# ---------- 2. ripgrep ----------
echo ""
echo "[2/17] ripgrep"
mkdir -p "${TOOLS_DIR}/archives"
download \
  "https://github.com/BurntSushi/ripgrep/releases/download/14.1.1/ripgrep-14.1.1-x86_64-pc-windows-gnu.zip" \
  "${TOOLS_DIR}/archives/ripgrep-14.1.1-x86_64-pc-windows-gnu.zip" \
  || FAILED=$((FAILED + 1))

# ---------- 3. Hunspell ----------
echo ""
echo "[3/17] Hunspell"
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
# Bundle dictionaries in a dedicated dir so the offline installer can copy
# them regardless of which hunspell extraction path is taken.
mkdir -p "${TOOLS_DIR}/dictionaries"
mkdir -p "${TOOLS_DIR}/hunspell/share/hunspell"
download \
  "https://raw.githubusercontent.com/wooorm/dictionaries/main/dictionaries/en-GB/index.dic" \
  "${TOOLS_DIR}/dictionaries/en_GB.dic" \
  || FAILED=$((FAILED + 1))
download \
  "https://raw.githubusercontent.com/wooorm/dictionaries/main/dictionaries/en-GB/index.aff" \
  "${TOOLS_DIR}/dictionaries/en_GB.aff" \
  || FAILED=$((FAILED + 1))
# Also place copies inside the pre-extracted hunspell dir
if [ -f "${TOOLS_DIR}/dictionaries/en_GB.dic" ]; then
  cp "${TOOLS_DIR}/dictionaries/en_GB.dic" "${TOOLS_DIR}/hunspell/share/hunspell/en_GB.dic" 2>/dev/null || true
  cp "${TOOLS_DIR}/dictionaries/en_GB.aff" "${TOOLS_DIR}/hunspell/share/hunspell/en_GB.aff" 2>/dev/null || true
fi

# ---------- 4. netcoredbg ----------
echo ""
echo "[4/17] netcoredbg"
download \
  "https://github.com/Samsung/netcoredbg/releases/download/3.2.0-1092/netcoredbg-win64.zip" \
  "${TOOLS_DIR}/archives/netcoredbg-win64.zip" \
  || FAILED=$((FAILED + 1))

# ---------- 5. ffmpeg ----------
echo ""
echo "[5/17] ffmpeg"
download \
  "https://www.gyan.dev/ffmpeg/builds/ffmpeg-release-essentials.zip" \
  "${TOOLS_DIR}/archives/ffmpeg-release-essentials.zip" \
  || FAILED=$((FAILED + 1))

# ---------- 6. ImageMagick (7z; pre-extracted to avoid 7z dep on Windows) ----------
echo ""
echo "[6/17] ImageMagick (7z)"
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
echo "[7/17] CMake"
download \
  "https://github.com/Kitware/CMake/releases/download/v4.2.0/cmake-4.2.0-windows-x86_64.zip" \
  "${TOOLS_DIR}/archives/cmake-4.2.0-windows-x86_64.zip" \
  || FAILED=$((FAILED + 1))

# ---------- 8. Clang/LLVM ----------
echo ""
echo "[8/17] Clang/LLVM"
download \
  "https://github.com/llvm/llvm-project/releases/download/llvmorg-20.1.0/LLVM-20.1.0-win64.exe" \
  "${TOOLS_DIR}/archives/LLVM-20.1.0-win64.exe" \
  || FAILED=$((FAILED + 1))

# ---------- 9. ada_language_server ----------
echo ""
echo "[9/17] Ada Language Server"
download \
  "https://github.com/AdaCore/ada_language_server/releases/download/2026.3.202607051/als-2026.3.202607051-win32-x64.tar.gz" \
  "${TOOLS_DIR}/archives/als-2026.3.202607051-win32-x64.tar.gz" \
  || FAILED=$((FAILED + 1))

# ---------- 10. csharp-ls ----------
echo ""
echo "[10/17] csharp-ls (C# Language Server)"
# Download the NuGet package directly
download \
  "https://www.nuget.org/api/v2/package/csharp-ls/0.25.0" \
  "${TOOLS_DIR}/archives/csharp-ls-0.25.0.nupkg" \
  || FAILED=$((FAILED + 1))
# Pre-extract on Linux so Windows install is a plain file copy
if [ -f "${TOOLS_DIR}/archives/csharp-ls-0.25.0.nupkg" ]; then
  echo "   Pre-extracting csharp-ls for offline install..."
  mkdir -p "${TOOLS_DIR}/csharp-ls"
  if command -v unzip &>/dev/null; then
    unzip -q "${TOOLS_DIR}/archives/csharp-ls-0.25.0.nupkg" -d "${TOOLS_DIR}/csharp-ls"
  else
    echo "   FAILED: unzip not available"
    FAILED=$((FAILED + 1))
  fi
  echo "   Pre-extracted."
fi

# ---------- 11. buf ----------
echo ""
echo "[11/17] buf (protobuf)"
download \
  "https://github.com/bufbuild/buf/releases/download/v1.50.0/buf-Windows-x86_64.exe" \
  "${TOOLS_DIR}/archives/buf-Windows-x86_64.exe" \
  || FAILED=$((FAILED + 1))

# ---------- 12. protoc ----------
echo ""
echo "[12/17] protoc"
download \
  "https://github.com/protocolbuffers/protobuf/releases/download/v31.1/protoc-31.1-win64.zip" \
  "${TOOLS_DIR}/archives/protoc-31.1-win64.zip" \
  || FAILED=$((FAILED + 1))

# ---------- 13. JDTLS (just the Windows launcher + config) ----------
echo ""
echo "[13/17] JDTLS (Eclipse JDT Language Server — Windows launcher)"
# The full JDTLS tarball includes the launcher; the JARs are already in the
# Linux toolkit tools/ and work cross-platform.
download \
  "https://download.eclipse.org/jdtls/snapshots/jdt-language-server-latest.tar.gz" \
  "${TOOLS_DIR}/archives/jdt-language-server-latest.tar.gz" \
  || FAILED=$((FAILED + 1))

# ---------- 14. kotlin-language-server (Windows launcher) ----------
echo ""
echo "[14/17] Kotlin Language Server (Windows launcher)"
# The JARs are cross-platform and already in the Linux toolkit tools/;
# download the full release for the .bat launcher.
download \
  "https://github.com/fwcd/kotlin-language-server/releases/download/1.3.13/server.zip" \
  "${TOOLS_DIR}/archives/kotlin-language-server-server.zip" \
  || FAILED=$((FAILED + 1))

# ---------- 15. Apache-Subversion ----------
echo ""
echo "[15/17] Apache-Subversion (svn)"
download \
  "https://sliksvn.com/pub/Slik-Subversion-1.14.5-x64.zip" \
  "${TOOLS_DIR}/archives/Slik-Subversion-1.14.5-x64.zip" \
  || FAILED=$((FAILED + 1))
# Pre-extract from the MSI inside the zip so Windows install is a plain file copy
if [ -f "${TOOLS_DIR}/archives/Slik-Subversion-1.14.5-x64.zip" ]; then
  echo "   Pre-extracting svn for offline install..."
  mkdir -p "${TOOLS_DIR}/svn/bin"
  svn_tmp="$(mktemp -d)"
  unzip -o "${TOOLS_DIR}/archives/Slik-Subversion-1.14.5-x64.zip" \
    -d "$svn_tmp" >/dev/null 2>&1
  7z x -y "$svn_tmp/Slik-Subversion-1.14.5-x64.msi" \
    -o"${TOOLS_DIR}/svn/bin" >/dev/null
  rm -rf "$svn_tmp"
  echo "   Pre-extracted."
fi

# ---------- 16. omnisharp-win-x64 ----------
echo ""
echo "[16/17] omnisharp-win-x64 (OmniSharp C# LSP)"
download \
  "https://github.com/OmniSharp/omnisharp-roslyn/releases/download/v1.39.11/omnisharp-win-x64.zip" \
  "${TOOLS_DIR}/archives/omnisharp-win-x64.zip" \
  || FAILED=$((FAILED + 1))

# ---------- 17. exiftool ----------
echo ""
echo "[17/17] exiftool"
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
  echo [1/17] PortableGit...
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
  echo [2/17] ripgrep...
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
  echo [3/17] Hunspell...
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
  :: Install pre-bundled English dictionary (no internet required)
  if not exist "%ROOT%\hunspell\share\hunspell" mkdir "%ROOT%\hunspell\share\hunspell"
  set "DICT_BUNDLE=%HERE%tools\dictionaries"
  if not exist "%ROOT%\hunspell\share\hunspell\en_GB.dic" (
    if exist "!DICT_BUNDLE!\en_GB.dic" (
      copy /Y "!DICT_BUNDLE!\en_GB.dic" "%ROOT%\hunspell\share\hunspell\en_GB.dic" >nul
      copy /Y "!DICT_BUNDLE!\en_GB.aff" "%ROOT%\hunspell\share\hunspell\en_GB.aff" >nul
      echo    Dictionaries copied from bundle.
    ) else (
      echo    WARNING: No bundled dictionary found.
    )
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
  echo [4/17] netcoredbg...
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
  echo [5/17] ffmpeg...
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
  echo [6/17] ImageMagick...
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
  echo [7/17] CMake...
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
  echo [8/17] Clang/LLVM...
  if exist "%ARCHIVES%\LLVM-20.1.0-win64.exe" (
    mkdir "%ROOT%\clang" 2>nul
    "%ARCHIVES%\LLVM-20.1.0-win64.exe" /S /D=%ROOT%\clang
    copy nul "%ROOT%\clang\.done" >nul
    echo    Installed.
  ) else (
    echo    Skipped ^(archive not found^).
  )
:skip_clang

:install_ada
if exist "%ROOT%\ada_language_server\.done" goto :skip_ada
  echo [9/17] Ada Language Server...
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

:install_csharpls
if exist "%ROOT%\csharp-ls\.done" goto :skip_csharpls
  echo [10/17] csharp-ls (C# Language Server)...
  set "CSLS_PREEXTRACTED=%HERE%tools\csharp-ls"
  if exist "!CSLS_PREEXTRACTED!" (
    xcopy /E /I /Y "!CSLS_PREEXTRACTED!" "%ROOT%\csharp-ls" >nul
    copy nul "%ROOT%\csharp-ls\.done" >nul
    echo    Installed ^(from pre-extracted^).
  ) else (
    echo    Skipped ^(no pre-extracted dir found^).
  )
  echo    Note: .NET SDK required to run csharp-ls
:skip_csharpls

:install_buf
if exist "%ROOT%\buf\.done" goto :skip_buf
  echo [11/17] buf...
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
  echo [12/17] protoc...
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
echo [13/17] JDTLS...
if exist "%ARCHIVES%\jdt-language-server-latest.tar.gz" (
  mkdir "%ROOT%\jdtls" 2>nul
  tar -xzf "%ARCHIVES%\jdt-language-server-latest.tar.gz" -C "%ROOT%\jdtls" --strip-components=1
  echo    Installed.
) else (
  echo    Skipped ^(archive not found^).
)
:skip_jdtls

:install_kotlin
echo [14/17] Kotlin Language Server...
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
  echo [15/17] Apache-Subversion (svn)...
  set "SVN_PREEXTRACTED=%HERE%tools\svn"
  if exist "!SVN_PREEXTRACTED!" (
    xcopy /E /I /Y "!SVN_PREEXTRACTED!" "%ROOT%\svn" >nul
  ) else if exist "%ARCHIVES%\Slik-Subversion-1.14.5-x64.zip" (
    mkdir "%ROOT%\svn" 2>nul
    mkdir "%TMP%\svn-extract" 2>nul
    tar -xf "%ARCHIVES%\Slik-Subversion-1.14.5-x64.zip" -C "%TMP%\svn-extract"
    if exist "%TMP%\svn-extract\Slik-Subversion-1.14.5-x64.msi" (
      mkdir "%ROOT%\svn\bin" 2>nul
      msiexec /a "%TMP%\svn-extract\Slik-Subversion-1.14.5-x64.msi" /qn TARGETDIR="%ROOT%\svn\bin"
    )
    rmdir /s /q "%TMP%\svn-extract" 2>nul
  ) else (
    echo    Skipped ^(no pre-extracted dir or archive found^).
    goto :skip_svn
  )
  copy nul "%ROOT%\svn\.done" >nul
  echo    Installed.
:skip_svn

:install_omnisharp
if exist "%ROOT%\omnisharp\.done" goto :skip_omnisharp
  echo [16/17] omnisharp-win-x64 (OmniSharp C# LSP)...
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
  echo [17/17] exiftool...
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
