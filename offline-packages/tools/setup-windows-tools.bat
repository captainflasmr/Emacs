@echo off
:: setup-windows-tools.bat — download all Windows-native tool binaries for Emacs
::
:: Mirrors the tool set from ~/source/repos/Emacs-on-windows:
::   PortableGit (coreutils: bash, find, grep, sed, awk, etc.)
::   csharp-ls, netcoredbg, ripgrep, ffmpeg, ImageMagick
::   JDTLS, kotlin-language-server, ada_language_server, buf
::   hunspell, cmake, clang, Pandoc, protoc, svn2git
::   Apache-Subversion (svn), omnisharp-win-x64, exiftool
::
:: Run AFTER setup.bat.  Installs into %APPDATA%\.emacs.d\bin\ by default.
:: (matching Windows init.el `~/bin/...` paths).  Pass a different root:
::   setup-windows-tools.bat D:\tools
::
:: Requires: curl (or PowerShell), ~1 GB free, admin rights for some MSI installers.
:: Prerequisites you install separately: Java 21+, .NET 10+, Node.js, Visual Studio.
setlocal enabledelayedexpansion

:: Ensure System32 and common Windows binary locations are on PATH.
set "WIN_PATH=%SystemRoot%\System32;%SystemRoot%;%SystemRoot%\System32\Wbem;%SystemRoot%\System32\WindowsPowerShell\v1.0\;%SystemRoot%\System32\OpenSSH\"
set "PATH=%WIN_PATH%;%PATH%"

set ROOT=%~1
if "%ROOT%"=="" set ROOT=%APPDATA%\.emacs.d\bin
if not exist "%ROOT%" mkdir "%ROOT%"

:: Helper: download with curl or PowerShell
set HAVE_CURL=0
where curl >nul 2>nul
if !errorlevel! equ 0 set HAVE_CURL=1

echo.
echo =================================================================
echo Windows Emacs Tools Installer
echo =================================================================
echo Installing to: %ROOT%
echo.
echo Tools will be placed in subdirectories under %ROOT%
echo (e.g. %ROOT%\PortableGit, %ROOT%\find, %ROOT%\cmake, etc.)
echo.

:: ============================================================
:: 1. PortableGit — Git + coreutils (bash, find, grep, sed, etc.)
:: ============================================================
echo [1/16] PortableGit (Git + coreutils)...
set GIT_DIR=%ROOT%\PortableGit
if exist "!GIT_DIR!\.done" goto ALREADY_GIT
  if exist "!GIT_DIR!" rmdir /s /q "!GIT_DIR!"
  set GITEXE=PortableGit-2.50.0-64-bit.7z.exe
  set GITURL=https://github.com/git-for-windows/git/releases/download/v2.50.0.windows.1/PortableGit-2.50.0-64-bit.7z.exe
  echo    Downloading from !GITURL!...
  call :download "!GITURL!" "%TMP%\!GITEXE!"
  if not exist "%TMP%\!GITEXE!" goto FAIL_GIT
  echo    Extracting (this may take a minute)...
  "%TMP%\!GITEXE!" -o"!GIT_DIR!" -y >nul
  del "%TMP%\!GITEXE!"
  copy nul "!GIT_DIR!\.done" >nul
  echo    PortableGit installed.
  goto END_GIT
:FAIL_GIT
  echo    Download failed. Install manually from https://git-scm.com/download/win
  goto END_GIT
:ALREADY_GIT
  echo    Already installed. Delete !GIT_DIR!\.done to reinstall.
:END_GIT

:: ============================================================
:: 2. ripgrep (rg) — fast code search in ~/bin/find/
:: ============================================================
echo [2/16] ripgrep (rg.exe)...
set FIND_DIR=%ROOT%\find
if not exist "!FIND_DIR!" mkdir "!FIND_DIR!"
set RGZIP=ripgrep-14.1.1-x86_64-pc-windows-gnu.zip
set RGURL=https://github.com/BurntSushi/ripgrep/releases/download/14.1.1/!RGZIP!
if exist "!FIND_DIR!\rg.exe" goto ALREADY_RG
  echo    Downloading from !RGURL!...
  call :download "!RGURL!" "%TMP%\!RGZIP!"
  if not exist "%TMP%\!RGZIP!" goto FAIL_RG
  tar -xf "%TMP%\!RGZIP!" -C "%TMP%" --strip-components=1
  if exist "%TMP%\rg.exe" (
    move "%TMP%\rg.exe" "!FIND_DIR!\rg.exe" >nul
    copy nul "!FIND_DIR!\.done" >nul
  )
  del "%TMP%\!RGZIP!"
  echo    ripgrep installed.
  goto END_RG
:FAIL_RG
  echo    Download failed. Install from https://github.com/BurntSushi/ripgrep/releases
  goto END_RG
:ALREADY_RG
  echo    Already installed.
:END_RG

:: ============================================================
:: 3. Hunspell — spell checking dictionaries + binaries
:: ============================================================
echo [3/16] Hunspell (spell checker)...
set HUN_DIR=%ROOT%\hunspell
if exist "!HUN_DIR!\.done" goto ALREADY_HUN
  if exist "!HUN_DIR!" rmdir /s /q "!HUN_DIR!"
  mkdir "!HUN_DIR!"
  set HUN7Z=hunspell-v1.7.3.7z
  set HUNURL=https://github.com/iquiw/hunspell-binary/releases/download/v1.7.3/!HUN7Z!
  echo    Downloading binaries from !HUNURL!...
  call :download "!HUNURL!" "%TMP%\!HUN7Z!"
  if exist "%TMP%\!HUN7Z!" (
    tar -xf "%TMP%\!HUN7Z!" -C "!HUN_DIR!" --strip-components=1
    del "%TMP%\!HUN7Z!" 2>nul
    echo    Binaries installed.
  ) else (
    echo    Binary download failed. Install manually from:
    echo    https://github.com/iquiw/hunspell-binary/releases
  )
  :: Download English dictionary
  pushd "!HUN_DIR!"
  if not exist "share\hunspell" mkdir "share\hunspell"
  call :download "https://raw.githubusercontent.com/wooorm/dictionaries/main/dictionaries/en-GB/index.dic" "share\hunspell\en_GB.dic"
  call :download "https://raw.githubusercontent.com/wooorm/dictionaries/main/dictionaries/en-GB/index.aff" "share\hunspell\en_GB.aff"
  popd
  :: Copy dictionaries to C:\Hunspell\ (in hunspell's built-in search path)
  if not exist "C:\Hunspell" mkdir "C:\Hunspell"
  if exist "!HUN_DIR!\share\hunspell\en_GB.dic" (
    copy /Y "!HUN_DIR!\share\hunspell\en_GB.dic" "C:\Hunspell\en_GB.dic"
    copy /Y "!HUN_DIR!\share\hunspell\en_GB.aff" "C:\Hunspell\en_GB.aff"
    copy /Y "!HUN_DIR!\share\hunspell\en_GB.dic" "C:\Hunspell\default.dic"
    copy /Y "!HUN_DIR!\share\hunspell\en_GB.aff" "C:\Hunspell\default.aff"
  )
  copy nul "!HUN_DIR!\.done" >nul
  echo    Hunspell installed.
  goto END_HUN
:ALREADY_HUN
  echo    Already installed.
:END_HUN

:: ============================================================
:: 4. netcoredbg — .NET debugger for dape
:: ============================================================
echo [4/16] netcoredbg (.NET debugger)...
set NC_DIR=%ROOT%\netcoredbg
if exist "!NC_DIR!\.done" goto ALREADY_NC
  if exist "!NC_DIR!" rmdir /s /q "!NC_DIR!"
  mkdir "!NC_DIR!"
  pushd "!NC_DIR!"
  set NCURL=https://github.com/Samsung/netcoredbg/releases/download/3.2.0-1092/netcoredbg-win64.zip
  echo    Downloading from !NCURL!...
  call :download "!NCURL!" "netcoredbg.zip"
  if not exist netcoredbg.zip goto NC_SKIP
  tar -xf netcoredbg.zip
  del netcoredbg.zip
:NC_SKIP
  copy nul .done >nul
  popd
  echo    netcoredbg installed.
  goto END_NC
:ALREADY_NC
  echo    Already installed.
:END_NC

:: ============================================================
:: 5. csharp-ls — C# Language Server (via dotnet tool)
:: ============================================================
echo [5/16] csharp-ls (C# Language Server)...
where dotnet >nul 2>nul
if !errorlevel! neq 0 goto NO_DOTNET
  mkdir "%ROOT%\csharp-ls" 2>nul
  pushd "%ROOT%\csharp-ls"
  dotnet tool install csharp-ls --tool-path "%ROOT%\csharp-ls" 2>nul
  if exist "tools\net9.0\any\CSharpLanguageServer.dll" goto LS_INSTALLED
    echo    Trying global install...
    dotnet tool install --global csharp-ls 2>nul || echo    Already installed globally.
  goto LS_DONE
:LS_INSTALLED
  echo    csharp-ls installed locally.
:LS_DONE
  popd
  goto END_CS
:NO_DOTNET
  echo    .NET SDK not found. Install from https://dotnet.microsoft.com/download
  echo    Then run: dotnet tool install --global csharp-ls
:END_CS

:: ============================================================
:: 6. ffmpeg — video processing
:: ============================================================
echo [6/16] ffmpeg (video processing)...
set FF_DIR=%ROOT%\ffmpeg-7.1.1-essentials_build
if exist "!FF_DIR!\.done" goto ALREADY_FF
  if exist "!FF_DIR!" rmdir /s /q "!FF_DIR!"
  set FFZIP=ffmpeg-release-essentials.zip
  set FFURL=https://www.gyan.dev/ffmpeg/builds/!FFZIP!
  echo    Downloading from !FFURL!...
  call :download "!FFURL!" "%TMP%\!FFZIP!"
  if not exist "%TMP%\!FFZIP!" goto FAIL_FF
  tar -xf "%TMP%\!FFZIP!" -C "%ROOT%"
  for /d %%d in ("%ROOT%\ffmpeg-*-essentials_build") do (
    move "%%d" "!FF_DIR!" >nul 2>nul
  )
  del "%TMP%\!FFZIP!"
  copy nul "!FF_DIR!\.done" >nul
  echo    ffmpeg installed.
  goto END_FF
:FAIL_FF
  echo    Download failed. Install from https://ffmpeg.org/download.html
  mkdir "!FF_DIR!"
  copy nul "!FF_DIR!\.skip" >nul
  goto END_FF
:ALREADY_FF
  echo    Already installed.
:END_FF

:: ============================================================
:: 7. ImageMagick — image processing (for image-dired)
::    Downloaded as .7z and extracted by Windows tar.
:: ============================================================
echo [7/16] ImageMagick (image processing)...
set IM_DIR=%ROOT%\ImageMagick-7.1.2-27-portable-Q16-x64
if exist "!IM_DIR!\.done" goto ALREADY_IM
  if exist "!IM_DIR!" rmdir /s /q "!IM_DIR!"
  set IMZIP=ImageMagick-7.1.2-27-portable-Q16-x64.7z
  set IMURL=https://github.com/ImageMagick/ImageMagick/releases/download/7.1.2-27/!IMZIP!
  echo    Downloading from !IMURL!...
  call :download "!IMURL!" "%TMP%\!IMZIP!"
  if not exist "%TMP%\!IMZIP!" goto FAIL_IM
  tar -xf "%TMP%\!IMZIP!" -C "%ROOT%"
  del "%TMP%\!IMZIP!" 2>nul
  if exist "!IM_DIR!" (
    copy nul "!IM_DIR!\.done" >nul
    echo    ImageMagick installed.
  ) else (
    echo    Failed to extract (extract .7z). Install manually from https://imagemagick.org/script/download.php
  )
  goto END_IM
:FAIL_IM
  echo    Download failed. Install from https://imagemagick.org/script/download.php
  goto END_IM
:ALREADY_IM
  echo    Already installed.
:END_IM

:: ============================================================
:: 8. CMake — build tool
:: ============================================================
echo [8/16] CMake (build tool)...
set CMAKE_DIR=%ROOT%\cmake
if exist "!CMAKE_DIR!\.done" goto ALREADY_CMAKE
  if exist "!CMAKE_DIR!" rmdir /s /q "!CMAKE_DIR!"
  set CMAKEZIP=cmake-4.2.0-windows-x86_64.zip
  set CMAKEURL=https://github.com/Kitware/CMake/releases/download/v4.2.0/!CMAKEZIP!
  echo    Downloading from !CMAKEURL!...
  call :download "!CMAKEURL!" "%TMP%\!CMAKEZIP!"
  if not exist "%TMP%\!CMAKEZIP!" goto FAIL_CMAKE
  mkdir "!CMAKE_DIR!"
  tar -xf "%TMP%\!CMAKEZIP!" -C "%TMP%"
  xcopy /E /I /Y "%TMP%\cmake-4.2.0-windows-x86_64\bin" "!CMAKE_DIR!\bin\" >nul 2>nul
  rmdir /s /q "%TMP%\cmake-4.2.0-windows-x86_64" 2>nul
  del "%TMP%\!CMAKEZIP!"
  copy nul "!CMAKE_DIR!\.done" >nul
  echo    CMake installed.
  goto END_CMAKE
:FAIL_CMAKE
  echo    Download failed. Install from https://cmake.org/download/
  goto END_CMAKE
:ALREADY_CMAKE
  echo    Already installed.
:END_CMAKE

:: ============================================================
:: 9. Clang — C/C++ language server + compiler
:: ============================================================
echo [9/16] Clang (C/C++ toolchain)...
set CLANG_DIR=%ROOT%\clang
if exist "!CLANG_DIR!\.done" goto ALREADY_CLANG
  if exist "!CLANG_DIR!" rmdir /s /q "!CLANG_DIR!"
  set CLANGZIP=LLVM-20.1.0-win64.exe
  set CLANGURL=https://github.com/llvm/llvm-project/releases/download/llvmorg-20.1.0/LLVM-20.1.0-win64.exe
  echo    Downloading from !CLANGURL!...
  call :download "!CLANGURL!" "%TMP%\!CLANGZIP!"
  if not exist "%TMP%\!CLANGZIP!" goto FAIL_CLANG
  echo    Running LLVM installer (may need admin rights)...
  mkdir "!CLANG_DIR!" 2>nul
  "%TMP%\!CLANGZIP!" /S /D="!CLANG_DIR!"
  del "%TMP%\!CLANGZIP!"
  copy nul "!CLANG_DIR!\.done" >nul
  echo    Clang/LLVM installed.
  goto END_CLANG
:FAIL_CLANG
  echo    Download failed. Install from https://github.com/llvm/llvm-project/releases
  goto END_CLANG
:ALREADY_CLANG
  echo    Already installed.
:END_CLANG

:: ============================================================
:: 10. JDTLS + kotlin-language-server — Java-based servers
::     Already bundled in toolkit tools/ — copy them over
:: ============================================================
echo [10/16] JDTLS + kotlin-language-server (Java-based)...
set TOOLKIT_TOOLS=%~dp0..\..\tools
if exist "!TOOLKIT_TOOLS!\jdtls" (
  xcopy /E /I /Y "!TOOLKIT_TOOLS!\jdtls" "%ROOT%\.emacs.d\bin\jdtls\" >nul 2>nul
)
if exist "!TOOLKIT_TOOLS!\kotlin-language-server" (
  xcopy /E /I /Y "!TOOLKIT_TOOLS!\kotlin-language-server" "%ROOT%\.emacs.d\bin\kotlin-language-server\" >nul 2>nul
)
if exist "!TOOLKIT_TOOLS!\npm" (
  xcopy /E /I /Y "!TOOLKIT_TOOLS!\npm" "%ROOT%\.emacs.d\bin\npm\" >nul 2>nul
)
echo    Java-based tools: copy bundled JARs if found, otherwise:
echo      JDTLS: https://download.eclipse.org/jdtls/snapshots/
echo      kotlin-ls: https://github.com/fwcd/kotlin-language-server/releases

:: ============================================================
:: 11. ada_language_server
:: ============================================================
echo [11/16] ada_language_server...
if exist "%ROOT%\ada_language_server\.done" goto ALREADY_ALS
  set ALSZIP=als-2026.3.202607051-win32-x64.tar.gz
  set ALSURL=https://github.com/AdaCore/ada_language_server/releases/download/2026.3.202607051/!ALSZIP!
  mkdir "%ROOT%\ada_language_server" 2>nul
  pushd "%ROOT%\ada_language_server"
  echo    Downloading from !ALSURL!...
  call :download "!ALSURL!" "als.zip"
  if not exist als.zip goto ALS_SKIP
  tar -xf als.zip
  del als.zip
  if exist "*.debug" del *.debug
  copy nul .done >nul
  echo    ada_language_server installed.
:ALS_SKIP
  popd
  goto END_ALS
:ALREADY_ALS
  echo    Already installed.
:END_ALS

:: ============================================================
:: 12. buf — Protocol Buffers CLI+LSP
:: ============================================================
echo [12/16] buf (protobuf CLI+LSP)...
if exist "%ROOT%\buf\.done" goto ALREADY_BUF
  mkdir "%ROOT%\buf" 2>nul
  pushd "%ROOT%\buf"
  set BUFURL=https://github.com/bufbuild/buf/releases/download/v1.50.0/buf-Windows-x86_64.exe
  echo    Downloading from !BUFURL!...
  call :download "!BUFURL!" "buf.exe"
  if not exist buf.exe goto BUF_SKIP
  mkdir bin 2>nul
  copy buf.exe bin\buf.exe >nul
  copy nul .done >nul
  echo    buf installed.
:BUF_SKIP
  popd
  goto END_BUF
:ALREADY_BUF
  echo    Already installed.
:END_BUF

:: ============================================================
:: 13. protoc — Protocol Buffers compiler
:: ============================================================
echo [13/16] protoc (Protocol Buffers compiler)...
set PROTOC_DIR=%ROOT%\protoc
if exist "!PROTOC_DIR!\.done" goto ALREADY_PROTOC
  if exist "!PROTOC_DIR!" rmdir /s /q "!PROTOC_DIR!"
  set PZIP=protoc-31.1-win64.zip
  set PURL=https://github.com/protocolbuffers/protobuf/releases/download/v31.1/!PZIP!
  echo    Downloading from !PURL!...
  call :download "!PURL!" "%TMP%\!PZIP!"
  if not exist "%TMP%\!PZIP!" goto FAIL_PROTOC
  mkdir "!PROTOC_DIR!"
  tar -xf "%TMP%\!PZIP!" -C "%TMP%" --strip-components=1
  if exist "%TMP%\bin\protoc.exe" move "%TMP%\bin\protoc.exe" "!PROTOC_DIR!\" >nul
  if exist "%TMP%\include" rmdir /s /q "%TMP%\include" 2>nul
  del "%TMP%\!PZIP!"
  copy nul "!PROTOC_DIR!\.done" >nul
  echo    protoc installed.
  goto END_PROTOC
:FAIL_PROTOC
  echo    Download failed. Install from https://github.com/protocolbuffers/protobuf/releases
  goto END_PROTOC
:ALREADY_PROTOC
  echo    Already installed.
:END_PROTOC

:: ============================================================
:: 14. Apache-Subversion (svn)
:: ============================================================
echo [14/16] Apache-Subversion (svn CLI)...
set SVN_DIR=%ROOT%\svn
if exist "!SVN_DIR!\.done" goto ALREADY_SVN
  if exist "!SVN_DIR!" rmdir /s /q "!SVN_DIR!"
  set SVNZIP=Slik-Subversion-1.14.5-x64.zip
  set SVNURL=https://sliksvn.com/pub/!SVNZIP!
  echo    Downloading from !SVNURL!...
  call :download "!SVNURL!" "%TMP%\!SVNZIP!"
  if not exist "%TMP%\!SVNZIP!" goto FAIL_SVN
  mkdir "!SVN_DIR!"
  mkdir "%TMP%\svn-extract"
  tar -xf "%TMP%\!SVNZIP!" -C "%TMP%\svn-extract"
  if exist "%TMP%\svn-extract\Slik-Subversion-1.14.5-x64.msi" (
    mkdir "!SVN_DIR!\bin"
    msiexec /a "%TMP%\svn-extract\Slik-Subversion-1.14.5-x64.msi" /qn TARGETDIR="!SVN_DIR!\bin"
    rmdir /s /q "%TMP%\svn-extract"
  )
  del "%TMP%\!SVNZIP!" 2>nul
  copy nul "!SVN_DIR!\.done" >nul
  echo    Apache-Subversion installed.
  goto END_SVN
:FAIL_SVN
  echo    Download failed. Install manually from https://sliksvn.com/
  goto END_SVN
:ALREADY_SVN
  echo    Already installed.
:END_SVN

:: ============================================================
:: 15. omnisharp-win-x64 — OmniSharp C# Language Server
:: ============================================================
echo [15/16] omnisharp-win-x64 (OmniSharp C# LSP)...
set OMNISHARP_DIR=%ROOT%\omnisharp
if exist "!OMNISHARP_DIR!\.done" goto ALREADY_OMNISHARP
  if exist "!OMNISHARP_DIR!" rmdir /s /q "!OMNISHARP_DIR!"
  set OMNIZIP=omnisharp-win-x64.zip
  set OMNIURL=https://github.com/OmniSharp/omnisharp-roslyn/releases/download/v1.39.11/!OMNIZIP!
  echo    Downloading from !OMNIURL!...
  call :download "!OMNIURL!" "%TMP%\!OMNIZIP!"
  if not exist "%TMP%\!OMNIZIP!" goto FAIL_OMNISHARP
  mkdir "!OMNISHARP_DIR!"
  tar -xf "%TMP%\!OMNIZIP!" -C "!OMNISHARP_DIR!" --strip-components=1
  del "%TMP%\!OMNIZIP!"
  copy nul "!OMNISHARP_DIR!\.done" >nul
  echo    omnisharp-win-x64 installed.
  goto END_OMNISHARP
:FAIL_OMNISHARP
  echo    Download failed. Install from https://github.com/OmniSharp/omnisharp-roslyn/releases
  goto END_OMNISHARP
:ALREADY_OMNISHARP
  echo    Already installed.
:END_OMNISHARP

:: ============================================================
:: 16. exiftool — Image metadata editor
:: ============================================================
echo [16/16] exiftool (image metadata editor)...
set EXIF_DIR=%ROOT%\exiftool
if exist "!EXIF_DIR!\.done" goto ALREADY_EXIF
  if exist "!EXIF_DIR!" rmdir /s /q "!EXIF_DIR!"
  set EXIFZIP=exiftool-13.59_64.zip
  set EXIFURL=https://sourceforge.net/projects/exiftool/files/!EXIFZIP!/download
  echo    Downloading from !EXIFURL!...
  call :download "!EXIFURL!" "%TMP%\!EXIFZIP!"
  if not exist "%TMP%\!EXIFZIP!" goto FAIL_EXIF
  mkdir "!EXIF_DIR!"
  tar -xf "%TMP%\!EXIFZIP!" -C "!EXIF_DIR!" --strip-components=1
  if exist "!EXIF_DIR!\exiftool(-k).exe" rename "!EXIF_DIR!\exiftool(-k).exe" exiftool.exe
  del "%TMP%\!EXIFZIP!"
  copy nul "!EXIF_DIR!\.done" >nul
  echo    exiftool installed.
  goto END_EXIF
:FAIL_EXIF
  echo    Download failed. Install from https://exiftool.org/
  goto END_EXIF
:ALREADY_EXIF
  echo    Already installed.
:END_EXIF

goto :main

:download
if "%HAVE_CURL%"=="1" (
  curl -fL# "%~1" -o "%~2"
) else (
  powershell -Command "Invoke-WebRequest -Uri '%~1' -OutFile '%~2'"
)
exit /b

:main

:: ============================================================
:: Extra: Pandoc — skipped, need manual install
:: ============================================================
echo.
echo =================================================================
echo Install these manually if needed:
echo =================================================================
echo   Pandoc:          https://pandoc.org/installing.html
echo   svn2git:         https://github.com/nirvdrum/svn2git
echo   GnuWin32:        https://gnuwin32.sourceforge.net/
echo   GNAT Ada:        https://www.adacore.com/download
echo   Java 21+:        https://adoptium.net/
echo   .NET 10+:        https://dotnet.microsoft.com/download
echo   Node.js:         https://nodejs.org/
echo   Visual Studio:   https://visualstudio.microsoft.com/
echo.
echo =================================================================
echo Installation summary
echo =================================================================
echo Tools installed to: %ROOT%
echo.
echo   PortableGit     -^> %ROOT%\PortableGit
echo   ripgrep         -^> %ROOT%\find\rg.exe
echo   Hunspell        -^> %ROOT%\hunspell
echo   netcoredbg      -^> %ROOT%\netcoredbg
echo   ffmpeg          -^> %ROOT%\ffmpeg-7.1.1-essentials_build\bin
echo   ImageMagick     -^> %ROOT%\ImageMagick-7.1.2-27-portable-Q16-x64
echo   CMake           -^> %ROOT%\cmake\bin
echo   Clang/LLVM      -^> %ROOT%\clang\bin
echo   ada_ls          -^> %ROOT%\ada_language_server
echo   buf             -^> %ROOT%\buf
echo   protoc          -^> %ROOT%\protoc
echo   csharp-ls       -^> %ROOT%\csharp-ls  (if .NET SDK available)
echo   svn             -^> %ROOT%\svn\bin
echo   omnisharp       -^> %ROOT%\omnisharp
echo   exiftool        -^> %ROOT%\exiftool
echo.
echo =================================================================
echo PATH setup for Emacs (add to your Windows init.el):
echo =================================================================
echo (when (eq system-type 'windows-nt^)
echo   (let* ((bin-root "%ROOT:\=\\%"^)^)
echo          (xPaths
echo           `(,bin-root
echo             ,(concat bin-root "/PortableGit/bin"^)
echo             ,(concat bin-root "/PortableGit/usr/bin"^)
echo             ,(concat bin-root "/hunspell/bin"^)
echo             ,(concat bin-root "/find"^)
echo             ,(concat bin-root "/netcoredbg"^)
echo             ,(concat bin-root "/csharp-ls"^)
echo             ,(concat bin-root "/ffmpeg-7.1.1-essentials_build/bin"^)
echo             ,(concat bin-root "/ImageMagick-7.1.2-27-portable-Q16-x64"^)
echo             ,(concat bin-root "/clang/bin"^)
echo             ,(concat bin-root "/cmake"^)
echo             ,(concat bin-root "/protoc"^)
echo             ,(concat bin-root "/ada_language_server"^)
echo             ,(concat bin-root "/buf"^)echo             ,(concat bin-root "/svn/bin"^)echo             ,(concat bin-root "/omnisharp"^)echo             ,(concat bin-root "/exiftool"^)
echo             "C:/Program Files/Pandoc")))
echo     (setenv "PATH" (concat (mapconcat 'identity xPaths ";") ";" (getenv "PATH"^)^))
echo     (setq exec-path (append xPaths (parse-colon-path (getenv "PATH"^)^)^)^))
echo.
echo =================================================================
echo Windows tools setup complete!
echo =================================================================
echo.
pause
