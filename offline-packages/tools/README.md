# tools/ — non-Emacs binaries for the offline toolkit

Gitignored drop-zone. Contents are copied into each toolkit tarball by
`build-toolkit.sh`, then installed to `~/.emacs.d/bin/` on the target by
`setup.sh`. `starters/coding.el` detects binaries here and wires them into
eglot / dape automatically.

## Bundled (Linux x86_64)

| Subdir | Size | Provenance | Used by |
|--------|------|------------|---------|
| `jdtls/` | ~56 MB | https://download.eclipse.org/jdtls/snapshots/jdt-language-server-latest.tar.gz | eglot (java-mode) |
| `jdtls/com.microsoft.java.debug.plugin-0.53.1.jar` | ~3 MB | Maven Central: `com.microsoft.java:com.microsoft.java.debug.plugin` | dape (Java debugging via JDTLS) |
| `netcoredbg/` | ~9 MB | https://github.com/Samsung/netcoredbg/releases/latest (linux-amd64) | dape (C#/.NET) |
| `ada_language_server/` | ~101 MB | https://github.com/AdaCore/ada_language_server/releases/latest (linux-x64) | eglot (ada-mode). Debug symbols (`*.debug`) dropped. |
| `buf/bin/buf` | ~54 MB | https://github.com/bufbuild/buf/releases/latest (buf-Linux-x86_64) | eglot (protobuf-mode via `buf lsp serve`). Doubles as the `buf` CLI. |
| `csharp-ls/` | ~106 MB | `dotnet tool install csharp-ls --tool-path tools/csharp-ls` | eglot (csharp-mode). Requires .NET 10 runtime on target (check `dotnet --list-runtimes`). |
| `kotlin-language-server/` | ~90 MB | https://github.com/fwcd/kotlin-language-server/releases/latest (`server.zip`) | eglot (kotlin-mode). Requires a JRE on the target (`java --version`). Windows `.bat` launcher dropped. |
| `npm/typescript-*.tgz` | ~5 MB | `npm pack typescript typescript-language-server` | `npm install -g` on the target |

Total ~420 MB.

## Re-fetching

Delete any subdir and re-download with the URLs above. The coding starter's
detection is path-based (`file-executable-p`) so version changes within a
subdir don't require starter edits. The java-debug JAR is matched by glob
(`com.microsoft.java.debug.plugin-*.jar`) so dropping a newer one alongside
is fine.

## What's not here (and why)

- **clangd, cmake, ninja, node, dotnet, protoc** — typically installed from
  the target distro's offline package repos. Bundling them would duplicate
  the distro package and lock to one Linux flavour.
- **vscode-js-debug** — requires `npm install && npm run compile`; ship the
  compiled `out/` tree from the online build machine if you want it.

## Windows support

These binaries are Linux x86_64. On Windows, use `setup-windows-tools.bat` (in this
directory) to download Windows-native equivalents. Tools that are Java-based
(JDTLS, kotlin-language-server) work cross-platform using the bundled JARs;
the script handles platform-specific launchers for the rest.

| Tool | Windows status | Provides |
|------|----------------|----------|
| `jdtls/` | Java JARs bundled work cross-platform; need `.bat` launcher | eglot (java-mode) |
| `kotlin-language-server/` | Java JARs bundled work cross-platform; need `.bat` launcher | eglot (kotlin-mode) |
| `netcoredbg/` | Downloaded by `setup-windows-tools.bat` as `win64` ZIP | dape (C#/.NET debugger) |
| `ada_language_server/` | Downloaded by `setup-windows-tools.bat` as `win64` ZIP | eglot (ada-mode) |
| `buf/` | Downloaded by `setup-windows-tools.bat` as `Windows-x86_64.exe` | eglot (protobuf-mode) |
| `csharp-ls/` | Installed via `dotnet tool install --global csharp-ls` | eglot (csharp-mode) |
| `svn/` | Downloaded by `setup-windows-tools.bat` as SlikSVN ZIP | `svn` CLI version control |
| `omnisharp/` | Downloaded by `setup-windows-tools.bat` as `omnisharp-win-x64.zip` | eglot (csharp-mode) alternative |
| `exiftool/` | Downloaded by `setup-windows-tools.bat` as ZIP | Image metadata editing (`exiftool`) |
| `npm/` | `.tgz` files are platform-independent | `npm install -g` |

Run the script from the extracted toolkit directory after `setup.bat`:
```
setup-windows-tools.bat
```

## Layout expected by `starters/coding.el`

```
~/.emacs.d/bin/
├── jdtls/
│   ├── bin/jdtls                              ← eglot-server-programs entry
│   └── com.microsoft.java.debug.plugin-*.jar  ← java-debug bundle (glob)
├── netcoredbg/netcoredbg                      ← dape netcoredbg-launch
├── ada_language_server/bin/ada_language_server ← eglot-server-programs entry
├── buf/bin/buf                                 ← eglot (protobuf-mode): `buf lsp serve`
├── csharp-ls/csharp-ls                         ← eglot (csharp-mode). Requires .NET 10 runtime.
├── svn/bin/svn                                 ← svn CLI
├── omnisharp/omnisharp                         ← eglot (csharp-mode) alternative
├── exiftool/exiftool                           ← Image metadata editing
└── kotlin-language-server/bin/kotlin-language-server ← eglot (kotlin-mode). Requires JRE.
```
