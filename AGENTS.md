# AGENTS.md — Emacs Configuration Repository

## Build/Test/Lint Commands

**Syntax check single file:**
```bash
emacs -batch -l /home/jdyer/.emacs.d/init.el -f batch-byte-compile <file.el>
```

**Byte-compile all Elisp files:**
```bash
emacs -batch -l /home/jdyer/.emacs.d/init.el -L . -f batch-byte-compile *.el
```

**Check for package-lint issues:**
```bash
emacs -batch -f package-lint-batch-and-exit <file.el>
```

**Load and validate configuration:**
```bash
emacs -q -l /home/jdyer/.emacs.d/init.el --eval '(message "Config loaded successfully")'
```

## Code Style Guidelines

### Elisp Format & Structure
- **Lexical binding:** Always include `; -*- lexical-binding: t; -*-` at file top
- **Line length:** Keep lines ≤100 characters where practical
- **Indentation:** 2 spaces; use `indent-region` for formatting
- **Comments:** Section headers wrapped in `;;` with `-> section-name` format

### Imports & Dependencies
- Use `use-package` macro with `:ensure t` for package management
- Load core modules with `require` at file start (see Emacs-vanilla/init.el)
- Group related `use-package` blocks by feature with comment headers

### Naming Conventions
- Functions: `my-function-name` (personal functions), `pkg/function-name` (packages)
- Variables: `my-var-name` (personal), `pkg-var-name` (packages)
- Keymaps: `my-<feature>-keymap`
- Predicates (booleans): `<name>-p` suffix (e.g., `my-check-p`)

### Error Handling
- Use `condition-case` for recoverable errors; `ignore-errors` for non-critical failures
- Provide fallback behavior (e.g., check feature availability before use)
- Log issues with `message` or `warn` for debugging

### Best Practices
- Keep init.el modular; use separate files in subdirectories (e.g., Emacs-vanilla/)
- Document complex logic with docstrings: `(defun name (args) "Docstring." ...)`
- Test configurations incrementally; use `eval-last-sexp` (C-x C-e) during development
