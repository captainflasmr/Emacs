;; -*- lexical-binding: t; -*-
;;
;; coding.el — language-agnostic coding companion.
;;
;; Distilled from docs/setting-up-emacs-for-coding/README.org (Part 15).
;; Optional, not auto-loaded. To try it, append to ~/.emacs.d/init.el:
;;   (load (expand-file-name "init-starter-coding" user-emacs-directory) t t)
;;
;; Built-in stack targeted: eglot (29+), flymake, eldoc, xref, project.el.
;; External packages from the mirror: corfu, dape, diff-hl, demap, cmake-mode,
;; typescript-mode, highlight-indent-guides, protobuf-mode, kotlin-mode, powershell.
;; Also wires the local emeld-sidebar package (project file-tree sidebar).
;;
;; Version-gated: blocks that need Emacs >= 29 are guarded by `fboundp'
;; checks so loading under 27.x/28.x degrades to no-ops rather than erroring.
;; `use-package' is assumed available (built-in on 29+, installed from the
;; mirror on 27.2/28.x).

(require 'use-package)
(setq use-package-always-ensure nil)

;;
;; -> windows PATH — Emacs needs explicit help finding external tools
;;
(when (eq system-type 'windows-nt)
  (let* ((bin (expand-file-name "bin" user-emacs-directory))
         (sys (getenv "SystemRoot"))
         (xPaths
          `(,bin
            ,(concat bin "/PortableGit/bin")
            ,(concat bin "/PortableGit/usr/bin")
            ,(concat bin "/netcoredbg")
            ,(concat bin "/csharp-ls/tools/net9.0/any")
            ,(concat bin "/hunspell/bin")
            ,(concat bin "/find")
            ,(concat bin "/clang/bin")
            ,(concat bin "/cmake/bin")
            ,(concat bin "/protoc")
            ,(concat bin "/ada_language_server/bin")
            ,(concat bin "/buf/bin")
            ,(concat bin "/kotlin-language-server/bin")
            ,(concat bin "/svn/bin")
            ,(concat bin "/omnisharp")
            ,(concat bin "/exiftool")
            ,(concat bin "/jdtls/bin")
            ,(concat bin "/ImageMagick-7.1.2-27-portable-Q16-x64")
            ,(concat bin "/ffmpeg-7.1.1-essentials_build/bin")
            ,(concat sys "/System32")
            ,(concat sys "/System32/Wbem")
            ,(concat sys "/System32/WindowsPowerShell/v1.0/")
            ,(concat sys "/System32/OpenSSH/")))
         (sysPath (getenv "PATH")))
    (setenv "PATH" (concat (mapconcat #'identity xPaths ";") ";" sysPath))
    (setq exec-path (append xPaths (split-string sysPath ";") (list "." exec-directory)))))

;;
;; -> performance — LSP servers emit lots of stdout
;;
(setq read-process-output-max (* 1024 1024))      ; 1 MB, up from 4 KB default

;;
;; -> eglot — built-in LSP client since Emacs 29.1
;;
(when (fboundp 'eglot-ensure)
  (use-package eglot
    :hook ((c-mode          . eglot-ensure)
           (c++-mode        . eglot-ensure)
           (java-mode       . eglot-ensure)
           (csharp-mode     . eglot-ensure)
           (typescript-mode . eglot-ensure)
           (protobuf-mode   . eglot-ensure)
           (kotlin-mode     . eglot-ensure))
    :config
    ;; Server entries live alongside the rest of the user's config so
    ;; ~/.emacs.d/bin/ (populated from offline-packages/tools/) stays the
    ;; single source of truth for language-server binaries.
    (let* ((bin   (expand-file-name "bin" user-emacs-directory))
           (jdtls (expand-file-name "jdtls/bin/jdtls" bin))
           ;; java-debug bundle jar — version varies; glob picks what's present.
           (jdbg  (car (file-expand-wildcards
                        (expand-file-name "jdtls/com.microsoft.java.debug.plugin-*.jar" bin))))
           (als   (expand-file-name "ada_language_server/bin/ada_language_server" bin))
           (buf   (expand-file-name "buf/bin/buf" bin))
           (csls  (expand-file-name "csharp-ls/csharp-ls" bin))
           (kls   (expand-file-name "kotlin-language-server/bin/kotlin-language-server" bin))
           ;; Windows-specific: csharp-ls is a .NET tool, launched via dotnet + DLL
           (csls-dll (car (file-expand-wildcards
                           (expand-file-name "csharp-ls/tools/net9.0/any/CSharpLanguageServer.dll" bin)))))
      (when (file-executable-p jdtls)
        (add-to-list 'eglot-server-programs
                     `((java-mode java-ts-mode) .
                       (,jdtls
                        ,@(when jdbg
                            `(:initializationOptions (:bundles [,jdbg])))))))
      (when (file-executable-p als)
        (add-to-list 'eglot-server-programs
                     `((ada-mode ada-ts-mode) . (,als))))
      (when (file-executable-p buf)
        (add-to-list 'eglot-server-programs
                     `(protobuf-mode . (,buf "lsp" "serve"))))
      (when (file-executable-p csls)
        (add-to-list 'eglot-server-programs
                     `((csharp-mode csharp-ts-mode) . (,csls))))
      ;; Windows: csharp-ls launched via dotnet + DLL path
      (when (and (eq system-type 'windows-nt) csls-dll (file-exists-p csls-dll))
        (add-to-list 'eglot-server-programs
                     `((csharp-mode csharp-ts-mode) . ("dotnet" ,csls-dll))))
      ;; Windows: omnisharp as alternative C# LSP
      (let ((omnisharp (expand-file-name "bin/omnisharp/OmniSharp.exe" user-emacs-directory))
            (omnisharp-linux (expand-file-name "bin/omnisharp/run" user-emacs-directory)))
        (when (file-executable-p omnisharp)
          (add-to-list 'eglot-server-programs
                       `((csharp-mode csharp-ts-mode) . (,omnisharp "-lsp"))))
        (when (file-executable-p omnisharp-linux)
          (add-to-list 'eglot-server-programs
                       `((csharp-mode csharp-ts-mode) . (,omnisharp-linux "--lsp")))))
      (when (file-executable-p kls)
        (add-to-list 'eglot-server-programs
                     `(kotlin-mode . (,kls)))))
    ;; Quieter, less chatty shutdown in batch/smoke tests.
    (setq eglot-autoshutdown t
          eglot-sync-connect 0)
    (cond ((boundp 'eglot-events-buffer-config)         ; Emacs 30+ / eglot 1.16+
           (setq eglot-events-buffer-config '(:size 0 :format full)))
          ((boundp 'eglot-events-buffer-size)           ; Emacs 29.x
           (setq eglot-events-buffer-size 0)))))

;;
;; -> dumb-jump — grep/ripgrep-based xref backend used as a fallback when
;; eglot is not active (or still connecting). Eglot adds its backend buffer-
;; locally and therefore still wins when a server is attached; dumb-jump is
;; prepended globally so it beats the default etags backend (which otherwise
;; hijacks `M-.' with a TAGS-table prompt).
;;
(use-package dumb-jump
  :after xref
  :custom
  (dumb-jump-force-searcher 'rg)
  (dumb-jump-prefer-searcher 'rg)
  (dumb-jump-selector 'completing-read)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; Minibuffer picker for M-. when multiple definitions match; keep the
;; `*xref*' buffer for M-? / reference browsing (better for bulk review).
;; -- commented out: Emacs-vanilla/xref-core sets
;; `my/xref-show-definitions-remember' which wraps
;; `xref-show-definitions-completing-read' with memoization + preview.
;; (setq xref-show-definitions-function #'xref-show-definitions-completing-read
;;       xref-show-xrefs-function       #'xref-show-definitions-buffer)

;; Ada support for dumb-jump (not built-in). Case-sensitive: works when the
;; codebase is consistent about casing. Covers subprograms, types, packages
;; and variable/constant declarations in .ads / .adb / .ada files.
(with-eval-after-load 'dumb-jump
  (dolist (ext '("ads" "adb" "ada"))
    (add-to-list 'dumb-jump-language-file-exts
                 `(:language "ada" :ext ,ext :agtype nil :rgtype nil)))

  (add-to-list 'dumb-jump-find-rules
               '(:type "function" :supports ("rg" "ag" "grep" "git-grep")
                       :language "ada"
                       :regex "\\s*(procedure|function)\\s+JJJ\\b"))

  (add-to-list 'dumb-jump-find-rules
               '(:type "type" :supports ("rg" "ag" "grep" "git-grep")
                       :language "ada"
                       :regex "\\s*(type|subtype)\\s+JJJ\\b"))

  (add-to-list 'dumb-jump-find-rules
               '(:type "module" :supports ("rg" "ag" "grep" "git-grep")
                       :language "ada"
                       :regex "\\s*package(\\s+body)?\\s+JJJ\\b"))

  (add-to-list 'dumb-jump-find-rules
               '(:type "variable" :supports ("rg" "ag" "grep" "git-grep")
                       :language "ada"
                       :regex "\\s*JJJ\\s*:\\s*(constant\\s+)?[A-Za-z_][A-Za-z0-9_.]*")))

;;
;; -> flymake — diagnostics, built-in. M-n/M-p to jump between errors.
;;
(use-package flymake
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

;;
;; -> diff-hl — inline VCS indicators in fringe (or margin in TTY)
;;
(use-package diff-hl
  :config
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode 1)
  (unless (display-graphic-p)
    (diff-hl-margin-mode 1)))

;;
;; -> magit — porcelain Git interface
;;
(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (setq magit-refresh-status-buffer t)
  (setq magit-section-initial-visibility-alist
        '((untracked . show)
          (stashes . hide)))
  (setq magit-diff-refine-hunk 'all)
  (define-key magit-status-mode-map (kbd "C-w") nil))

;;
;; -> eldoc — signature hints in echo area, built-in.
;;
(use-package eldoc
  :custom
  (eldoc-echo-area-use-multiline-p nil))

;;
;; -> corfu — in-buffer completion UI. Manual trigger keeps the echo area
;; free for eldoc; bind TAB if you prefer tab-to-complete.
;;
(use-package corfu
  :custom
  (corfu-auto nil)
  (corfu-cycle t)
  (corfu-preselect 'first)
  :init
  (when (fboundp 'global-corfu-mode) (global-corfu-mode 1)))

;;
;; -> demap — minimap sidebar with diff-hl integration
;;
(use-package demap
  :demand t
  :config
  (defface my/demap-diff-added
    '((t :background "#335533" :extend t))
    "Face for added lines in demap diff display.")
  (defface my/demap-diff-modified
    '((t :background "#555533" :extend t))
    "Face for modified lines in demap diff display.")
  (defface my/demap-diff-removed
    '((t :background "#553333" :extend t))
    "Face for deleted lines in demap diff display.")

  (defun my/demap-diff-update (&optional minimap-or-name)
    "Update diff overlays in MINIMAP from the source buffer's diff-hl data."
    (let* ((minimap (demap-normalize-minimap
                     (or minimap-or-name (demap-buffer-minimap))))
           (minimap-buf (demap-minimap-buffer minimap))
           (source-buf (demap-minimap-showing minimap)))
      (when (and minimap-buf
                 (buffer-live-p minimap-buf)
                 source-buf
                 (buffer-live-p source-buf))
        (with-current-buffer minimap-buf
          (dolist (ov (overlays-in (point-min) (point-max)))
            (when (overlay-get ov 'my/demap-diff)
              (delete-overlay ov)))
          (with-current-buffer source-buf
            (dolist (ov (overlays-in (point-min) (point-max)))
              (when (overlay-get ov 'diff-hl-hunk)
                (let* ((beg (overlay-start ov))
                       (end (overlay-end ov))
                       (type (overlay-get ov 'diff-hl-hunk-type))
                       (face (pcase type
                               ('insert 'my/demap-diff-added)
                               ('change 'my/demap-diff-modified)
                               ('delete 'my/demap-diff-removed)
                               (_ nil)))
                       (new-ov (make-overlay beg end minimap-buf)))
                  (when face
                    (overlay-put new-ov 'face face)
                    (overlay-put new-ov 'my/demap-diff t)
                    (overlay-put new-ov 'priority 0))))))))))

  (defun my/demap-diff-clear ()
    "Remove all diff overlays from the current minimap buffer."
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'my/demap-diff)
        (delete-overlay ov))))

  (defun my/demap-diff-update-current ()
    "Update diff overlays for any minimap showing the current buffer."
    (when-let* ((minimap-buf (get-buffer demap-minimap-default-name))
                (minimap (demap-buffer-minimap minimap-buf))
                (source-buf (demap-minimap-showing minimap))
                ((buffer-live-p source-buf)))
      (when (eq source-buf (current-buffer))
        (my/demap-diff-update minimap))))

  (defun my/demap-diff-schedule-update ()
    "Schedule a diff update for the minimap after diff-hl runs."
    (run-with-idle-timer 0.1 nil #'my/demap-diff-update-current))

  (defun my/demap-diff-after-save ()
    "Update demap diff overlays after saving a buffer with diff-hl active."
    (when (bound-and-true-p diff-hl-mode)
      (my/demap-diff-schedule-update)))

  (add-hook 'after-save-hook #'my/demap-diff-after-save)
  (add-hook 'window-state-change-hook
            (lambda ()
              (run-with-idle-timer 0.05 nil #'my/demap-diff-update-current)))

  (add-hook 'demap-minimap-construct-hook
            (lambda ()
              (my/demap-diff-update-current)))

  (add-hook 'demap-minimap-construct-hook
            (lambda ()
              (my/demap-diff-update-current)
              (add-hook 'post-command-hook #'my/demap-diff-update-current nil t))
            nil t))

;;
;; -> dape — Debug Adapter Protocol client (gdb, lldb, netcoredbg, jdtls...).
;; F5 run, F9 toggle breakpoint, F10/F11 step.
;;
(use-package dape
  :bind (("<f5>"    . dape)
         ("<S-f5>"  . dape-kill)
         ("<C-f5>"  . dape-restart)
         ("<f9>"    . dape-breakpoint-toggle)
         ("<f10>"   . dape-next)
         ("<f11>"   . dape-step-in)
         ("<S-f11>" . dape-step-out))
  :config
  (setq dape-request-timeout 30)
  ;; Windows path normalization — dape defaults to Unix-style paths which
  ;; breaks debug adapters expecting Windows drive-letter paths.
  (when (eq system-type 'windows-nt)
    (setq dape-normalize-path-separator 'windows))
  ;; netcoredbg — register if the bundled binary is present. The config is
  ;; a template; override :program / :cwd per project via `M-x dape' prompt.
  (let* ((ncdbg   (expand-file-name "bin/netcoredbg/netcoredbg" user-emacs-directory))
         (ncdbg-win (expand-file-name "bin/netcoredbg/netcoredbg.exe" user-emacs-directory))
         (ncdbg-path (or (and (file-executable-p ncdbg) ncdbg)
                         (and (file-executable-p ncdbg-win) ncdbg-win))))
    (when ncdbg-path
      (add-to-list 'dape-configs
                   `(netcoredbg-launch
                     modes (csharp-mode csharp-ts-mode)
                     command ,ncdbg-path
                     command-args ("--interpreter=vscode")
                     :type "coreclr"
                     :request "launch"
                     :console "internalConsole"
                     :stopAtEntry nil)))))

;;
;; -> language mode hints — file-extension associations only; no heavy config.
;;
(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'"         . cmake-mode)))

(use-package typescript-mode
  :mode "\\.ts\\'")

(use-package protobuf-mode
  :mode "\\.proto\\'")

(use-package kotlin-mode
  :mode "\\.kts\\'")

(use-package powershell
  :mode (("\\.ps1\\'"  . powershell-mode)
         ("\\.psm1\\'" . powershell-mode)
         ("\\.psd1\\'" . powershell-mode)))

(use-package highlight-indent-guides
  :hook ((prog-mode yaml-mode) . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top))

;;
;; -> GUD/GDB fallback keys — overridden by dape when a dape session is live
;; because dape's keymap is buffer-local to its debuggee.
;;
(setq gdb-display-io-nopopup 1
      gdb-many-windows t)

;;
;; -> old-ada-mode loaded from local-packages/ if present (see coding guide
;; Part 12). Silent no-op otherwise.
;;
(let ((old-ada (expand-file-name "local-packages/old-ada-mode" user-emacs-directory)))
  (when (file-directory-p old-ada)
    (add-to-list 'load-path old-ada)
    (use-package ada-mode
      :mode ("\\.gpr\\'" "\\.ada\\'" "\\.ads\\'" "\\.adb\\'"))))

;;
;; -> Ada project file helpers — ada_language_server needs a .gpr to work
;; properly; these functions find existing ones or create a basic template.
;;
(defun my/ada-find-gpr-file (&optional dir)
  "Find the nearest .gpr file upward from DIR (defaults to `default-directory')."
  (let ((dir (or dir default-directory)))
    (locate-dominating-file dir
                            (lambda (d) (car (directory-files d t "\\.gpr\\'"))))))

(defun my/ada-create-gpr (project-name &optional main-file)
  "Create PROJECT-NAME.gpr with an optional MAIN-FILE entry point."
  (interactive "sProject name: \nsMain Ada file (e.g. main.adb): ")
  (let ((gpr-file (concat project-name ".gpr")))
    (with-current-buffer (find-file gpr-file)
      (erase-buffer)
      (insert (format "project %s is\n" project-name)
              "\n"
              "   for Source_Dirs use (\"**\");\n"
              "   for Object_Dir use \"build\";\n"
              "   for Exec_Dir use \".\";\n"
              (when (and main-file (not (string-empty-p main-file)))
                (format "   for Main use (\"%s\");\n" main-file))
              "\n"
              (format "end %s;\n" project-name))
      (goto-char (point-min))
      (when (called-interactively-p 'interactive)
        (message "Created %s" gpr-file))
      gpr-file)))

(defun my/ada-ensure-gpr (&optional dir)
  "Find a .gpr file in DIR or interactively create one.
Returns the absolute path to the .gpr file, or nil if cancelled."
  (let ((dir (or dir default-directory)))
    (or (my/ada-find-gpr-file dir)
        (when (y-or-n-p "No .gpr file found.  Create one? ")
          (call-interactively #'my/ada-create-gpr)))))

(defun my/ada-setup-project (&optional dir)
  "Set up ada_language_server project for the current Ada buffer.
Finds or creates a .gpr file and restarts eglot so ALS picks it up."
  (interactive)
  (unless (derived-mode-p 'ada-mode)
    (user-error "Not in an Ada buffer"))
  (let* ((dir (or dir default-directory))
         (gpr (my/ada-ensure-gpr dir)))
    (if gpr
        (let* ((gpr-dir (file-name-directory gpr))
               (gpr-file (file-name-nondirectory gpr)))
          ;; Tell ALS which project file to use via eglot workspace config
          (setq-local eglot-workspace-configuration
                      `((ada . ((projectFile . ,gpr)))))
          ;; If eglot is already running, restart it so ALS re-reads config
          (when (eglot-current-server)
            (eglot-shutdown (eglot-current-server))
            ;; Re-open the project root so eglot picks the right root
            (let ((default-directory gpr-dir))
              (eglot-ensure))))
      (user-error "No .gpr file; ada_language_server needs a project file"))))

;;
;; -> emeld-sidebar — project file-tree sidebar that follows the active file
;;
;; A docked sidebar that highlights and reveals the active file as you switch
;; buffers; `C-x m' toggles it open/closed, rooted at the current file's
;; project.  Current-line overlay and follow behaviour are built in.  Ships in
;; this toolkit under local-packages/emeld.
(use-package emeld
  :load-path "~/.emacs.d/offline-packages/local-packages/emeld"
  :bind ("C-x m" . emeld-sidebar))

;;
;; -> project-overview — dashboard table of git projects with status + actions
;;
;; `C-x p O' opens a sortable table with one row per auto-discovered git
;; project, showing the latest CHANGELOG.org version/date, open/total BUGS.org
;; count, and git branch/dirty/ahead-behind state.  Single keys act on the
;; project under point (switch, find-file, magit, dired, open CHANGELOG/BUGS).
;; Ships in this toolkit under local-packages/project-overview.  Needs the
;; Emacs 28+ project.el commands, so it is skipped on 27.x.
;;
;; In the dashboard: RET/o switch · f find file · m magit · D dired ·
;; w browse remote · c/C changelog · R README · b BUGS · i/P GitHub
;; issues/PRs · / filter · V cycle view · t toggle Description · g refresh ·
;; r cache/pull · ? all actions.  Every option below has a sensible default,
;; so the whole `:custom' block is optional reference; see example-init.el in
;; the package directory for the full annotated list.
(when (fboundp 'project-switch-project)
  (use-package project-overview
    :load-path "~/.emacs.d/offline-packages/local-packages/project-overview"
    :commands (project-overview)
    :bind (:map project-prefix-map ("O" . project-overview))
    :init
    ;; Show the dashboard instead of the splash screen at startup.
    (setq initial-buffer-choice
          (lambda ()
            (project-overview)
            (get-buffer project-overview-buffer-name)))
    :custom
    ;; Roots scanned for git projects (each checked directly and one level
    ;; deep for .git subdirs).  Adjust to wherever you keep your repos.
    (project-overview-search-roots
     (list (expand-file-name "~/source/repos")
           (expand-file-name "~/.emacs.d")))
    ;; Also list the projects Emacs already knows (`project-switch-project').
    (project-overview-include-known-projects t)
    ;; Hide project directories whose name matches this regexp.
    (project-overview-exclude-regexp "\\`linux-")
    ;; Column layout on open (full/minimal/status/remote); remember `V' choice.
    (project-overview-default-view 'full)
    (project-overview-remember-view t)
    ;; Fetch open issue/PR counts for GitHub repos (async; needs the
    ;; authenticated "gh" CLI).  Set to nil to skip all network calls.
    (project-overview-show-github t)
    ;; Fall back to fetching the MELPA list from melpa.org when package.el
    ;; has no local archive contents (once per session).
    (project-overview-melpa-fallback t)
    ;; Persist MELPA list + GitHub counts between sessions; refresh after TTL.
    (project-overview-cache-file
     (locate-user-emacs-file "project-overview-cache.el"))
    (project-overview-cache-ttl 86400)
    ;; -- Personal, off by default; uncomment and adapt --
    ;; Your GitHub user: summarises owned repos' issue/PR totals in the
    ;; header line and enables the "owned by me" filter (/ o).
    ;; (project-overview-github-user "your-github-username")
    ;; Always open remotes in Firefox regardless of the system default.
    ;; (project-overview-browse-url-function #'browse-url-firefox)
    ))

;;
;; -> vc-shuttle — override vc-git-push/pull for air-gapped VM workflow
;; (calls local scripts on the VM guest that copy repo data to/from a
;; shared folder on the host machine, since there's no remote to push to).
;;
(defun my/vc-git-push-shuttle (_file-list &rest _args)
  "Override vc-push to run a local sync script instead of git push."
  (interactive)
  (let ((script-path "/home/jdyer/bin/out"))
    (if (file-executable-p script-path)
        (progn
          (message "Syncing source TO shared folder...")
          (async-shell-command script-path "*out*"))
      (error "Sync script not found or not executable at %s"
             script-path))))

(defun my/vc-git-pull-shuttle (_file-list &rest _args)
  "Override vc-pull to run a local sync script instead of git pull."
  (interactive)
  (let ((script-path "/home/jdyer/bin/in"))
    (if (file-executable-p script-path)
        (progn
          (message "Syncing source FROM shared folder...")
          (async-shell-command script-path "*in*"))
      (error "Sync script not found or not executable at %s"
             script-path))))

(advice-add 'vc-git-push :override #'my/vc-git-push-shuttle)
(advice-add 'vc-git-pull :override #'my/vc-git-pull-shuttle)

;;
;; -> outline-indent — indentation-based folding for structured files
;; (XML/FOM, web-mode, YAML, JSON, Dockerfiles, Python, conf).  Keeps the
;; major mode intact (e.g. nxml schema validation) while adding outline
;; folding, narrow-to-subtree, and subtree reindent/move.  A `transient'
;; menu on `C-c o' surfaces every command for discovery; `C-c C-f' toggles
;; the fold at point in any outline-indent buffer (web-mode muscle memory).
;; Guarded with `locate-library' so it no-ops where the package is absent
;; (e.g. older Emacs where it failed to install from the mirror).
;;
(when (locate-library "outline-indent")
  (use-package outline-indent
    :ensure t
    :commands outline-indent-minor-mode
    :custom
    (outline-indent-ellipsis " ▼")
    :hook
    ((nxml-mode
      mhtml-mode
      prog-mode
      web-mode
      yaml-mode
      yaml-ts-mode
      json-mode
      json-ts-mode
      js-json-mode
      dockerfile-mode
      python-mode
      python-ts-mode
      conf-mode) . outline-indent-minor-mode))

  ;; (add-hook 'outline-indent-minor-mode-hook
  ;;           (lambda ()
  ;;             (when (derived-mode-p 'nxml-mode)
  ;;               (outline-indent-close-level 2))))

  (defun my/outline-hide-sublevels-prompt ()
    "Hide buffer to a given level, prompting for the level number."
    (interactive)
    (outline-hide-sublevels (read-number "Levels to hide: ")))

  (defun my/outline-toggle-recursive ()
    "Toggle fold: close if open, open recursively if closed."
    (interactive)
    (if (outline-indent-folded-p)
        (outline-indent-open-fold-rec)
      (outline-indent-close-fold)))

  (require 'transient)

  (transient-define-prefix outline-indent-transient ()
    "Outline / outline-indent folding, navigation, and structure commands."
    :transient-non-suffix #'transient--do-stay
    [["Fold at point"
      ("TAB" "Toggle level"         outline-indent-toggle-level-at-point)
      ("o"   "Toggle recursive"     my/outline-toggle-recursive)
      ("h"   "Close fold"           outline-indent-close-fold)
      ("s"   "Show subtree"         outline-show-subtree)]
     ["Whole buffer"
      ("a"   "Open all folds"       outline-indent-open-folds)
      ("m"   "Close all folds"      outline-indent-close-folds)
      ("l"   "Hide to N levels"     my/outline-hide-sublevels-prompt)
      ("k"   "Isolate (hide other)" outline-hide-other)]
     ["Navigate"
      ("n"   "Next heading"         outline-next-visible-heading)
      ("p"   "Prev heading"         outline-previous-visible-heading)
      ("f"   "Fwd same level"       outline-indent-forward-same-level)
      ("b"   "Back same level"      outline-indent-backward-same-level)
      ("u"   "Up heading"           outline-up-heading)]
     ["Structure / Isolate"
      (">"   "Shift right"          outline-indent-shift-right)
      ("<"   "Shift left"           outline-indent-shift-left)
      ("v"   "Select block"         outline-indent-select)
      ("N"   "Narrow to block"      outline-indent-narrow)
      ("w"   "Widen"                widen)]
     ])

  (with-eval-after-load 'outline-indent
    (define-key outline-indent-minor-mode-map (kbd "C-c o") #'outline-indent-transient)
    ;; Global fold/unfold across every outline-indent buffer (web-mode muscle memory).
    (define-key outline-indent-minor-mode-map (kbd "C-c C-f") #'outline-cycle)
    ;; TAB (C-i in GUI) toggles fold at point, same as C-c C-f.
    (define-key outline-indent-minor-mode-map (kbd "C-i") #'outline-cycle))
  )
