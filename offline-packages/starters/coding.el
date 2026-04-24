;; -*- lexical-binding: t; -*-
;;
;; coding.el — language-agnostic coding companion.
;;
;; Distilled from docs/setting-up-emacs-for-coding/README.org (Part 15).
;; Optional, not auto-loaded. To try it, append to ~/.emacs.d/init.el:
;;   (load (expand-file-name "coding-starter" user-emacs-directory) t t)
;;
;; Built-in stack targeted: eglot (29+), flymake, eldoc, xref, project.el.
;; External packages from the mirror: corfu, dape, cmake-mode, typescript-mode,
;; highlight-indent-guides, protobuf-mode, kotlin-mode, treemacs.
;;
;; Version-gated: blocks that need Emacs >= 29 are guarded by `fboundp'
;; checks so loading under 27.x/28.x degrades to no-ops rather than erroring.
;; `use-package' is assumed available (built-in on 29+, installed from the
;; mirror on 27.2/28.x).

(require 'use-package)
(setq use-package-always-ensure nil)

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
           (kls   (expand-file-name "kotlin-language-server/bin/kotlin-language-server" bin)))
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
(setq xref-show-definitions-function #'xref-show-definitions-completing-read
      xref-show-xrefs-function       #'xref-show-definitions-buffer)

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
  ;; netcoredbg — register if the bundled binary is present. The config is
  ;; a template; override :program / :cwd per project via `M-x dape' prompt.
  (let ((ncdbg (expand-file-name "bin/netcoredbg/netcoredbg" user-emacs-directory)))
    (when (file-executable-p ncdbg)
      (add-to-list 'dape-configs
                   `(netcoredbg-launch
                     modes (csharp-mode csharp-ts-mode)
                     command ,ncdbg
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
    (dolist (ext '("\\.gpr\\'" "\\.ada\\'" "\\.ads\\'" "\\.adb\\'"))
      (add-to-list 'auto-mode-alist (cons ext 'ada-mode)))))

;;
;; -> treemacs — project sidebar that follows the current buffer (VSCode-like).
;; C-x m toggles a current-project-only view; focus stays in the editing window.
;; Current-line highlight is a dedicated high-priority overlay so it renders
;; regardless of any local `hl-line' customization.
;;
(use-package treemacs
  :bind ("C-x m" . my/treemacs-toggle-current-project)
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (setq treemacs-pulse-on-success t
        treemacs-follow-after-init t
        treemacs-show-cursor nil))

(defvar-local my/treemacs-current-overlay nil)

(defun my/treemacs-update-current-line ()
  "Paint a persistent, high-priority region-face overlay on point's line."
  (unless (overlayp my/treemacs-current-overlay)
    (setq my/treemacs-current-overlay (make-overlay 1 1))
    (overlay-put my/treemacs-current-overlay 'face 'region)
    (overlay-put my/treemacs-current-overlay 'priority 1000))
  (move-overlay my/treemacs-current-overlay
                (line-beginning-position)
                (1+ (line-end-position))))

(defun my/treemacs-update-current-line-after-follow (&rest _)
  "Run `my/treemacs-update-current-line' in the treemacs buffer.
Follow-mode drives point from a timer, which does not fire
`post-command-hook', so this advice closes that gap."
  (let ((buf (ignore-errors (treemacs-get-local-buffer))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (my/treemacs-update-current-line)))))

(with-eval-after-load 'treemacs
  (advice-add 'treemacs--follow :after
              #'my/treemacs-update-current-line-after-follow))

(add-hook 'treemacs-mode-hook
          (lambda ()
            (hl-line-mode -1)
            (add-hook 'post-command-hook
                      #'my/treemacs-update-current-line nil t)
            (my/treemacs-update-current-line)))

(defun my/treemacs-toggle-current-project ()
  "Show treemacs with the current project only, or hide it if visible.
On open, keep focus in the original window."
  (interactive)
  (let ((win (and (fboundp 'treemacs-get-local-window)
                  (treemacs-get-local-window))))
    (if (and win (window-live-p win))
        (delete-window win)
      (save-selected-window
        (treemacs-display-current-project-exclusively)))))
