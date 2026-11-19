;; -*- lexical-binding: t; -*-
;;
;; Starter customisations for Emacs 31.1 — optional, unused by default.
;;
;; Distilled from the upstream author's live init.el.  The rendered init.el is
;; now only a bootstrapper: it installs the ELPA mirror, bootstraps
;; ~/.emacs.d/local-packages, loads Emacs-vanilla + Emacs-DIYer, installs the
;; per-version package list, and applies the initial custom faces.  All
;; per-package setup lives here and in starters/coding.el.
;;
;; Coding-oriented blocks (eglot, dape, emeld-sidebar, outline-indent, etc.)
;; live in starters/coding.el to avoid duplication — load both:
;;   (load (expand-file-name "init-starter" user-emacs-directory) t t)
;;   (load (expand-file-name "init-starter-coding" user-emacs-directory) t t)
;;
;; To try it, either:
;;   (a) append this line to ~/.emacs.d/init.el:
;;         (load (expand-file-name "init-starter" user-emacs-directory) t t)
;;   (b) or cherry-pick blocks into your own init.el.
;;
;; Assumes `use-package' is installed (bundled via the offline mirror, and
;; also built in to Emacs 29+).  `:ensure' is deliberately left off: the
;; bundled init.el already `my/ensure-package'd everything from
;; packages/emacs-31.1.el.  Blocks for packages that only ship in newer
;; mirrors are guarded with `:if (locate-library ...)`, so this starter also
;; loads cleanly on older targets.

(require 'use-package)
(setq use-package-always-ensure nil)
(setq use-package-verbose t)

;;
;; -> themes (pick one to enable at startup, or just M-x load-theme)
;;
(use-package doom-themes
  :demand t)

(use-package ef-themes
  :defer t
  :if (locate-library "ef-themes"))

(use-package modus-themes
  :defer t
  :if (locate-library "modus-themes"))

(use-package gruvbox-theme
  :defer t)

(use-package timu-caribbean-theme
  :defer t)

(use-package timu-spacegrey-theme
  :defer t
  :custom
  (timu-spacegrey-scale-org-document-title 1.8)
  (timu-spacegrey-scale-org-document-info 1.4)
  (timu-spacegrey-scale-org-level-1 1.8)
  (timu-spacegrey-scale-org-level-2 1.4)
  (timu-spacegrey-scale-org-level-3 1.2))

(use-package timu-rouge-theme
  :defer t
  :custom
  (timu-rouge-mode-line-border t)
  (timu-rouge-scale-org-document-title 1.8)
  (timu-rouge-scale-org-document-info 1.4)
  (timu-rouge-scale-org-level-1 1.8)
  (timu-rouge-scale-org-level-2 1.4)
  (timu-rouge-scale-org-level-3 1.2))

;;
;; -> favourite-themes
;;
;; Pick one at a time; the others stay commented so they are easy to
;; swap.  `gruvbox-dark-hard' comes from `gruvbox-theme' and
;; `ef-trio-dark' from `ef-themes'.
;;
;; (load-theme 'gruvbox-dark-hard t)
(load-theme 'doom-nord t)
;; (load-theme 'doom-snazzy t)
;; (load-theme 'doom-tokyo-night t)
;; (load-theme 'doom-material-dark t)
;; (load-theme 'doom-outrun-electric t)
;; (load-theme 'doom-manegarm t)
;; (load-theme 'ef-trio-dark t)

;;
;; -> face heights
;;
;; `default' is the only face that needs an absolute height; everything else
;; uses a float relative to it so global font-size changes and
;; `text-scale-mode' keep the UI proportional.  Override specs are applied
;; after themes, so a theme change cannot reset these sizes.
(dolist (face-height '((fixed-pitch 1.1)
                       (variable-pitch 1.2)
                       (mode-line 1.4)
                       (mode-line-inactive 1.4)
                       (org-mode-line-clock 1.4)
                       (org-mode-line-clock-overrun 1.4)))
  (face-spec-set (car face-height)
                 `((t (:height ,(cadr face-height))))
                 'face-override-spec))

;; Tab labels inherit `fixed-pitch' so a buffer-local `variable-pitch-mode'
;; cannot change their typeface as you switch tabs.
(face-spec-set 'tab-bar '((t (:inherit fixed-pitch))) 'face-override-spec)
(face-spec-set 'tab-bar-tab
               '((t (:inherit (highlight fixed-pitch))))
               'face-override-spec)
(face-spec-set 'tab-bar-tab-inactive
               '((t (:inherit fixed-pitch)))
               'face-override-spec)

;;
;; -> font-size-overrides
;;
;; Per-machine override point for the sizes in `font-size-core'.  The
;; face-height block above supplies the starter defaults; uncommenting
;; this replaces them.  `default' and `variable-pitch' are in 1/10pt;
;; `fixed-pitch' and `mode-line' are relative to `default'.
;; `my/set-font-size-overrides' applies the whole set, so include every
;; face you want to pin; unspecified faces fall back to Emacs-vanilla's
;; defaults.
;;
;; (when (fboundp 'my/set-font-size-overrides)
;;   (my/set-font-size-overrides
;;    '((default . 100)
;;      (fixed-pitch . 1.1)
;;      (variable-pitch . 120)
;;      (mode-line . 1.4))))

;;
;; -> mode hints for file types
;;
(use-package yaml-mode
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)))

(use-package csv-mode
  :mode "\\.csv\\'")

(use-package i3wm-config-mode
  :mode (("/sway/config\\'" . i3wm-config-mode)
         ("/sway/.*config.*/" . i3wm-config-mode)))

;; Emacs 30+ otherwise falls back to `fundamental-mode' for .ts/.tsx when the
;; tree-sitter TypeScript grammar is not installed — no font-lock at all.
(use-package typescript-mode
  :mode (("\\.ts\\'"  . typescript-mode)
         ("\\.tsx\\'" . typescript-mode)))

(use-package web-mode
  :mode "\\.cshtml?\\'"
  :hook (html-mode . web-mode)
  :bind (:map web-mode-map ("M-;" . nil)))

(use-package markdown-mode
  :mode ("\\.md\\'" "\\.markdown\\'"))

(use-package zig-mode
  :mode "\\.zig\\'")

(use-package plantuml-mode
  :mode ("\\.puml\\'" "\\.plantuml\\'" "\\.iuml\\'")
  :custom
  (plantuml-jar-path "~/.emacs.d/local-packages/ox-plantuml-gantt/plantuml.jar")
  (plantuml-default-exec-mode 'jar)
  (plantuml-output-type "png"))

(use-package ox-plantuml-gantt
  :ensure nil
  :after org)

(add-to-list 'auto-mode-alist '("\\.cshtml\\'" . html-mode))

;;
;; -> async dired
;;
(use-package async
  :config
  (require 'dired-async)
  (dired-async-mode 1))

;;
;; -> vlf — view and edit huge files in batches (GNU ELPA)
;;
;; VLF reads a file chunk by chunk, so files larger than RAM open without
;; swapping. Loading `vlf-setup' hooks the built-in large-file prompt;
;; `vlf-application' decides when VLF is used:
;;   ask       (default) offer VLF past `large-file-warning-threshold'
;;   dont-ask  open large files in VLF automatically
;;   always    use VLF for every file
;;   nil       never automatic — open explicitly with M-x vlf
;;
;; Dired gains a `V' binding (dired-vlf) once vlf-setup is loaded.
;; Inside a VLF buffer C-c C-v is the prefix: n/p next/prev batch,
;; s/r search forward/backward, o occur index, l goto-line, j jump to
;; a batch, % query-replace across the whole file.
(use-package vlf
  :demand t
  :custom
  (vlf-application 'ask)
  (vlf-batch-size 1048576)
  (vlf-tune-enabled t)
  :config
  ;; Hooks VLF into `abort-if-file-too-large' and defines `dired-vlf'.
  (require 'vlf-setup)
  ;; SVN dumps / generic dumps: keep them in fundamental-mode so no slow
  ;; major-mode or font-lock runs over the chunk.
  (add-to-list 'auto-mode-alist '("\\.svndump\\'" . fundamental-mode))
  (add-to-list 'auto-mode-alist '("\\.dump\\'" . fundamental-mode))
  ;; Safety net for very long single lines inside a chunk.
  (when (fboundp 'global-so-long-mode)
    (global-so-long-mode 1))
  (define-key my-overrides-mode-map (kbd "C-c V") #'vlf)
  (with-eval-after-load 'dired
    (define-key dired-mode-map (kbd "V") #'dired-vlf)))

;;
;; -> selected-window-accent-mode — highlights the active window's borders
;;
(use-package selected-window-accent-mode
  :config
  (selected-window-accent-mode 1)
  (define-key my-overrides-mode-map (kbd "C-c w") selected-window-accent-map)
  :custom
  (selected-window-accent-fringe-minimum 10)
  (selected-window-accent-fringe-thickness 10)
  (selected-window-accent-mode-style 'default)
  (selected-window-accent-tab-accent t)
  (selected-window-accent-use-pywal t)
  (selected-window-accent-smart-borders nil))

;;
;; -> simply-annotate — overlay notes on any file
;;
(use-package simply-annotate
  :if (version<= "28.1" emacs-version)
  :demand t
  :hook (find-file-hook . simply-annotate-mode)
  :config
  ;; Fall through to the stock search map for unbound M-s keys.
  (set-keymap-parent simply-annotate-command-map search-map)
  (let ((my-ms-map (make-sparse-keymap)))
    (set-keymap-parent my-ms-map simply-annotate-command-map)
    (define-key my-ms-map (kbd "u")
                (lambda () (interactive) (my/tab-bar-push-window -1)))
    (define-key my-ms-map (kbd "i")
                (lambda () (interactive) (my/tab-bar-push-window 1)))
    (define-key my-overrides-mode-map (kbd "M-s") my-ms-map))
  (define-key simply-annotate-mode-map (kbd "M-N") #'simply-annotate-next)
  (define-key simply-annotate-mode-map (kbd "M-P") #'simply-annotate-previous)
  (defun my/simply-annotate-next-or-flymake ()
    "Go to the next annotation, or the next flymake error when not annotating."
    (interactive)
    (if (bound-and-true-p simply-annotate-mode)
        (simply-annotate-next)
      (call-interactively #'flymake-goto-next-error)))
  (defun my/simply-annotate-previous-or-flymake ()
    "Go to the previous annotation, or the previous flymake error when not annotating."
    (interactive)
    (if (bound-and-true-p simply-annotate-mode)
        (simply-annotate-previous)
      (call-interactively #'flymake-goto-prev-error)))
  (with-eval-after-load 'flymake
    (define-key flymake-mode-map (kbd "M-N") #'my/simply-annotate-next-or-flymake)
    (define-key flymake-mode-map (kbd "M-P") #'my/simply-annotate-previous-or-flymake))
  ;; `posframe' is optional in the offline toolkit; load it when present.
  (when (locate-library "posframe")
    (require 'posframe))
  (setq simply-annotate-inline-position 'margin-right)
  (setq simply-annotate-tint-amount 50)
  (setq simply-annotate-hide-done-statuses '("resolved" "closed"))
  (setq simply-annotate-hide-done-style 'indicator)
  (setq simply-annotate-inline-pointer-above "┗━▶")
  (setq simply-annotate-inline-pointer-after "┏━▶")
  (setq simply-annotate-author-list '("John Doe" "Jane Smith" "James Dyer"))
  (setq simply-annotate-prompt-for-author 'threads-only)
  (setq simply-annotate-database-strategy 'both)
  ;; Offline project roots are often read-only.  Fall back to the
  ;; writable global database when the project-local database cannot be
  ;; written, instead of silently failing to persist annotations.
  (defun my/simply-annotate--database-path (orig)
    "Use the project database when writable, else the global database."
    (let ((path (funcall orig)))
      (if (file-writable-p path)
          path
        (expand-file-name simply-annotate-file))))
  (unless (advice-member-p #'my/simply-annotate--database-path
                           'simply-annotate--database-path)
    (advice-add 'simply-annotate--database-path :around
                #'my/simply-annotate--database-path))
  (setq simply-annotate-display-style '(bracket)))

;; Enable annotation mode for every new file.  The package's own
;; find-file hook only enables it when annotations already exist.
(when (version<= "28.1" emacs-version)
  (add-hook 'find-file-hook #'simply-annotate-mode))

(with-eval-after-load 'simply-annotate
  (add-hook 'dired-mode-hook #'simply-annotate-dired-mode))


;;
;; -> doc-view-follow — sync pages between windows showing the same document
;; (doc-view-mode / pdf-view-mode). Split the window and both panes navigate
;; in lockstep — handy for a side-by-side "book view".
;;
(use-package doc-view-follow
  :if (locate-library "doc-view-follow")
  :config (global-doc-view-follow-mode 1))


;;
;; -> package-lint — M-x package-lint-current-buffer to audit your own packages
;;
(use-package package-lint
  :if (locate-library "package-lint")
  :commands (package-lint-current-buffer
             package-lint-batch-and-exit))

;;
;; -> ollama-buddy — AI chat against a local ollama server or cloud providers
;; Needs `ollama serve' running locally, or cloud API keys in auth-source.
;;
(use-package ollama-buddy
  :if (locate-library "ollama-buddy")
  :bind
  ("C-c o" . ollama-buddy-role-transient-menu)
  ("C-c O" . ollama-buddy-transient-menu)
  :config
  (setq ollama-buddy-default-model "deepseek-v3.1:671b-cloud")
  (setq ollama-buddy-max-history-length 999))

;;
;; -> diff-minimap — minimap sidebar showing diff-hl regions
;;
;; Toggle with `C-z m' (see `my-win-keymap' below).  It lets `M-n'/`M-p'
;; keep their hunk-navigation meaning in file buffers.
;;
(use-package diff-minimap
  :ensure nil
  :demand t
  :bind (:map my-win-keymap
              ("m" . diff-minimap-toggle))
  :init
  (add-to-list 'my/window-nav-passthrough-minor-modes 'diff-minimap-mode))

;;
;; -> transmute — media management utilities (image, video, audio)
;;
;; On GNU/Linux, make a bundled exiftool under ~/.emacs.d/bin/exiftool
;; discoverable by `executable-find' (transmute treats exiftool as a hard
;; dependency with no fallback, unlike trash-put). Mirrors the Linux PATH
;; block in init-starter-coding.el.
(when (and (eq system-type 'gnu/linux)
           (file-directory-p
            (expand-file-name "bin/exiftool" user-emacs-directory)))
  (let ((p (expand-file-name "bin/exiftool" user-emacs-directory)))
    (setenv "PATH" (concat p path-separator (getenv "PATH")))
    (add-to-list 'exec-path p)))

(use-package transmute
  :ensure nil
  :demand t
  :bind (:map my-overrides-mode-map
              ("C-c I" . transmute-menu))
  :config
  (with-eval-after-load 'image-dired
    (transmute-setup-thumbnail-keys)))

;;
;; -> org-bootstrap-publish — generate Bootstrap 5 sites from Org files
;;
(use-package org-bootstrap-publish
  :ensure nil
  :demand t)

;;
;; -> org-table-fit — fit Org tables to the window width
;;
;; Vendored under local-packages/org-table-fit; no ELPA archive needed.
;; `org-table-fit-overlay-mode' wraps wide tables with overlays without
;; changing the buffer text; `org-table-fit-window' rewrites the table at
;; point and `org-table-fit-unwrap' restores it.  Requires Emacs 29.1, so the
;; block skips itself on 27.x/28.x.
;;
(use-package org-table-fit
  :ensure nil
  :if (version<= "29.1" emacs-version)
  :load-path ("offline-packages/local-packages/org-table-fit"
              "local-packages/org-table-fit")
  :hook (org-mode . org-table-fit-overlay-mode)
  :bind (:map org-mode-map
              ("C-c t i" . org-table-fit-window)
              ("C-c t t" . org-table-fit-overlay-mode)
              ("C-c t u" . org-table-fit-unwrap)
              ("C-c t r" . org-table-fit-toggle-reveal-on-point))
  :custom
  (org-table-fit-overlay-reveal-on-point nil))

;;
;; -> calfw — month calendar backed by Org agenda files
;;
;; Available from the 30.x/31.x mirrors.  `C-c D' opens the month view;
;; `M'/`W'/`D' switch between month, week, and day views, `SPC' opens the
;; selected day in Org Agenda, and `RET' visits the selected item.
;;
(use-package calfw
  :if (locate-library "calfw")
  :custom
  ;; Hide calendar holidays; only Org events should appear.
  (calfw-display-calendar-holidays nil))

(use-package calfw-org
  :if (locate-library "calfw-org")
  :bind ("C-c D" . calfw-org-open-calendar)
  :custom
  ;; Include plain timestamps as well as SCHEDULED/DEADLINE entries.
  (calfw-org-overwrite-default-keybinding t)
  (calfw-org-agenda-schedule-args '(:timestamp :scheduled :deadline)))

;;
;; -> org-ibullets — render Org heading stars as UTF-8 bullets
;;
;; Vendored under local-packages/org-ibullets; no ELPA archive needed.
;; Disabling font-cache compaction keeps the composed bullets fast.
;;
(use-package org-ibullets
  :ensure nil
  :load-path ("offline-packages/local-packages/org-ibullets"
              "local-packages/org-ibullets")
  :commands org-ibullets-mode
  :init
  (setq inhibit-compacting-font-caches t)
  :hook (org-mode . org-ibullets-mode))

;;
;; -> org-slides — present an Org buffer, one top-level heading per slide
;;
;; Vendored under local-packages/org-slides; no ELPA archive needed.
;; C-c P starts, n/SPC/right go forward, p/S-SPC/left go back, and q quits.
;; C-c s is the global spelling-menu prefix, so use capital P for slides.
;;
(use-package org-slides
  :ensure nil
  :load-path ("offline-packages/local-packages/org-slides"
              "local-packages/org-slides")
  :commands (org-slides-start org-slides-timed-macro-toggle)
  :bind ("C-c T" . org-slides-timed-macro-toggle)
  :init
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-c P") #'org-slides-start))
  :custom
  (org-slides-page-style 'split)
  (org-slides-text-scale 3)
  (org-slides-hide-stars-in-headings nil)
  :config
  ;; Refit displayed tables after org-slides narrows to a slide.  The
  ;; text-scale refresh can run while the title page is current, and slide
  ;; navigation does not otherwise retrigger it, so wide tables can keep
  ;; their old layout until the window is resized.
  (add-hook 'org-slides-after-navigate-functions
            (lambda (&rest _)
              (when (and (bound-and-true-p org-table-fit-overlay-mode)
                         (fboundp 'org-table-fit-refresh-overlays))
                (org-table-fit-refresh-overlays)))))

;;
;; -> xkb-mode — edit XKB keyboard layout files
;;
;; Vendored under local-packages/xkb-mode; no ELPA archive needed.  Local
;; packages get no generated autoloads, so register .xkb explicitly.
;;
(use-package xkb-mode
  :ensure nil
  :load-path ("offline-packages/local-packages/xkb-mode"
              "local-packages/xkb-mode")
  :mode "\\.xkb\\'")

;;
;; -> simply-kanban — Org-backed kanban board
;;
(use-package simply-kanban
  :ensure nil
  :commands simply-kanban-show-card
  :bind
  ("C-c K" . simply-kanban)
  ("C-c A" . simply-kanban-agenda))

(autoload 'org-dblock-write:simply-kanban-sprint-summary "simply-kanban"
  "Write a Simply Kanban sprint summary table." nil)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c ;") #'simply-kanban-show-card))

;;
;; -> quality-of-life
;;
(setq recentf-max-menu-items 40
      recentf-max-saved-items 40
      max-mini-window-height 8
      tab-bar-auto-width-max '((120) 20))

;; Slight transparency on the frame background. Harmless in TTY
;; environments that don't support it.
(set-frame-parameter nil 'alpha-background 90)
(add-to-list 'default-frame-alist '(alpha-background . 90))

;; `repeat-mode' is intentionally left off; enable it with `M-x repeat-mode'.

(use-package chess
  :defer t
  :custom
  (chess-images-separate-frame nil)
  (chess-images-default-size 64))

(use-package gnuplot
  :defer t)

(use-package htmlize
  :defer t)

;;
;; -> machine-specific paths (GNU/Linux)
;;
(when (eq system-type 'gnu/linux)
  (define-key my-jump-keymap (kbd "a")
    (lambda () (interactive) (find-file "~/DCIM/content/emacs.org")))
  (define-key my-jump-keymap (kbd "c")
    (lambda () (interactive) (find-file "~/DCIM/content/aaa--calendar.org")))
  (define-key my-jump-keymap (kbd "f")
    (lambda () (interactive) (find-file "~/nas")))
  (define-key my-jump-keymap (kbd "m")
    (lambda () (interactive) (find-file "~/DCIM/Camera")))
  (define-key my-jump-keymap (kbd "j")
    (lambda () (interactive) (find-file "~/DCIM/content/aaa--todo.org")))
  (define-key my-jump-keymap (kbd "n")
    (lambda () (interactive) (find-file "~/DCIM/Screenshots")))
  (define-key my-jump-keymap (kbd "w")
    (lambda () (interactive) (find-file "~/DCIM/content/")))
  ;; Sway/Waybar stores its config as JSON with comments.
  (add-to-list 'auto-mode-alist '("waybar.*/config\\'" . js-json-mode))
  (setq diary-file "~/DCIM/content/diary.org"))
