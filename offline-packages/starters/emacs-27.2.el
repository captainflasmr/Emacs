;; -*- lexical-binding: t; -*-
;;
;; Starter customisations for Emacs 27.2 — optional, unused by default.
;;
;; Distilled from the upstream author's live init.el. Supplements (does not
;; replace) the bundled init.el, which already installs the ELPA mirror,
;; bootstraps ~/.emacs.d/local-packages, and loads Emacs-vanilla + Emacs-DIYer.
;;
;; Coding-oriented blocks (eglot, dape, emeld-sidebar, etc.) live in
;; starters/coding.el (version-agnostic, with fboundp guards for 29+
;; features). Load both to get the full set:
;;   (load (expand-file-name "init-starter" user-emacs-directory) t t)
;;   (load (expand-file-name "init-starter-coding" user-emacs-directory) t t)
;;
;; To try it standalone:
;;   (a) append this line to ~/.emacs.d/init.el:
;;         (load (expand-file-name "init-starter" user-emacs-directory) t t)
;;   (b) or cherry-pick blocks into your own init.el.
;;
;; Assumes `use-package' is installed (it is, via the offline mirror).
;; `:ensure' is left off deliberately — the bundled init.el already
;; `my/ensure-package'd everything from packages/emacs-27.2.el.

(require 'use-package)
(setq use-package-always-ensure nil)

;;
;; -> themes (pick one to enable at startup, or just M-x load-theme)
;;
(use-package doom-themes)
(use-package gruvbox-theme)
(use-package timu-caribbean-theme)

(use-package timu-spacegrey-theme
  :config
  (setq timu-spacegrey-scale-org-document-title 1.8)
  (setq timu-spacegrey-scale-org-document-info 1.4)
  (setq timu-spacegrey-scale-org-level-1 1.8)
  (setq timu-spacegrey-scale-org-level-2 1.4)
  (setq timu-spacegrey-scale-org-level-3 1.2))

(use-package timu-rouge-theme
  :config
  (setq timu-rouge-mode-line-border t)
  (setq timu-rouge-scale-org-document-title 1.8)
  (setq timu-rouge-scale-org-document-info 1.4)
  (setq timu-rouge-scale-org-level-1 1.8)
  (setq timu-rouge-scale-org-level-2 1.4)
  (setq timu-rouge-scale-org-level-3 1.2))

;;
;; -> favourite-themes
;;
;; Pick one at a time; the others stay commented so they are easy to
;; swap.  `gruvbox-dark-hard' comes from `gruvbox-theme'; `ef-trio-dark'
;; requires `ef-themes', which is not in this version's mirror.
;;
;; (load-theme 'gruvbox-dark-hard t)
(load-theme 'doom-tokyo-night t)
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
                       (mode-line-inactive 1.4)))
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
(use-package yaml-mode)
(use-package csv-mode)
(use-package i3wm-config-mode)

(use-package web-mode
  :mode "\\.cshtml?\\'"
  :hook (html-mode . web-mode)
  :bind (:map web-mode-map ("M-;" . nil)))

(use-package plantuml-mode
  :mode ("\\.puml\\'" "\\.plantuml\\'" "\\.iuml\\'")
  :custom
  (plantuml-jar-path "~/.emacs.d/local-packages/ox-plantuml-gantt/plantuml.jar")
  (plantuml-default-exec-mode 'jar)
  (plantuml-output-type "png"))

(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.cshtml\\'" . html-mode))
(add-to-list 'auto-mode-alist '("/sway/config\\'" . i3wm-config-mode))
(add-to-list 'auto-mode-alist '("/sway/.*config.*/" . i3wm-config-mode))

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
;;
;; Other useful options:
;;   (setq vlf-tune-enabled nil)         ; don't auto-tune batch size
;;   (setq vlf-tune-max (* 4 1024 1024)) ; cap tuned batch size at 4 MB
;;   (setq vlf-save-in-place 'ask)       ; write via temp file on resize
(use-package vlf
  :commands vlf
  :init
  (require 'vlf-setup)
  :custom
  (vlf-application 'dont-ask))

;;
;; -> selected-window-accent-mode — highlights the active window's borders
;;
(use-package selected-window-accent-mode
  :config (selected-window-accent-mode 1)
  :custom
  (selected-window-accent-fringe-thickness 10)
  (selected-window-accent-mode-style 'default)
  (selected-window-accent-percentage-darken 20)
  (selected-window-accent-percentage-desaturate 20)
  (selected-window-accent-tab-accent t)
  (selected-window-accent-smart-borders nil))

(define-key my-overrides-mode-map (kbd "C-c w") selected-window-accent-map)

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
;; -> diff-minimap — minimap sidebar showing diff-hl regions
;;
(use-package diff-minimap :demand t)

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
  :demand t
  :config
  (define-key my-overrides-mode-map (kbd "C-c I") #'transmute-menu)
  (with-eval-after-load 'image-dired
    (transmute-setup-thumbnail-keys)))

;;
;; -> org-bootstrap-publish — generate Bootstrap 5 sites from Org files
;;
(use-package org-bootstrap-publish :demand t)

;;
;; -> org-table-fit — fit Org tables to the window width
;;
;; Vendored under local-packages/org-table-fit; no ELPA archive needed.
;; `org-table-fit-overlay-mode' wraps wide tables with overlays without
;; changing the buffer text; `org-table-fit-window' rewrites the table at
;; point and `org-table-fit-unwrap' restores it.  Org 9.7 removed the
;; `org-table-map' prefix keymap (C-c | is now a direct binding), so these
;; live on the free C-c t prefix.  Requires Emacs 29.1; the block is
;; skipped on 27.x/28.x.
;;
(unless (and (boundp 'my/org-table-fit-configured)
             my/org-table-fit-configured)
  (when (and (locate-library "org-table-fit")
             (version<= "29.1" emacs-version))
    (use-package org-table-fit
      :ensure nil
      :load-path ("offline-packages/local-packages/org-table-fit"
                  "local-packages/org-table-fit")
      :demand t
      :hook (org-mode . org-table-fit-overlay-mode)
      :bind (:map org-mode-map
                  ("C-c t f" . org-table-fit-window)
                  ("C-c t i" . org-table-fit-window)
                  ("C-c t t" . org-table-fit-overlay-mode)
                  ("C-c t u" . org-table-fit-unwrap)
                  ("C-c t r" . org-table-fit-toggle-reveal-on-point))
      :init
      (setq my/org-table-fit-configured t)
      :config
      (setq org-table-fit-overlay-reveal-on-point nil))))

;;
;; -> org-ibullets — render Org heading stars as UTF-8 bullets
;;
;; Vendored under local-packages/org-ibullets; no ELPA archive needed.
;; Disabling font-cache compaction keeps the composed bullets fast.
;;
(unless (and (boundp 'my/org-ibullets-configured)
             my/org-ibullets-configured)
  (use-package org-ibullets
    :ensure nil
    :load-path ("offline-packages/local-packages/org-ibullets"
                "local-packages/org-ibullets")
    :commands org-ibullets-mode
    :init
    (setq my/org-ibullets-configured t
          inhibit-compacting-font-caches t)
    :hook (org-mode . org-ibullets-mode)))

;;
;; -> org-slides — present an Org buffer, one top-level heading per slide
;;
;; Vendored under local-packages/org-slides; no ELPA archive needed.
;; C-c P starts, n/SPC/right go forward, p/S-SPC/left go back, and q quits.
;;
(unless (and (boundp 'my/org-slides-configured)
             my/org-slides-configured)
  (use-package org-slides
    :ensure nil
    :load-path ("offline-packages/local-packages/org-slides"
                "local-packages/org-slides")
    :commands org-slides-start
    :init
    (setq my/org-slides-configured t)
    (with-eval-after-load 'org
      (define-key org-mode-map (kbd "C-c P") #'org-slides-start))))

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
;; -> quality-of-life
;;
(setq recentf-max-menu-items 40
      recentf-max-saved-items 40
      max-mini-window-height 8)

(use-package chess)

(setq chess-images-separate-frame nil)
(setq chess-images-default-size 64)

(when (eq system-type 'gnu/linux)
  (define-key my-jump-keymap (kbd "c") (lambda () (interactive) (find-file "~/DCIM/content/aaa--calendar.org")))
  (define-key my-jump-keymap (kbd "m") (lambda () (interactive) (find-file "~/DCIM/Camera")))
  (define-key my-jump-keymap (kbd "j") (lambda () (interactive) (find-file "~/DCIM/content/aaa--todo.org")))
  (define-key my-jump-keymap (kbd "n") (lambda () (interactive) (find-file "~/DCIM/Screenshots")))
  (define-key my-jump-keymap (kbd "w") (lambda () (interactive) (find-file "~/DCIM/content/")))
  (setq diary-file "~/DCIM/content/diary.org"))

(use-package gnuplot)
