;; -*- lexical-binding: t; -*-
;;
;; Starter customisations for Emacs 27.2 — optional, unused by default.
;;
;; Distilled from the upstream author's live init.el. Supplements (does not
;; replace) the bundled init.el, which already installs the ELPA mirror,
;; bootstraps ~/.emacs.d/local-packages, and loads the Emacs-vanilla base.
;;
;; Coding-oriented blocks (eglot, dape, emeld-sidebar, demap, etc.) live in
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

;; (load-theme 'timu-spacegrey t)   ; uncomment to pick one

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

(with-eval-after-load 'selected-window-accent-mode
  (global-set-key (kbd "C-c w") selected-window-accent-map))

;;
;; -> simply-annotate — overlay notes on any file
;;
(use-package simply-annotate
  :hook (find-file-hook . simply-annotate-mode)
  :config
  (global-set-key (kbd "M-s") simply-annotate-command-map)
  (setq simply-annotate-inline-position 'above)
  (setq simply-annotate-tint-amount 20)
  (setq simply-annotate-inline-pointer-above "┗━▶")
  (setq simply-annotate-inline-pointer-after "┏━▶")
  (setq simply-annotate-database-strategy 'both)
  (setq simply-annotate-inline-default t))


;;
;; -> diff-minimap — minimap sidebar showing diff-hl regions
;;
(use-package diff-minimap :demand t)

;;
;; -> transmute — media management utilities (image, video, audio)
;;
(use-package transmute
  :demand t
  :bind (("C-c I" . transmute-menu))
  :config
  (with-eval-after-load 'image-dired
    (transmute-setup-thumbnail-keys)))

;;
;; -> org-bootstrap-publish — generate Bootstrap 5 sites from Org files
;;
(use-package org-bootstrap-publish :demand t)

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
