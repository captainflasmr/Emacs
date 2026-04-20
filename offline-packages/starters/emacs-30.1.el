;; -*- lexical-binding: t; -*-
;;
;; Starter customisations for Emacs 30.1 — optional, unused by default.
;;
;; Distilled from the upstream author's live init.el. Supplements (does not
;; replace) the bundled init.el, which already installs the ELPA mirror,
;; bootstraps ~/.emacs.d/local-packages, and loads the Emacs-vanilla base.
;;
;; To try it, either:
;;   (a) append this line to ~/.emacs.d/init.el:
;;         (load (expand-file-name "init-starter" user-emacs-directory) t t)
;;   (b) or cherry-pick blocks into your own init.el.
;;
;; Assumes `use-package' is installed (bundled via the offline mirror, and
;; also built in to Emacs 29+). `:ensure' is left off deliberately — the
;; bundled init.el already `my/ensure-package'd everything from
;; packages/emacs-30.1.el.

(require 'use-package)
(setq use-package-always-ensure nil)

;;
;; -> themes (pick one to enable at startup, or just M-x load-theme)
;;
(use-package doom-themes)
(use-package ef-themes)
(use-package modus-themes)
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

;; (load-theme 'ef-dark t)          ; uncomment to pick one

;;
;; -> mode hints for file types
;;
(use-package yaml-mode)
(use-package csv-mode)
(use-package i3wm-config-mode)
(use-package protobuf-mode)

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
;; -> diff-hl — inline VCS indicators in fringe (or margin in TTY)
;;
(use-package diff-hl
  :hook (dired-mode . diff-hl-dired-mode)
  :config
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode 1)
  (unless (display-graphic-p)
    (diff-hl-margin-mode 1)))

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
;; -> ztree — directory/file tree diff
;;
(use-package ztree
  :config
  (setq-default ztree-diff-filter-list
                '("build" "\\.dll" "\\.iso" "\\.cache" "\\.git"
                  "^elpa$" "^eln-cache$" "bin" "obj"))
  (setq-default ztree-diff-consider-file-size t)
  (setq-default ztree-diff-show-equal-files nil))

;;
;; -> dape — debug adapter protocol client (gdb, lldb, netcoredbg, jdtls, ...)
;;
(use-package dape
  :bind
  (("<f5>"    . dape)
   ("<S-f5>"  . dape-kill)
   ("<f9>"    . dape-breakpoint-toggle)
   ("<f10>"   . dape-next)
   ("<f11>"   . dape-step-in)
   ("<S-f11>" . dape-step-out)
   ("<C-f5>"  . dape-restart))
  :config
  (setq dape-request-timeout 30))

;;
;; -> package-lint — M-x package-lint-current-buffer to audit your own packages
;;
(use-package package-lint)

;;
;; -> ollama-buddy — AI chat against a local ollama server or cloud providers
;; Needs `ollama serve' running locally, or cloud API keys in auth-source.
;;
(use-package ollama-buddy
  :bind
  ("C-c o" . ollama-buddy-role-transient-menu)
  ("C-c O" . ollama-buddy-transient-menu)
  :config
  (setq ollama-buddy-default-model "llama3.2:latest")
  (setq ollama-buddy-max-history-length 999))

;;
;; -> quality-of-life
;;
(setq recentf-max-menu-items 40
      recentf-max-saved-items 40
      max-mini-window-height 6
      tab-bar-auto-width-max '((120) 20))

(when (fboundp 'repeat-mode) (repeat-mode 1))
