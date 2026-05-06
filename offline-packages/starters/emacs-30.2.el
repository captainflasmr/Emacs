;; -*- lexical-binding: t; -*-
;;
;; Starter customisations for Emacs 30.2 — optional, unused by default.
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
  ;; :hook (dired-mode . diff-hl-dired-mode) ; conflicts with emacs-solo git status overlays
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
;; -> treemacs — project sidebar with current-project toggle and enhanced config
;;
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

(use-package treemacs
  :demand t
  :bind ("C-x m" . my/treemacs-toggle-current-project)
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (setq treemacs-pulse-on-success t
        treemacs-follow-after-init t
        treemacs-indentation 2
        treemacs-no-png-images t
        treemacs-text-scale -1
        treemacs-show-cursor nil)
  (set-face-attribute 'treemacs-root-face nil
                      :inherit 'font-lock-constant-face
                      :underline t
                      :weight 'bold
                      :height 1.0))

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
  "Run `my/treemacs-update-current-line' in the treemacs buffer."
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
              (let ((type (overlay-get ov 'diff-hl)))
                (when type
                  (let* ((beg (overlay-start ov))
                         (end (overlay-end ov))
                         (face (pcase type
                                 ('insert 'my/demap-diff-added)
                                 ('change 'my/demap-diff-modified)
                                 ('delete 'my/demap-diff-removed)
                                 (_ nil)))
                         (new-ov (make-overlay beg end minimap-buf)))
                    (when face
                      (overlay-put new-ov 'face face)
                      (overlay-put new-ov 'my/demap-diff t)
                      (overlay-put new-ov 'priority 0)))))))))))

  (defun my/demap-diff-clear ()
    "Remove all diff overlays from the current minimap buffer."
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'my/demap-diff)
        (delete-overlay ov))))

  (defun my/demap-diff-update-current ()
    "Update diff overlays for the current minimap."
    (when (demap-buffer-minimap)
      (my/demap-diff-update (demap-buffer-minimap))))

  (defun my/demap-diff-schedule-update ()
    "Schedule a diff update for the minimap after diff-hl runs."
    (run-with-idle-timer 0.1 nil #'my/demap-diff-update-current))

  (with-eval-after-load 'diff-hl
    (add-hook 'diff-hl-after-change-hook #'my/demap-diff-schedule-update)
    (add-hook 'diff-hl-after-revert-hook #'my/demap-diff-schedule-update)
    (add-hook 'diff-hl-after-checkout-hook #'my/demap-diff-schedule-update))

  (add-hook 'demap-minimap-construct-hook
            (lambda ()
              (my/demap-diff-update-current)
              (add-hook 'post-command-hook #'my/demap-diff-update-current nil t))
            nil t))

;;
;; -> windmove repeat map — h/j/k/l navigation repeats after C-M- prefix
;;
(put 'windmove-left  'repeat-map 'windmove-repeat-map)
(put 'windmove-right 'repeat-map 'windmove-repeat-map)
(put 'windmove-up    'repeat-map 'windmove-repeat-map)
(put 'windmove-down  'repeat-map 'windmove-repeat-map)

(defvar windmove-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "h") #'windmove-left)
    (define-key map (kbd "j") #'windmove-down)
    (define-key map (kbd "k") #'windmove-up)
    (define-key map (kbd "l") #'windmove-right)
    map))

;;
;; -> quality-of-life
;;
(setq recentf-max-menu-items 40
      recentf-max-saved-items 40
      max-mini-window-height 6
      tab-bar-auto-width-max '((120) 20))

(when (fboundp 'repeat-mode) (repeat-mode 1))
