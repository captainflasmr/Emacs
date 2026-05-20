;; -*- lexical-binding: t; -*-
;;
;; Starter customisations for Emacs 30.2 — optional, unused by default.
;;
;; Distilled from the upstream author's live init.el. Supplements (does not
;; replace) the bundled init.el, which already installs the ELPA mirror,
;; bootstraps ~/.emacs.d/local-packages, and loads the Emacs-vanilla base.
;;
;; Coding-oriented blocks (eglot, dape, treemacs, demap, protobuf-mode, etc.)
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
;; also built in to Emacs 29+). `:ensure' is left off deliberately — the
;; bundled init.el already `my/ensure-package'd everything from
;; packages/emacs-30.2.el.

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
  :load-path "~/.emacs.d/local-packages/ztree"
  :ensure nil
  :custom
  (ztree-indent-step 2)
  (ztree-draw-unicode-lines t)
  :config
  (setq-default ztree-diff-filter-list
                '("\.class" "^tmp$" "^.idea$"
                  "build" "\.dll" "\.iso" "\.xmp" "\.cache" "\.gnupg" "\.local"
                  "\.mozilla" "\.thunderbird" "\.wine" "\.mp3" "\.mp4" "\.arpack"
                  "\.git" "^Volume$" "^Games$" "^cache$" "^chromium$" "^elpa$" "^nas$"
                  "^syncthing$" "bin" "obj"))
  ;; (setq-default ztree-diff-additional-options '("-w" "-i"))
  (setq-default ztree-diff-consider-file-size t)
  (setq-default ztree-diff-consider-file-permissions nil)
  (setq-default ztree-diff-show-equal-files nil)

  ;; Bind 'g' to full rescan in diff mode
  (with-eval-after-load 'ztree-diff
    (define-key ztree-mode-map (kbd "g") 'ztree-diff-full-rescan))

  ;; Helper: collect directories from visible dired windows
  (defun ztree-get-dired-directories ()
    "Get directories from all visible dired buffers."
    (let ((directories '()))
      (dolist (window (window-list))
        (with-current-buffer (window-buffer window)
          (when (eq major-mode 'dired-mode)
            (let ((dir (dired-current-directory)))
              (when dir
                (push (expand-file-name dir) directories))))))
      (reverse (delete-dups directories))))

  ;; Enhanced ztree-diff with DWIM directory suggestion
  (defun ztree-diff-dwim ()
    "Enhanced ztree-diff that suggests directories from dired windows."
    (interactive)
    (let* ((dired-dirs (ztree-get-dired-directories))
           (default-dir1 (or (car dired-dirs) default-directory))
           (default-dir2 (or (cadr dired-dirs) default-directory))
           (dir1 (read-directory-name
                  (format "First directory (default: %s): "
                          (file-name-nondirectory (directory-file-name default-dir1)))
                  default-dir1 default-dir1 t))
           (dir2 (read-directory-name
                  (format "Second directory (default: %s): "
                          (file-name-nondirectory (directory-file-name default-dir2)))
                  default-dir2 default-dir2 t)))
      (ztree-diff dir1 dir2)))

  (global-set-key (kbd "C-c z d") 'ztree-diff-dwim))

(with-eval-after-load 'ztree
  (define-key ztree-mode-map (kbd "u") #'ztree-previous-line)
  (define-key ztree-mode-map (kbd "n") #'ztree-next-line)
  (define-key ztree-mode-map (kbd "p") #'ztree-previous-line)
  (define-key ztree-mode-map (kbd "f") #'ztree-diff-view-file)
  (define-key ztree-mode-map (kbd "^") #'ztree-move-up-in-tree)
  (define-key ztree-mode-map (kbd "=") #'my/ztree-diff-ediff)
  (define-key ztree-mode-map (kbd "e") #'my/ztree-diff-ediff)
  (define-key ztree-mode-map (kbd "w") #'my/ztree-copy-filename-as-kill)

  (defun my/ztree-toggle-current-node ()
    "Toggle expand/collapse on the directory at point."
    (interactive)
    (let* ((line (line-number-at-pos))
           (node (ztree-find-node-in-line line)))
      (when (and node (ztree-node-expandable-p node))
        (ztree-toggle-expand-state node)
        (let ((current-pos (window-start)))
          (ztree-refresh-buffer line)
          (set-window-start (selected-window) current-pos)))))

  (defun my/ztree-copy-filename-as-kill (&optional full-path)
    "Copy the filename at point to the kill ring.
With prefix argument, copy the full absolute path.
Like `dired-copy-filename-as-kill' but for ztree-diff."
    (interactive "P")
    (let ((found (ztree-find-node-at-point)))
      (unless found
        (user-error "No file at point"))
      (let* ((node (car found))
             (side (cdr found))
             (path (if (eq side 'left)
                       (ztree-diff-node-left-path node)
                     (ztree-diff-node-right-path node))))
        (unless path
          (user-error "No file path on this side"))
        (let ((name (if full-path
                        (expand-file-name path)
                      (file-name-nondirectory path))))
          (kill-new name)
          (message "%s" name)))))

  (defun my/ztree-diff-ediff ()
    "Run ediff on the two sides of the file at point."
    (interactive)
    (let ((found (ztree-find-node-at-point)))
      (when found
        (let* ((node (car found))
               (left (ztree-diff-node-left-path node))
               (right (ztree-diff-node-right-path node)))
          (if (and left right)
              (ztree-diff-ediff left right)
            (message "File only exists on one side; nothing to ediff"))))))

  ;; Preserve point across tab history switches
  (defun my/ztree-save-all-points ()
    "Save point and window-start for all `ztree-mode' buffers."
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (derived-mode-p 'ztree-mode)
          (set (make-local-variable 'my/ztree-saved-point) (point))
          (let ((win (get-buffer-window buf t)))
            (when win
              (set (make-local-variable 'my/ztree-saved-window-start) (window-start win))))))))

  (defun my/ztree-restore-visible-points ()
    "Restore saved point and window-start for visible `ztree-mode' buffers."
    (dolist (win (window-list))
      (let ((buf (window-buffer win)))
        (with-current-buffer buf
          (when (and (derived-mode-p 'ztree-mode)
                     (boundp 'my/ztree-saved-point))
            (let ((p (min my/ztree-saved-point (point-max))))
              (with-selected-window win
                (goto-char p))
              (when (and (boundp 'my/ztree-saved-window-start)
                         (integerp my/ztree-saved-window-start))
                (set-window-start win (min my/ztree-saved-window-start (point-max))))))))))

  (advice-add 'ztree-diff-view-file :before #'my/ztree-save-all-points)
  (advice-add 'ztree-diff-simple-diff-files :before #'my/ztree-save-all-points)
  (advice-add 'ztree-diff-ediff :before (lambda (&rest _) (my/ztree-save-all-points)))
  (advice-add 'tab-bar-history-back :after #'my/ztree-restore-visible-points)
  (advice-add 'tab-bar-history-forward :after #'my/ztree-restore-visible-points))

;; Theme-aware ztree face remapping
(defun my/ztree-remap-faces ()
  "Map ztree/ztreep faces to theme faces for coherence with current theme."
  (dolist (fn (face-list))
    (let ((name (symbol-name fn)))
      (when (or (string-prefix-p "ztree" name)
                (string-prefix-p "ztreep" name))
        (cond
         ((string-match-p "model-add-face\\|add-face$" name)
          (set-face-attribute fn nil :foreground 'unspecified :background 'unspecified :inherit 'success))
         ((string-match-p "model-diff-face" name)
          (set-face-attribute fn nil :foreground 'unspecified :background 'unspecified :inherit 'error))
         ((string-match-p "model-ignored-face" name)
          (set-face-attribute fn nil :foreground 'unspecified :background 'unspecified :inherit 'font-lock-comment-face))
         ((string-match-p "model-normal-face" name)
          (set-face-attribute fn nil :foreground 'unspecified :background 'unspecified :inherit 'default))
         ((string-match-p "model-name" name)
          (set-face-attribute fn nil :foreground 'unspecified :background 'unspecified :inherit 'font-lock-function-name-face))
         ((string-match-p "header-face" name)
          (set-face-attribute fn nil :foreground 'unspecified :background 'unspecified :inherit 'font-lock-keyword-face))
         (t
          (set-face-attribute fn nil :foreground 'unspecified :background 'unspecified :inherit 'default)))))))

(advice-add 'load-theme :after (lambda (&rest _) (my/ztree-remap-faces)))
(when custom-enabled-themes (my/ztree-remap-faces))
(my/ztree-remap-faces)

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
