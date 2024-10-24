;;
;; -> package-archives
;;

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))

;; (setq package-archives '(("melpa" . "~/emacs-pkgs/melpa")
;;                           ("elpa" . "~/emacs-pkgs/elpa")
;;                           ("org" . "~/emacs-pkgs/org-mode/lisp")))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(require 'use-package)
(setq load-prefer-newer t)

;; (when init-file-debug
(setq use-package-verbose t
      use-package-expand-minimally nil
      use-package-compute-statistics t
      debug-on-error nil)

;;
;; -> requires
;;

(require 'org)
(require 'dired-x)
(require 'org-agenda)
(require 'cl-lib)

;;
;; -> startup
;;

(defun display-startup-time ()
  "Display startup time."
  (message "Emacs startup time: %s" (emacs-init-time)))

(add-hook 'emacs-startup-hook 'display-startup-time)

;;
;; -> use-package
;;
(use-package ahk-mode)
(use-package arscript-mode)
(use-package async)
(use-package diminish)
(use-package diredfl
  :init (diredfl-global-mode 1)
  :diminish diredfl-mode)
(use-package doom-themes)
(use-package ef-themes)
(use-package embark)
(use-package embark-consult)
(use-package free-keys)
(use-package git-timemachine)
(use-package gnuplot)
(use-package gruvbox-theme)
(use-package hl-sentence)
(use-package htmlize)
(use-package i3wm-config-mode)
(use-package lorem-ipsum)
(use-package markdown-mode)
(use-package org-kanban)
(use-package syntax-subword
  :init
  (global-syntax-subword-mode))
(use-package xkb-mode)

(use-package rainbow-mode
  :diminish rainbow-mode
  :hook
  (prog-mode . rainbow-mode)
  (conf-space-mode . rainbow-mode)
  (org-mode . rainbow-mode))

(use-package visual-fill-column
  :config
  (setq-default visual-fill-column-center-text t))

(use-package ox-hugo
  :defer t
  :config
  (setq org-hugo-front-matter-format "yaml"))

(use-package deadgrep
  :config
  (setq-default deadgrep--search-case 'ignore)
  :custom
  (deadgrep-max-buffers 1)
  (deadgrep-extra-arguments '("--no-config")))
;; (deadgrep-extra-arguments '("--no-config" "--no-ignore" "--no-ignore-vcs")))

(use-package ready-player
  :init
  (ready-player-mode 1)
  :custom
  (ready-player-thumbnail-max-pixel-height 200)
  (ready-player-autoplay nil)
  (ready-player-repeat t)
  (ready-player-shuffle t)
  (ready-player-open-playback-commands
   '((ready-player-is-audio-p "mplayer")
     (ready-player-is-video-p "mpv"))))

(use-package ibuffer
  :bind (:map ibuffer-mode-map
              ("j" . next-line)
              ("k" . previous-line)))

;;
;; -> completion
;;

(defun my/complete ()
  (interactive)
  (cond
   ;; Check if corfu is available and corfu-mode is active
   ((and (fboundp 'corfu-mode) (bound-and-true-p corfu-mode))
    (message "Using corfu for completion")
    (indent-for-tab-command)
    ;; (corfu-complete)
    )
   ;; Check if company is available and company-mode is active
   ((and (fboundp 'company-mode) (bound-and-true-p company-mode))
    (message "Using company for completion")
    (company-manual-begin))
   ;; Fallback
   (t
    (message "Using hippie-expand for completion")
    (hippie-expand nil))))

(use-package orderless
  :custom
  (completion-styles '(basic partial-completion orderless)))

(setq-default abbrev-mode t)

(global-set-key (kbd "M-/") 'hippie-expand)

(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially try-complete-file-name
                                         try-expand-all-abbrevs try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill
                                         try-complete-lisp-symbol-partially try-complete-lisp-symbol))

(use-package cape)

(use-package capf-autosuggest
  :hook
  (eshell-mode . capf-autosuggest-mode)
  (shell-mode . capf-autosuggest-mode))

(use-package eglot
  :hook
  (eglot-managed-mode
   . (lambda ()
       (setq-local completion-at-point-functions
                   (list (cape-capf-super
                          #'cape-dabbrev
                          #'eglot-completion-at-point)))))
  :custom
  (eglot-ignored-server-capabilities
   '(
     ;; :hoverProvider                    ; Provides information when you hover over code elements.
     ;; :completionProvider               ; Provides code completion suggestions.
     ;; :signatureHelpProvider            ; Offers signature information for functions/methods.
     ;; :definitionProvider               ; Finds the definition of variables/functions.
     ;; :typeDefinitionProvider           ; Finds the type definition of variables/functions.
     ;; :implementationProvider           ; Finds the implementation of types/functions.
     ;; :declarationProvider              ; Finds the declaration of variables/types.
     ;; :referencesProvider               ; Finds all references to the symbol at the caret.
     ;; :documentHighlightProvider        ; Highlights references to the symbol at the caret.
     ;; :documentSymbolProvider           ; Lists all symbols in a document.
     ;; :workspaceSymbolProvider          ; Lists symbols across workspace/project.
     ;; :codeActionProvider               ; Suggests code actions (like quick fixes).
     ;; :codeLensProvider                 ; Displays inline code actions or information.
     ;; :documentFormattingProvider       ; Formats an entire document.
     ;; :documentRangeFormattingProvider  ; Formats a specified range in a document.
     ;; :documentOnTypeFormattingProvider ; Formats code as you type.
     ;; :renameProvider                   ; Refactors/renames symbols.
     ;; :documentLinkProvider             ; Handles clickable links in documents.
     ;; :colorProvider                    ; Provides color information for document.
     ;; :foldingRangeProvider             ; Supports code folding.
     ;; :executeCommandProvider           ; Allows execution of commands.
     ;; :inlayHintProvider                ; Displays inline hints (e.g., parameter names).
     ))
  (eglot-send-changes-idle-time 2.0))

(use-package corfu
  :custom
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-auto nil)
  (corfu-separator ?\s)
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match nil)
  (corfu-preview-current nil)
  (corfu-preselect 'first)
  (corfu-on-exact-match nil)
  (corfu-scroll-margin 5))

(use-package company
  :bind
  (:map company-active-map
        ("M-j" . company-select-next)
        ("M-k" . company-select-previous)
        ("<tab>" . company-complete-selection))
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay nil))

(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

(use-package tempel
  :diminish tempel-abbrev-mode global-tempel-abbrev-mode abbrev-mode
  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))
  :init
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
  (add-hook 'conf-mode-hook #'tempel-setup-capf)
  (add-hook 'prog-mode-hook #'tempel-setup-capf)
  (add-hook 'text-mode-hook #'tempel-setup-capf)
  (global-tempel-abbrev-mode))

;;
;; -> modeline-completion-advanced
;;

(use-package vertico
  :init
  (vertico-mode 1)
  :custom
  (vertico-cycle t)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  (vertico-resize nil)
  (vertico-count 10)
  :bind (:map vertico-map
              ("C-n" . vertico-next)
              ("C-p" . vertico-previous)
              :repeat-map my/vertico-repeat-map
              ("n" . vertico-next)
              ("p" . vertico-previous)))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators
   '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

;;
;; -> keys-navigation
;;

(defvar my-jump-keymap (make-sparse-keymap))
(global-set-key (kbd "M-o") my-jump-keymap)

(define-key my-jump-keymap (kbd "-") #'tab-close)
(define-key my-jump-keymap (kbd "=") (lambda () (interactive) (tab-bar-new-tab-to -1)))
(define-key my-jump-keymap (kbd "e") (lambda () (interactive) (find-file (concat user-emacs-directory "init.el"))))
(define-key my-jump-keymap (kbd "h") (lambda () (interactive) (find-file "~")))
(define-key my-jump-keymap (kbd "k") (lambda () (interactive) (find-file (concat user-emacs-directory "emacs--init.org"))))
(define-key my-jump-keymap (kbd "l") #'recentf-open)
(define-key my-jump-keymap (kbd "m") #'customize-themes)
(define-key my-jump-keymap (kbd "o") #'bookmark-jump)
(define-key my-jump-keymap (kbd "r") #'scratch-buffer)
(define-key my-jump-keymap (kbd "t") #'customize-themes)
(define-key my-jump-keymap (kbd "z") #'list-packages)
(define-key my-jump-keymap (kbd "i") #'my/complete)


(use-package ace-window
  :config
  (setq aw-keys '(?j ?k ?l ?\; ?a ?s ?d ?f))
  (setq aw-background nil))

(bind-key* (kbd "M-a") #'ace-window)

;;
;; -> keys-visual
;;

(defvar my-win-keymap (make-sparse-keymap))
(global-set-key (kbd "C-q") my-win-keymap)

(define-key my-win-keymap (kbd "a") #'selected-window-accent-mode)
(define-key my-win-keymap (kbd "b") #'(lambda () (interactive)(tab-bar-mode 'toggle)))
(define-key my-win-keymap (kbd "c") #'display-fill-column-indicator-mode)
(define-key my-win-keymap (kbd "d") #'window-divider-mode)
(define-key my-win-keymap (kbd "e") #'whitespace-mode)
(define-key my-win-keymap (kbd "f") #'font-lock-mode)
(define-key my-win-keymap (kbd "g") #'my/toggle-scroll-margin)
(define-key my-win-keymap (kbd "h") #'global-hl-line-mode)
(define-key my-win-keymap (kbd "i") #'highlight-indent-guides-mode)
(define-key my-win-keymap (kbd "j") #'org-redisplay-inline-images)
(define-key my-win-keymap (kbd "l") #'my/sync-tab-bar-to-theme)
(define-key my-win-keymap (kbd "k") #'my/toggle-mode-line)
(define-key my-win-keymap (kbd "m") #'consult-theme)
(define-key my-win-keymap (kbd "n") #'display-line-numbers-mode)
(define-key my-win-keymap (kbd "o") #'visual-fill-column-mode)
(define-key my-win-keymap (kbd "p") #'variable-pitch-mode)
(define-key my-win-keymap (kbd "q") #'toggle-menu-bar-mode-from-frame)
(define-key my-win-keymap (kbd "s") #'my/toggle-internal-border-width)
(define-key my-win-keymap (kbd "v") #'visual-line-mode)

;;
;; -> keys-other
;;

(bind-key* (kbd "M-s ,") #'my/mark-line)
(bind-key* (kbd "M-s ;") #'mark-sexp)
(bind-key* (kbd "M-s =") #'ediff-buffers)
(bind-key* (kbd "M-s [") #'beginning-of-buffer)
(bind-key* (kbd "M-s ]") #'end-of-buffer)
(bind-key* (kbd "M-s c") #'cfw:open-org-calendar)
(bind-key* (kbd "M-s e") #'my/push-block)
(bind-key* (kbd "M-s f") #'my/find-file)
(bind-key* (kbd "M-s i") #'my/convert-markdown-clipboard-to-org)
(bind-key* (kbd "M-s k") #'org-kanban/shift)
(bind-key* (kbd "M-s l") #'mark-sexp)
(bind-key* (kbd "M-s m") #'org-preview-html-mode)
(bind-key* (kbd "M-s r") #'org-preview-html-refresh)
(bind-key* (kbd "M-s t") #'my/save-buffer-as-html)
(bind-key* (kbd "M-s v") #'eval-expression)
(bind-key* (kbd "M-s w") #'org-table-expand)
(bind-key* (kbd "M-s x") #'diff-buffer-with-file)
(bind-key* (kbd "M-s z") #'org-table-shrink)
(global-set-key (kbd "M-s M-[") #'beginning-of-buffer)
(global-set-key (kbd "M-s M-]") #'end-of-buffer)
(global-set-key (kbd "M-s b") #'my/dired-duplicate-backup-file)
(global-set-key (kbd "M-s g") #'my/grep)
(global-set-key (kbd "M-s h") #'my/mark-block)
(global-set-key (kbd "M-s j") #'eval-defun)

;;
;; -> magit
;;

(when (executable-find "git")
  (use-package magit
    :defer 5
    :config
    (magit-add-section-hook
     'magit-status-sections-hook 'magit-insert-tracked-files nil 'append)
    :custom
    (magit-section-initial-visibility-alist (quote ((untracked . hide))))
    (magit-repolist-column-flag-alist
     '((magit-untracked-files . "N")
       (magit-unstaged-files . "U")
       (magit-staged-files . "S")))
    (magit-repolist-columns
     '(("Name" 25 magit-repolist-column-ident nil)
       ("" 3 magit-repolist-column-flag)
       ("Version" 25 magit-repolist-column-version
        ((:sort magit-repolist-version<)))
       ("B<U" 3 magit-repolist-column-unpulled-from-upstream
        ((:right-align t)
         (:sort <)))
       ("B>U" 3 magit-repolist-column-unpushed-to-upstream
        ((:right-align t)
         (:sort <)))
       ("Path" 99 magit-repolist-column-path nil)))
    (magit-repository-directories
     '(("~/.config" . 0)
       ("~/source/repos" . 2)
       ("~/bin" . 1)
       ("~/.emacs.d" . 1)
       ("~/DCIM/Art/Content" . 2)
       ("~/DCIM/themes" . 2)))))

;;
;; -> emms
;;

(use-package emms
  :init
  (emms-all)
  :hook
  (emms-browser-mode . turn-on-follow-mode)
  (emms-browser-mode . hl-line-mode)
  :bind
  ("S-<return>" . emms-next)
  ("C-M-<return>" . emms-random)
  :custom
  (emms-default-players)
  (emms-player-list '(emms-player-vlc))
  (emms-browser-covers 'emms-browser-cache-thumbnail-async)
  (emms-source-file-default-directory "/run/media/jdyer/Backup/MyMusicLibrary")
  (emms-volume-amixer-card 1)
  (emms-volume-change-function 'emms-volume-pulse-change))

(require 'emms-setup)

;;
;; -> elfeed
;;

(use-package elfeed
  :bind
  ("C-x w" . elfeed)
  (:map elfeed-search-mode-map
        ("n" . (lambda () (interactive)
                 (forward-line 1) (call-interactively 'elfeed-search-show-entry)))
        ("p" . (lambda () (interactive)
                 (forward-line -1) (call-interactively 'elfeed-search-show-entry)))
        ("m" . (lambda () (interactive)
                 (apply 'elfeed-search-toggle-all '(star)))))
  :custom
  (elfeed-search-remain-on-entry t)
  (elfeed-search-title-min-width 60)
  (elfeed-search-title-max-width 60)
  (elfeed-search-filter "@1-months-ago")
  (elfeed-feeds
   '("https://www.dyerdwelling.family/index.xml"
     "https://www.emacs.dyerdwelling.family/index.xml"
     "https://www.emacs.dyerdwelling.family/tags/emacs/index.xml"
     "http://emacsninja.com/feed.atom"
     "http://www.omgubuntu.co.uk/feed"
     "http://feeds.feedburner.com/XahsEmacsBlog"
     "https://emacsair.me/feed.xml"
     "https://www.ghacks.net/feed/"
     "https://linuxstoney.com/feed"
     "http://emacsredux.com/atom.xml"
     "https://www.creativebloq.com/feed"
     "https://feeds.howtogeek.com/HowToGeek"
     "http://planet.emacslife.com/atom.xml"
     "http://irreal.org/blog/?feed=rss2"
     "https://itsfoss.com/feed/"
     "https://9to5linux.com/feed/atom"
     "https://opensource.com/feed"
     "http://www.masteringemacs.org/feed/"
     "https://jao.io/blog/rss.xml")))

(defun my/show-elfeed (buffer)
  "Show Elfeed wrapper with BUFFER."
  (display-buffer buffer))

(setq elfeed-show-mode-hook
      (lambda ()
        (set-face-attribute 'variable-pitch (selected-frame)
                            :font (font-spec :family "Source Code Pro" :size 16))
        (setq elfeed-show-entry-switch #'my/show-elfeed)))

;;
;; -> keybinding
;;

(bind-key* (kbd "C-+") (lambda ()(interactive)(text-scale-adjust 1)))
(bind-key* (kbd "C--") #'bookmark-jump)
(bind-key* (kbd "C--") (lambda ()(interactive)(text-scale-adjust -1)))
(bind-key* (kbd "C-0") #'my/switch-to-thing)
(bind-key* (kbd "C-=") #'switch-to-buffer)
(bind-key* (kbd "C-=") (lambda ()(interactive)(text-scale-adjust 1)))
(bind-key* (kbd "C-@") #'my/shell-create)
(bind-key* (kbd "C-c ,") #'embark-act)
(bind-key* (kbd "C-c r") #'my/repeat-window-size)
(bind-key* (kbd "C-o") #'other-window)
(bind-key* (kbd "C-x s") #'save-buffer)
(bind-key* (kbd "C-z") #'undo)
(bind-key* (kbd "M-'") #'set-mark-command)
(bind-key* (kbd "M-9") #'my/complete)
(bind-key* (kbd "M-SPC") #'set-mark-command)
(bind-key* (kbd "M-h") #'backward-char)
(bind-key* (kbd "M-j") #'next-line)
(bind-key* (kbd "M-k") #'previous-line)
(bind-key* (kbd "M-l") #'forward-char)
(bind-key* (kbd "M-m") #'(lambda ()(interactive)(scroll-down (/ (window-height) 4))))
(bind-key* (kbd "M-n") #'(lambda ()(interactive)(scroll-up (/ (window-height) 4))))
(define-key minibuffer-local-map (kbd "C-c c") #'embark-collect)
(define-key minibuffer-local-map (kbd "C-c e") #'embark-export)
(global-set-key (kbd "C-1") #'delete-other-windows)
(global-set-key (kbd "C-2") #'split-window-vertically)
(global-set-key (kbd "C-3") #'split-window-horizontally)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c b") #'(lambda ()(interactive)(async-shell-command "do_backup home" "*backup*")))
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-x l") #'scroll-lock-mode)
(global-set-key (kbd "C-x v e") 'vc-ediff)
(global-set-key (kbd "M--") #'bookmark-jump)
(global-set-key (kbd "M-0") #'delete-window)
(global-set-key (kbd "M-;") #'my/comment-or-uncomment)
(global-set-key (kbd "M-[") #'yank)
(global-set-key (kbd "M-]") #'yank-pop)
(global-unset-key (kbd "C-h h"))
(global-unset-key (kbd "C-t"))
(global-unset-key (kbd "C-x m"))

(global-set-key (kbd "M-g o") 'consult-outline)
(global-set-key (kbd "M-g i") 'consult-imenu)

(consult-customize
 consult-theme :preview-key '(:debounce 0.2 any)
 consult-recent-file consult-buffer consult-outline consult-imenu consult-history :preview-key nil)

;;
;; -> modes
;;
(global-hl-line-mode -1)
(global-font-lock-mode 1)
(savehist-mode 1)
(add-to-list 'savehist-additional-variables 'comint-input-ring)
(global-ede-mode -1)
(global-prettify-symbols-mode t)
(auto-fill-mode -1)
(blink-cursor-mode -1)
(column-number-mode 1)
(global-auto-revert-mode 1)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled -1)
(show-paren-mode 1)
(setq tooltip-mode nil)
(transient-mark-mode 1)
(pixel-scroll-precision-mode -1)

;;
;; -> bell
;;

(setq visible-bell t)
(setq ring-bell-function 'ignore)

;;
;; -> setqs
;;

(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

(setq auto-revert-use-notify nil)
(setq auto-revert-verbose nil)
(setq case-fold-search t)
(setq create-lockfiles nil)
(setq custom-safe-themes t)
(setq delete-by-moving-to-trash t)
(setq disabled-command-function nil)
(setq enable-local-variables :all)
(setq european-calendar-style t)
(setq fit-window-to-buffer-horizontally t)
(setq flymake-show-diagnostics-at-end-of-line t)
(setq frame-inhibit-implied-resize t)
(setq global-auto-revert-non-file-buffers t)
(setq grep-command "grep -ni ")
(setq isearch-lazy-count t)
(setq kill-buffer-query-functions nil)
(setq kill-whole-line t)
(setq large-file-warning-threshold nil)
(setq native-comp-async-report-warnings-errors nil)
(setq reb-re-syntax 'string)
(setq sentence-end-double-space nil)
(setq shr-ignore-cache t)
(setq shr-max-image-proportion 0.8)
(setq shr-max-width 80)
(setq shr-width 70)
(setq suggest-key-bindings nil)
(setq switch-to-buffer-obey-display-actions t)
(setq tooltip-hide-delay 0)
(setq tramp-default-method "ssh")
(setq truncate-lines t)
(setq use-dialog-box nil)
(setq use-short-answers t)

;;
;; -> confirm
;;

(defalias 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'y-or-n-p)
(setq confirm-kill-processes nil)
(setq confirm-nonexistent-file-or-buffer nil)
(set-buffer-modified-p nil)

;;
;; -> backups
;;

(setq make-backup-files 1)
(setq backup-directory-alist '(("." . "~/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 10   ; how many of the newest versions to keep
      kept-old-versions 5)   ; and how many of the old

;;
;; -> hooks
;;

(defun my/after-theme-loaded(theme)
  (my/sync-tab-bar-to-theme))

(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'calendar-mode-hook #'diary-mark-entries)
(add-hook 'chatgpt-shell-mode-hook #'visual-line-mode)
(add-hook 'diary-list-entries-hook #'diary-include-other-diary-files)
(add-hook 'diary-mark-entries-hook #'diary-mark-included-diary-files)
(add-hook 'enable-theme-functions #'my/after-theme-loaded)
(add-hook 'next-error-hook #'org-fold-show-all)

;;
;; -> custom-settings
;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(gruvbox))
 '(warning-suppress-log-types '((frameset)))
 '(warning-suppress-types '((frameset))))

;;
;; -> defun
;;

(defun my/resize-window (delta &optional horizontal)
  "Resize window back and forth by DELTA and HORIZONTAL."
  (interactive)
  (enlarge-window delta horizontal))

(defun save-macro (name)
  "Save a macro by NAME."
  (interactive "SName of the macro: ")
  (kmacro-name-last-macro name)
  (find-file user-init-file)
  (goto-char (point-max))
  (newline)
  (insert-kbd-macro name)
  (newline))

(defun my/comment-or-uncomment ()
  "Comment or uncomment the current line or region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region
       (region-beginning)(region-end))
    (comment-or-uncomment-region
     (line-beginning-position)(line-end-position))))

(defun my/grep (arg)
  "Wrapper to grep with ARG."
  (interactive "p")
  (let ((search-term
         (if (equal major-mode 'dired-mode)
             (read-from-minibuffer "Search : ")
           (read-from-minibuffer "Search : " (thing-at-point 'symbol)))))
    (if (= arg 1)
        (deadgrep search-term default-directory)
      (progn
        (setq current-prefix-arg nil)
        (deadgrep search-term "~")))))

(defun my/dired-duplicate-file (arg)
  "Duplicate a file from DIRED with an incremented number.
If ARG is provided, it sets the counter."
  (interactive "p")
  (let* ((file (dired-get-file-for-visit))
         (dir (file-name-directory file))
         (name (file-name-nondirectory file))
         (base-name (file-name-sans-extension name))
         (extension (file-name-extension name t))
         (counter (if arg (prefix-numeric-value arg) 1))
         (new-file))
    (while (and (setq new-file
                      (format "%s%s_%03d%s" dir base-name counter extension))
                (file-exists-p new-file))
      (setq counter (1+ counter)))
    (if (file-directory-p file)
        (copy-directory file new-file)
      (copy-file file new-file))
    (dired-revert)))

(defun convert-weight (weight)
  "Convert WEIGHT from string to pounds."
  (let* ((parts (split-string weight ":"))
         (stone (string-to-number (car parts)))
         (pounds (string-to-number (cadr parts))))
    (+ (* stone 14) pounds)))

(defun my/mark-line ()
  "Mark whole line."
  (interactive)
  (beginning-of-line)
  (push-mark (point) nil t)
  (end-of-line))

(defun my/mark-block ()
  "Marking a block of text surrounded by a newline."
  (interactive)
  (when (not (region-active-p))
    (backward-char))
  (skip-chars-forward " \n\t")
  (re-search-backward "^[ \t]*\n" nil 1)
  (skip-chars-forward " \n\t")
  (when (not (region-active-p))
    (push-mark))
  (re-search-forward "^[ \t]*\n" nil 1)
  (skip-chars-backward " \n\t")
  (setq mark-active t))

(defun my/text-browser-search ()
  "Use the selected text (or word under cursor)
as search term for Google search in web browser."
  (interactive)
  (let (search-term start end)
    ;; Check if text is selected, otherwise use the word at the cursor position
    (if (use-region-p)
        (setq start (region-beginning)
              end (region-end))
      (setq start (beginning-of-thing 'word)
            end (end-of-thing 'word)))
    ;; Extract the search term and urlencode it
    (setq search-term (buffer-substring-no-properties start end))
    (setq search-term (replace-regexp-in-string "[[:space:]\n]+" "+" search-term))
    ;; Open in an external browser
    (browse-url (concat "https://www.startpage.com/search?q=" search-term))))

(defun my/toggle-scroll-margin (&optional value)
  "Toggle the scroll margin based on VALUE."
  (interactive "P")
  (let ((new-value (if value
                       value
                     (if (= (or scroll-margin 0) 0)
                         20
                       0))))
    (setq scroll-margin new-value)))

(defun my/clear-recentf-list ()
  "Clears the recentf list."
  (interactive)
  (setq recentf-list nil)
  (recentf-save-list)
  (message "Cleared recent files list"))

(defun my/shell-create (name)
  "Create a custom-named eshell buffer with NAME."
  (interactive "sName: ")
  (eshell 'new)
  (let ((new-buffer-name (concat "*eshell-" name "*")))
    (rename-buffer new-buffer-name t)))

(defun my/repeat-window-size ()
  "Call FUNC and set up a sparse keymap for repeating actions."
  (interactive)
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "h") (lambda () (interactive)
                                (my/resize-window 2 t)
                                (my/repeat-window-size)))
    (define-key map (kbd "l") (lambda () (interactive)
                                (my/resize-window -2 t)
                                (my/repeat-window-size)))
    (define-key map (kbd "j") (lambda () (interactive)
                                (my/resize-window 1 nil)
                                (my/repeat-window-size)))
    (define-key map (kbd "k") (lambda () (interactive)
                                (my/resize-window -1 nil)
                                (my/repeat-window-size)))
    (set-transient-map map t)))

(defun my/switch-to-thing ()
  "Switch to a buffer, open a recent file, jump to a bookmark,
                   or change the theme from a unified interface."
  (interactive)
  (let* ((buffers (mapcar #'buffer-name (buffer-list)))
         (recent-files recentf-list)
         (bookmarks (bookmark-all-names))
         (all-options (append buffers recent-files bookmarks))
         (selection (completing-read "Switch to: "
                                     (lambda (str pred action)
                                       (if (eq action 'metadata)
                                           '(metadata . ((category . file)))
                                         (complete-with-action action all-options str pred)))
                                     nil t nil 'file-name-history)))
    (pcase selection
      ((pred (lambda (sel) (member sel buffers))) (switch-to-buffer selection))
      ((pred (lambda (sel) (member sel bookmarks))) (bookmark-jump selection))
      (_ (find-file selection)))))

(defun my/convert-markdown-clipboard-to-org ()
  "Convert Markdown content from clipboard to Org format and insert it at point."
  (interactive)
  (let ((markdown-content (current-kill 0))
        (output-buffer (get-buffer-create "*markdown-to-org-output*"))
        (original-buffer (current-buffer)))
    (with-temp-buffer
      (insert markdown-content)
      (call-process-region (point-min) (point-max) "pandoc" nil output-buffer nil
                           "-f" "markdown" "-t" "org"))
    (with-current-buffer output-buffer
      (let ((org-content (buffer-string)))
        (setq org-content (replace-regexp-in-string
                           "^:PROPERTIES:\n.*\n.*:END:" "" org-content))
        ;; Replace erroneous code block conversion
        ;; (setq org-content (replace-regexp-in-string
        ;; "^=elisp"
        ;; "#+begin_src elisp\n"
        ;; org-content))
        ;; (setq org-content (replace-regexp-in-string
        ;; "=$"
        ;; "\n#+end_src"
        ;; org-content))
        (with-current-buffer original-buffer
          (insert org-content))))
    (kill-buffer output-buffer)))

(defun my/sync-tab-bar-to-theme ()
  "Synchronize tab-bar faces with the current theme."
  (interactive)
  (let ((default-bg (face-background 'default))
        (default-fg (face-foreground 'default))
        (inactive-fg (face-foreground 'mode-line-inactive))) ;; Fallback to mode-line-inactive
    (custom-set-faces
     `(tab-bar ((t (:inherit default :background ,default-bg :foreground ,default-fg))))
     `(tab-bar-tab ((t (:inherit default :background ,default-fg :foreground ,default-bg))))
     `(tab-bar-tab-inactive ((t (:inherit default :background ,default-bg :foreground ,inactive-fg)))))))

(defun my/dired-file-to-org-link ()
  "Transform the file path under the cursor in Dired to an Org mode
link and copy to kill ring.
This function transforms the current file path in Dired mode into
an Org link with attributes for both org-mode and HTML width
settings. The generated link is then copied to the kill ring for
easy pasting."
  (interactive)
  (let ((file-path (dired-get-file-for-visit)))
    (if file-path
        (let* ((relative-path (file-relative-name file-path
                                                  (project-root (project-current t))))
               (org-link (concat "#+attr_org: :width 300px\n"
                                 "#+attr_html: :width 100%\n"
                                 "[[file:" relative-path "]]\n")))
          (kill-new org-link)
          (message "Copied to kill ring: %s" org-link))
      (message "No file under the cursor"))))

(defun my/dired-duplicate-backup-file (arg)
  "Duplicate a file to a backup directory with an incremented number.
If ARG is provided, it sets the counter."
  (interactive "p")
  (let* ((dir "~/backup/")
         (name (buffer-name))
         (base-name (file-name-sans-extension name))
         (extension (file-name-extension name t))
         (counter (if arg (prefix-numeric-value arg) 1))
         (new-file))
    (while (and (setq new-file
                      (format "%s%s_%03d%s" dir base-name counter extension))
                (file-exists-p new-file))
      (setq counter (1+ counter)))
    (message (concat "Backed " new-file))
    (copy-file name new-file)))

(defun my/save-buffer-as-html ()
  (interactive)
  ;; Define the export file name by appending .html to the current buffer name
  (let* ((original-buffer-name (buffer-name))
         (html-file-name (concat (file-name-sans-extension original-buffer-name) ".html"))
         (html-buffer (htmlize-buffer (current-buffer))))
    ;; Check if the file exists, and if so, ask the user if they want to overwrite it
    (if (file-exists-p html-file-name)
        (if (y-or-n-p (format "File %s already exists. Overwrite? " html-file-name))
            (progn
              ;; User chose to overwrite: Save the HTML buffer to the file
              (with-current-buffer html-buffer
                (write-file html-file-name))
              (kill-buffer html-buffer) ;; Clean up the temporary HTML buffer
              (message "Exported to %s" html-file-name))
          ;; User chose not to overwrite: Just clean up
          (kill-buffer html-buffer))
      ;; File doesn't exist: Save directly
      (with-current-buffer html-buffer
        (write-file html-file-name))
      (kill-buffer html-buffer) ;; Clean up the temporary HTML buffer
      (message "Exported to %s" html-file-name))))

;;
;; -> window-positioning
;;

(add-to-list 'display-buffer-alist
             '("\\*kmonad" display-buffer-no-window
               (allow-no-window . t)))

(add-to-list 'display-buffer-alist
             '("\\*Async" display-buffer-no-window
               (allow-no-window . t)))

(add-to-list 'display-buffer-alist
             '("\\*Proced" display-buffer-same-window))

(add-to-list 'display-buffer-alist
             '("\\*Messages" display-buffer-same-window))

(add-to-list 'display-buffer-alist
             '("magit:" display-buffer-same-window))

(add-to-list 'display-buffer-alist
             '("\\*deadgrep"
               (display-buffer-reuse-window display-buffer-in-direction)
               (direction . leftmost)
               (dedicated . t)
               (window-width . 0.33)
               (inhibit-same-window . t)))

(add-to-list 'display-buffer-alist
             '("\\*compilation"
               (display-buffer-reuse-window display-buffer-in-direction)
               (direction . leftmost)
               (dedicated . t)
               (window-width . 0.3)
               (inhibit-same-window . t)))

(add-to-list 'display-buffer-alist
             '("consult-ripgrep"
               (display-buffer-reuse-window display-buffer-in-direction)
               (direction . leftmost)
               (dedicated . t)
               (window-width . 0.33)
               (inhibit-same-window . t)))

(add-to-list 'display-buffer-alist
             '("\\Running"
               (display-buffer-reuse-window display-buffer-in-direction)
               (direction . leftmost)
               (dedicated . t)
               (window-width . 0.33)
               (inhibit-same-window . t)))

(add-to-list 'display-buffer-alist
             '("\\*Help\\*"
               (display-buffer-reuse-window display-buffer-same-window)))

;;
;; -> org-capture
;;

(setq bookmark-fringe-mark nil)

(defun my-capture-top-level ()
  "Function to capture a new entry at the top level of the given file."
  (goto-char (point-min))
  (or (outline-next-heading)
      (goto-char (point-max)))
  (unless (bolp) (insert "\n")))

(setq org-capture-templates
      '(
        ("t" "Tagged" plain
         (file+function
          "~/DCIM/content/tags--all.org"
          my-capture-top-level)
         "* DONE %^{title} tagged :%\\1:
:PROPERTIES:
:EXPORT_FILE_NAME: index
:EXPORT_HUGO_SECTION: tagged/%\\1
:EXPORT_HUGO_LASTMOD: <%<%Y-%m-%d %H:%M>>
:EXPORT_HUGO_TYPE: gallery
:EXPORT_HUGO_CUSTOM_FRONT_MATTER+: :thumbnail /tagged/%\\1.jpg
:END:
%\\1 tagged
%?
" :prepend t :jump-to-captured t)

        ("b" "Blog" plain
         (file+function
          "~/DCIM/content/blog--all.org"
          my-capture-top-level)
         "* TODO %^{title} :%(format-time-string \"%Y\"):
:PROPERTIES:
:EXPORT_FILE_NAME: %<%Y%m%d%H%M%S>-blog--%\\1
:EXPORT_HUGO_SECTION: blog
:EXPORT_HUGO_LASTMOD: <%<%Y-%m-%d %H:%M>>
:EXPORT_HUGO_CUSTOM_FRONT_MATTER+: :thumbnail /blog/%<%Y%m%d%H%M%S>-blog--%\\1.jpg
:END:
%?
" :prepend t :jump-to-captured t)

        ("g" "Gallery" plain
         (file+function
          "~/DCIM/content/blog--all.org"
          my-capture-top-level)
         (function my/org-hugo-new-subtree-post-capture-template)
         :prepend t :jump-to-captured t)

        ("e" "Emacs" plain
         (file+function
          "~/DCIM/content/emacs--all.org"
          my-capture-top-level)
         "* TODO %^{title} :emacs:%(format-time-string \"%Y\"):
:PROPERTIES:
:EXPORT_FILE_NAME: %<%Y%m%d%H%M%S>-emacs--%\\1
:EXPORT_HUGO_SECTION: emacs
:EXPORT_HUGO_LASTMOD: <%<%Y-%m-%d %H:%M>>
:EXPORT_HUGO_CUSTOM_FRONT_MATTER+: :thumbnail /emacs/%<%Y%m%d%H%M%S>-emacs--%\\1.jpg
:END:
%?
" :prepend t :jump-to-captured t)

        ("l" "Linux" plain
         (file+function
          "~/DCIM/content/linux--all.org"
          my-capture-top-level)
         "* TODO %^{title} :%(format-time-string \"%Y\"):
:PROPERTIES:
:EXPORT_FILE_NAME: %<%Y%m%d%H%M%S>-linux--%\\1
:EXPORT_HUGO_SECTION: linux
:EXPORT_HUGO_LASTMOD: <%<%Y-%m-%d %H:%M>>
:EXPORT_HUGO_CUSTOM_FRONT_MATTER+: :thumbnail /linux/%<%Y%m%d%H%M%S>-emacs--%\\1.jpg
:END:
%?
" :prepend t :jump-to-captured t)

        ("a" "Art")

        ("av" "Art Videos" plain
         (file+function
          "~/DCIM/content/art--all.org"
          my-capture-top-level)
         "* TODO %^{title} :videos:painter:krita:artrage:%(format-time-string \"%Y\"):
:PROPERTIES:
:EXPORT_FILE_NAME: %<%Y%m%d%H%M%S>--%\\1-%\\2
:EXPORT_HUGO_SECTION: art--videos
:EXPORT_HUGO_LASTMOD: <%<%Y-%m-%d %H:%M>>
:EXPORT_HUGO_CUSTOM_FRONT_MATTER+: :thumbnail /art--videos/%<%Y%m%d%H%M%S>--%\\1-%\\2.jpg
:VIDEO:
:END:
#+begin_export md
{{< youtube %^{youtube} >}}
#+end_export
%?
" :prepend t :jump-to-captured t)

        ("aa" "Art" plain
         (file+function
          "~/DCIM/content/art--all.org"
          my-capture-top-level)
         "* TODO %^{title} :painter:krita:artrage:%(format-time-string \"%Y\"):
:PROPERTIES:
:EXPORT_FILE_NAME: %\\1
:EXPORT_HUGO_SECTION: art--all
:EXPORT_HUGO_LASTMOD: <%<%Y-%m-%d %H:%M>>
:EXPORT_HUGO_CUSTOM_FRONT_MATTER+: :thumbnail /art--all/%\\1.jpg
:VIDEO:
:END:
#+attr_org: :width 300px
#+attr_html: :width 100%
#+begin_export md
#+end_export
%?
" :prepend t :jump-to-captured t)))

;;
;; -> org
;;
(use-package org-preview-html
  :custom
  (org-preview-html-subtree-only nil)
  (org-preview-html-refresh-configuration 'manual)
  (org-preview-html-viewer 'eww))

(defun my/org-tag-refresh()
  ""
  (interactive)
  (revert-buffer-quick)
  (org-align-tags t))

(use-package org
  :defer t
  :bind
  (:map org-mode-map
        ("M-8" . org-metadown)
        ("M-9" . org-metaup))
  :config
  (setq org-src-tab-acts-natively t
        org-edit-src-content-indentation 0
        org-log-done t
        org-use-speed-commands t
        org-tags-sort-function 'org-string-collate-greaterp
        org-export-with-sub-superscripts nil
        org-deadline-warning-days 365
        org-hugo-base-dir "~/DCIM"
        org-image-actual-width (list 50)
        org-startup-indented t
        org-return-follows-link t
        org-use-fast-todo-selection 'expert
        org-todo-keywords
        ;; '((sequence "TODO(t)" "DOING(d)" "ORDR(o)" "SENT(s)" "|" "CANCELLED(c)" "DONE(n)"))
        '((sequence "TODO" "DOING" "ORDR" "SENT" "|" "CANCELLED" "DONE"))
        org-todo-keyword-faces
        '(("TODO" . "#ee5566")
          ("DOING" . "#5577aa")
          ("ORDR" . "#bb44ee")
          ("SENT" . "#bb44ee")
          ("CANCELLED" . "#426b3e")
          ("DONE" . "#77aa66"))
        org-cycle-separator-lines 0))

(use-package org-tidy)

(use-package toc-org
  :commands
  toc-org-enable
  :init
  (add-hook 'org-mode-hook 'toc-org-enable))

(defun org-syntax-table-modify ()
  "Modify `org-mode-syntax-table' for the current org buffer."
  (modify-syntax-entry ?< "." org-mode-syntax-table)
  (modify-syntax-entry ?> "." org-mode-syntax-table))

(defun my/org-shrink-tables ()
  "Shrink all tables in the Org buffer."
  (interactive)
  (save-excursion
    (let ((block-start (point-min))   ;; Initialize to the start of the buffer
          (block-end (point-min)))
      (goto-char (point-min))
      ;; Loop over all tables in the buffer
      (while (search-forward "|-" nil t)
        (save-excursion
          ;; Check if we're currently in a source block
          (when (org-between-regexps-p "^[ \t]*#\\+begin_src" "^[ \t]*#\\+end_src")
            ;; If yes, move block-end to the end of the current source block
            (end-of-line)
            (search-forward-regexp "^[ \t]*#\\+end_src" nil t)
            (setq block-end (point))
            ;; Jump to the end of the current source block
            (goto-char block-end)))
        ;; Ensure we're not inside a recently skipped source block
        (unless (<= (point) block-end)
          ;; Shrink table as we're outside a source block
          (org-table-shrink))))))

(add-hook 'org-mode-hook #'my/org-shrink-tables)
(add-hook 'org-mode-hook #'org-syntax-table-modify)
;; (remove-hook 'org-mode-hook #'org-syntax-table-modify)
;; (remove-hook 'org-mode-hook #'my/org-shrink-tables)

;;
;; -> org-agenda
;;

(use-package org
  :custom
  (org-agenda-include-diary nil)
  (org-agenda-show-all-dates t)
  (org-refile-targets '((org-agenda-files :maxlevel . 1)))
  (org-agenda-files '("~/DCIM/content/aaa--aaa.org"
                      "~/DCIM/content/aaa--todo.org"
                      "~/DCIM/content/aab--calendar.org"
                      "~/DCIM/content/aac--baby.org"
                      "~/DCIM/content/aaf--kate.org"
                      "~/DCIM/content/aag--emacs-todo.org"
                      ))
  :config
  (with-eval-after-load 'org-agenda
    (unbind-key "M-m" org-agenda-mode-map)
    (setq org-agenda-custom-commands
          '(("m" "Month View" agenda ""
             ((org-agenda-start-day "today")
              (org-agenda-span 30)
              (org-agenda-time-grid nil)))
            ("0" "Year View (2020)" agenda ""
             ((org-agenda-start-day "2020-01-01")
              (org-agenda-span 'year)
              (org-agenda-time-grid nil)))
            ("1" "Year View (2021)" agenda ""
             ((org-agenda-start-day "2021-01-01")
              (org-agenda-span 'year)
              (org-agenda-time-grid nil)))
            ("2" "Year View (2022)" agenda ""
             ((org-agenda-start-day "2022-01-01")
              (org-agenda-span 'year)
              (org-agenda-time-grid nil)))
            ("3" "Year View (2023)" agenda ""
             ((org-agenda-start-day "2023-01-01")
              (org-agenda-span 'year)
              (org-agenda-time-grid nil)))
            ("4" "Year View (2024)" agenda ""
             ((org-agenda-start-day "2024-01-01")
              (org-agenda-span 'year)
              (org-agenda-time-grid nil)))))))

;;
;; -> dwim
;;

(when (file-exists-p "/home/jdyer/bin/category-list-uniq.txt")
  (progn
    (defvar my/dwim-convert-commands
      '("ConvertNoSpace" "AudioConvert" "AudioInfo" "AudioNormalise"
        "AudioTrimSilence" "PictureAutoColour" "PictureConvert"
        "PictureCrush" "PictureFrompdf" "PictureInfo" "PictureMontage"
        "PictureOrganise" "PictureCrop" "PictureRotateFlip" "PictureEmail"
        "PictureUpdateFromCreateDate"
        "PictureRotateLeft" "PictureRotateRight" "PictureScale"
        "PictureUpscale" "PictureGetText" "PictureOrientation"
        "PictureUpdateToCreateDate" "VideoConcat" "VideoConvert" "VideoConvertToGif"
        "VideoCut" "VideoDouble" "VideoExtractAudio" "VideoExtractFrames"
        "VideoFilter" "VideoFromFrames" "VideoInfo" "VideoRemoveAudio"
        "VideoReplaceVideoAudio" "VideoRescale" "VideoReverse"
        "VideoRotate" "VideoRotateLeft" "VideoRotateRight" "VideoShrink"
        "VideoSlowDown" "VideoSpeedUp" "VideoZoom" "WhatsAppConvert"
        "PictureCorrect" "Picture2pdf" "PictureTag" "PictureTagRename"
        "OtherTagDate" "VideoRemoveFlips")
      "List of commands for dwim-convert.")

    (defun my/read-lines (file-path)
      "Return a list of lines of a file at FILE-PATH."
      (with-temp-buffer
        (insert-file-contents file-path)
        (split-string (buffer-string) "\n" t)))

    (defun my/dwim-convert-generic (command)
      "Execute a dwim-shell-command-on-marked-files with the given COMMAND."
      (let* ((unique-text-file "/home/jdyer/bin/category-list-uniq.txt")
             (user-selection nil)
             (files (dired-get-marked-files nil current-prefix-arg))
             (command-and-files (concat command " " (mapconcat 'identity files " "))))
        (when (string= command "PictureTag")
          (setq user-selection (completing-read "Choose an option: "
                                                (my/read-lines unique-text-file)
                                                nil t)))
        (async-shell-command (if user-selection
                                 (concat command " " user-selection " " (mapconcat 'identity files " "))
                               (concat command " " (mapconcat 'identity files " ")))
                             "*convert*")))

    (defun my/dwim-convert-with-selection ()
      "Prompt user to choose command and execute dwim-shell-command-on-marked-files."
      (interactive)
      (let ((chosen-command (completing-read "Choose command: "
                                             my/dwim-convert-commands)))
        (my/dwim-convert-generic chosen-command)))

    (global-set-key (kbd "C-c v") 'my/dwim-convert-with-selection)))

;;
;; -> scroll
;;
(setq scroll-step 2)
(setq scroll-conservatively 10)
(setq scroll-margin 10)
(setq scroll-preserve-screen-position t)

;;
;; -> custom-set-faces
;;

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit default :weight regular :height 1.1))))
 '(org-level-2 ((t (:inherit default :weight light :height 1.0))))
 '(org-level-3 ((t (:inherit default :weight light :height 1.0))))
 '(org-level-4 ((t (:inherit default :weight light :height 1.0))))
 '(org-level-5 ((t (:inherit default :weight light :height 1.0))))
 '(org-level-6 ((t (:inherit default :weight light :height 1.0))))
 '(diredfl-date-time ((t (:foreground "#8d909b"))))
 '(diredfl-dir-heading ((t (:foreground "#aa5555" :weight bold))))
 '(diredfl-dir-priv ((t (:foreground "DarkRed"))))
 '(diredfl-exec-priv ((t (:foreground "#999999"))))
 '(diredfl-file-name ((t (:foreground "#818282"))))
 '(diredfl-no-priv ((t nil)))
 '(diredfl-number ((t (:foreground "#999999"))))
 '(diredfl-read-priv ((t nil)))
 '(diredfl-write-priv ((t nil)))
 '(ediff-current-diff-A ((t (:extend t :background "#b5daeb" :foreground "#000000"))))
 '(ediff-even-diff-A ((t (:background "#bafbba" :foreground "#000000" :extend t))))
 '(ediff-fine-diff-A ((t (:background "#f4bd92" :foreground "#000000" :extend t))))
 '(ediff-odd-diff-A ((t (:background "#b8fbb8" :foreground "#000000" :extend t))))
 '(ztreep-diff-model-diff-face ((t (:foreground "#7cb0f2"))))
 '(ztreep-diff-model-add-face ((t (:foreground "#e38d5a"))))
 '(elfeed-search-title-face ((t (:foreground "#4E4E4E" :height 1.1 :family "Source Code Pro"))))
 '(font-lock-warning-face ((t (:foreground "#930000" :inverse-video nil))))
 '(org-link ((t (:underline nil))))
 '(indent-guide-face ((t (:background "#282828" :foreground "#666666"))))
 '(stripe-highlight ((t (:background "#F0F0F0"))))
 '(widget-button ((t (:inherit fixed-pitch :weight regular))))
 '(window-divider ((t (:foreground "black"))))
 '(org-tag ((t (:height 0.99))))
 '(aw-leading-char-face ((t (:inherit (highlight) :inverse-video nil :weight bold :height 1.1))))
 '(vertical-border ((t (:foreground "#000000")))))

;;
;; -> dired
;;

(defun my/dired-du ()
  "Run 'du -hc' on the directory under the cursor in Dired."
  (interactive)
  (let ((current-dir (dired-get-file-for-visit)))
    (if (file-directory-p current-dir)
        (dired-do-async-shell-command "du -hc" nil (list current-dir))
      (message "The current point is not a directory."))))

(use-package dired
  :ensure nil
  :diminish dired-async-mode
  :commands (dired dired-jump)
  :bind (("M-e" . dired-jump)
         (:map dired-mode-map
               ("W" . dired-do-async-shell-command)
               ("j" . dired-next-line)
               ("k" . dired-previous-line)
               ("-" . dired-jump)
               ("b" . my/dired-file-to-org-link)
               ("_" . dired-create-empty-file)
               ("C-c i" . my/image-dired-sort)
               ("C-c u" . my/dired-du)
               ("C-c d" . my/dired-duplicate-file)))
  :custom
  ;; (dired-async--modeline-mode 1)
  (dired-guess-shell-alist-user
   '(("\\.\\(jpg\\|jpeg\\|png\\|gif\\|bmp\\)$" "gthumb")
     ("\\.\\(mp4\\|mkv\\|avi\\|mov\\|wmv\\|flv\\|mpg\\)$" "mpv")
     ("\\.\\(mp3\\|wav\\|ogg\\|\\)$" "mpv")
     ("\\.\\(kra\\)$" "org.kde.krita")
     ("\\.\\(odt\\|ods\\)$" "libreoffice")
     ("\\.\\(html\\|htm\\)$" "firefox")
     ("\\.\\(pdf\\|epub\\)$" "xournalpp")))
  (dired-dwim-target t)
  (dired-listing-switches "-alGgh")
  (dired-auto-revert-buffer t)
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-confirm-shell-command nil)
  (dired-no-confirm t)
  (dired-recursive-deletes 'always)
  (dired-deletion-confirmer '(lambda (x) t))
  :config
  (dired-async-mode 1))

(defun my/dired-create-directory ()
  "Wrapper to dired-create-directory to avoid minibuffer completion."
  (interactive)
  (let ((search-term
         (read-from-minibuffer "Dir : ")))
    (dired-create-directory search-term)))

(defun my/dired-create-empty-file ()
  "Wrapper to dired-create-empty-file to avoid minibuffer completion."
  (interactive)
  (let ((search-term
         (read-from-minibuffer "File : ")))
    (dired-create-empty-file search-term)))

;;
;; -> image-dired
;;

(require 'image-mode)
(require 'image-dired)

(add-to-list 'display-buffer-alist
             '("\\*image-dired\\*"
               display-buffer-in-direction
               (direction . left)
               (window . root)
               (window-width . 0.5)))

(add-to-list 'display-buffer-alist
             '("\\*image-dired-display-image\\*"
               display-buffer-in-direction
               (direction . right)
               (window . root)
               (window-width . 0.5)))

(defun my/image-dired-sort (arg)
  "Sort images in various ways given ARG."
  (interactive "P")
  ;; Use `let` to temporarily set `dired-actual-switches`
  (let ((dired-actual-switches
         (cond
          ((equal arg nil)            ; no C-u
           "-lGghat --ignore=*.xmp")
          ((equal arg '(4))           ; C-u
           "-lGgha --ignore=*.xmp")
          ((equal arg 1)              ; C-u 1
           "-lGgha --ignore=*.xmp"))))
    (let ((w (selected-window)))
      (delete-other-windows)
      (revert-buffer)
      (image-dired ".")
      (let ((idw (selected-window)))
        (select-window w)
        (dired-unmark-all-marks)
        (select-window idw)
        (image-dired-display-this)
        (image-dired-line-up-dynamic)))))

(setq image-use-external-converter t)
(setq image-dired-external-viewer "/usr/bin/gthumb")
(setq image-dired-show-all-from-dir-max-files 999)
(setq image-dired-thumbs-per-row 999)
(setq image-dired-thumb-relief 0)
(setq image-dired-thumb-margin 5)
(setq image-dired-thumb-size 120)

(defun my/image-save-as ()
  "Save the current image buffer as a new file."
  (interactive)
  (let* ((file (buffer-file-name))
         (dir (file-name-directory file))
         (name (file-name-nondirectory file))
         (base-name (file-name-sans-extension name))
         (extension (file-name-extension name t))
         (initial_mode major-mode)
         (counter 1)
         (new-file))
    (while (and (setq new-file
                      (format "%s%s_%03d%s" dir base-name counter extension))
                (file-exists-p new-file))
      (setq counter (1+ counter)))
    (write-region (point-min) (point-max) new-file nil 'no-message)
    (revert-buffer nil t nil)
    ;; (delete-file file t)
    (if (equal initial_mode 'image-dired-image-mode)
        (progn
          (image-dired ".")
          (image-dired-display-this))
      (find-file new-file t))))

(defun my/delete-current-image-and-move-to-next ()
  "Delete the current image file and move to the next image in the directory."
  (interactive)
  (let ((current-file (buffer-file-name)))
    (when current-file
      (image-next-file 1)
      (delete-file current-file)
      (message "Deleted %s" current-file))))

(defun my/delete-current-image-thumbnails ()
  "Delete the current image file and move to the next image in the directory."
  (interactive)
  (let ((file-name (image-dired-original-file-name)))
    (delete-file file-name)
    (image-dired-delete-char)
    (image-dired-display-this)))

(eval-after-load 'image-mode
  '(progn
     (define-key image-mode-map (kbd "C-d") 'my/delete-current-image-and-move-to-next)
     (define-key image-mode-map (kbd "C-x C-s") 'my/image-save-as)))

(eval-after-load 'image-dired
  '(progn
     (define-key image-dired-thumbnail-mode-map (kbd "C-d") 'my/delete-current-image-thumbnails)
     (define-key image-dired-thumbnail-mode-map (kbd "n")
                 (lambda ()(interactive)(image-dired-forward-image)(image-dired-display-this)))
     (define-key image-dired-thumbnail-mode-map (kbd "p")
                 (lambda ()(interactive)(image-dired-backward-image)(image-dired-display-this)))
     ))

;;
;; -> visuals
;;

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq inhibit-startup-screen t)

(defvar my-org-mode-exclude-files-list
  (list
   "~/.config/emacs/emacs--init.org"
   "~/DCIM/content/aac--baby.org"
   )
  "List of file paths to exclude from `my-org-visual-line-mode-exclude-init`.")

(defun my-org-visual-line-mode-exclude-init ()
  (let ((current-file (buffer-file-name))
        (full-paths-exclude-list (mapcar 'expand-file-name my-org-mode-exclude-files-list)))
    (unless (member current-file full-paths-exclude-list)
      (visual-line-mode 1))))

(add-hook 'org-mode-hook 'my-org-visual-line-mode-exclude-init)

(setq-default truncate-partial-width-windows 120)

(set-frame-parameter nil 'alpha-background 95)
(add-to-list 'default-frame-alist '(alpha-background . 95))

(set-fringe-mode '(20 . 20))
(set-display-table-slot standard-display-table 0 ?\ )

(setq window-divider-default-bottom-width 6)
(setq window-divider-default-right-width 6)
(setq window-divider-default-places t)
(window-divider-mode -1)

(setq-default left-margin-width 0 right-margin-width 0)

(defvar my/internal-border-width 0 "Default internal border width for toggling.")

(defun my/toggle-internal-border-width (&optional value)
  "Toggle internal border width given VALUE."
  (interactive "P")
  (let ((new-value (if value
                       value
                     (if (= (or (frame-parameter nil 'internal-border-width) 0)
                            0)
                         my/internal-border-width
                       0))))
    (modify-all-frames-parameters `((internal-border-width . ,new-value)))))

(modify-all-frames-parameters `((internal-border-width . ,my/internal-border-width)))

;;
;; -> imenu
;;

(defun my-imenu-create-index ()
  "Create an index using definitions starting with ';; ->'."
  (let ((index-alist '())
        (regex "^;;[[:space:]]->\\(.+\\)$"))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regex nil t)
        (let ((name (s-trim (match-string 1)))
              (pos (match-beginning 0)))
          (push (cons name (set-marker (make-marker) pos)) index-alist))))
    (setq imenu--index-alist (sort
                              index-alist
                              (lambda (a b)
                                (string< (car a) (car b)))))))

;; (setq imenu-create-index-function #'my-imenu-create-index)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq truncate-lines t)
            (setq imenu-sort-function 'imenu--sort-by-name)
            (setq imenu-generic-expression
                  '((nil "^;;[[:space:]]+-> \\(.*\\)$" 1)))
            (imenu-add-menubar-index)))

(add-hook 'conf-space-mode-hook
          (lambda ()
            (setq imenu-sort-function 'imenu--sort-by-name)
            (setq imenu-generic-expression
                  '((nil "^#[[:space:]]+-> \\(.*\\)$" 1)))
            (imenu-add-menubar-index)))

;;
;; -> recentf
;;

(recentf-mode 1)

(setq recentf-max-menu-items 200)
(setq recentf-max-saved-items 200)

;;
;; -> modeline
;;

(setq-default mode-line-modified
              '(:eval (if (and (buffer-file-name) (buffer-modified-p))
                          (propertize " * " 'face
                                      '(:background "#ff0000" :foreground "#ffffff" :inherit bold)) "")))

(set-face-attribute 'mode-line-active nil :height 130 :underline nil :overline nil :box nil
                    :background "#a7a7a7" :foreground "#000000")
(set-face-attribute 'mode-line-inactive nil :height 110 :underline nil :overline nil
                    :background "#151515" :foreground "#cacaca")

(defun my-tab-bar-number ()
  "Return the current tab's index (number) as a string."
  (let ((current-tab (assq 'current-tab (funcall tab-bar-tabs-function)))
        (tabs (funcall tab-bar-tabs-function))
        (index 1))
    (while (and tabs (not (eq (car tabs) current-tab)))
      (setq tabs (cdr tabs))
      (setq index (1+ index)))
    (format " %d " index)))

(defun my-all-tabs-string ()
  "Return a string representing all tabs with the current tab highlighted."
  (let* ((current-tab (assq 'current-tab (funcall tab-bar-tabs-function)))
         (tabs (funcall tab-bar-tabs-function))
         (index 1)
         (tabs-string ""))
    (while tabs
      ;; For the current tab, apply special properties. Otherwise, format normally.
      (let ((tab-string (if (eq (car tabs) current-tab)
                            (propertize (format " %d " index) 'face '(:inverse-video t :box (:line-width (1 . 1) :style flat)))
                          (format " %d " index))))
        (setq tabs-string (concat tabs-string tab-string)))
      (setq tabs (cdr tabs))
      (setq index (1+ index)))
    tabs-string))

(setq my/mode-line-format
      '("%e"
        ;; (:eval (my-all-tabs-string))
        mode-line-modified
        (:eval
         (propertize (format "%s" (abbreviate-file-name default-directory)) 'face '(:inherit bold)))
        (:eval
         (when (or (eq major-mode 'image-mode)
                   (eq major-mode 'image-dired-image-mode))
           (process-lines  "identify"  "-format"  "[%m %wx%h %b]" (buffer-file-name))))
        (:eval
         (if (not (equal major-mode 'dired-mode))
             (propertize (format "%s " (buffer-name)))
           " "))
        mode-line-position
        mode-line-modes
        mode-line-misc-info))
;; "-%-"))

(setq-default mode-line-format my/mode-line-format)

(defun my-frame-title-format ()
  "Return the buffer's file path with home replaced by `~`."
  (let ((filename (or (buffer-file-name) dired-directory default-directory)))
    (if filename
        (abbreviate-file-name filename)  ; Use ~ for home directory
      "%b")))  ; If no file, show the buffer name (%b)

(setq frame-title-format '(:eval (my-frame-title-format)))
;; (setq frame-title-format "%f")

(defun my/toggle-mode-line ()
  "Toggle the visibility of the mode-line by checking its current state."
  (interactive)
  (if (eq mode-line-format nil)
      (progn
        (setq-default mode-line-format my/mode-line-format)
        (setq frame-title-format "%f"))
    (progn
      (setq-default mode-line-format nil)
      (setq frame-title-format my/mode-line-format)))
  (force-mode-line-update t))

(display-time-mode -1)
(setq mode-line-compact nil)

;;
;; -> find
;;

(setq find-dired-refine-function 'find-dired-sort-by-filename)
(setq find-dired-refine-function 'nil)
(setq find-ls-option (cons "-exec ls -lSh {} +" "-lSh"))

(defun my/find-file ()
  "Find file from current directory in many different ways."
  (interactive)
  (let* ((find-options '(("find -type f -printf \"$PWD/%p\\0\"" . :string)
                         ("fd --absolute-path --type f -0" . :string)
                         ("rg --follow --files --null" . :string)
                         ("find-name-dired" . :command)))
         (selection (completing-read "Select : " find-options))
         (metadata '((category . file)))
         (file-list)
         (file))
    (pcase (alist-get selection find-options nil nil #'string=)
      (:command
       (call-interactively (intern selection)))
      (:string
       (setq file-list (split-string (shell-command-to-string selection) "\0" t))
       (setq file (completing-read (format "Find file in %s: " (abbreviate-file-name default-directory))
                                   (lambda (str pred action)
                                     (if (eq action 'metadata)
                                         `(metadata . ,metadata)
                                       (complete-with-action action file-list str pred)))
                                   nil t nil 'file-name-history))))
    (when file (find-file (expand-file-name file)))))

;;
;; -> grep
;;

(require 'grep)

(eval-after-load 'grep
  '(progn
     (dolist (dir '("nas" ".cache" "cache" "elpa" "chromium"
                    ".local/share" "syncthing" ".mozilla" ".local/lib" "Games"
                    ".wine" ".thunderbird" ".gnupg"))
       (push dir grep-find-ignored-directories))
     (dolist (file '(".cache" "*cache*" "*.iso" "*.xmp" "*.jpg" "*.mp4" "*.dll" "*.mp3"))
       (push file grep-find-ignored-files))))

;; (setq-default deadgrep--search-case 'ignore)

;;
;; -> spelling
;;

(use-package jinx)
(use-package writegood-mode)

(use-package powerthesaurus
  :init
  (require 'transient)
  (transient-define-prefix my/transient-spelling ()
    "Spelling commands"
    ["Spelling"
     ["Lookups"
      ("t" "Synonyms" powerthesaurus-lookup-synonyms-dwim)
      ("a" "Antonyms" powerthesaurus-lookup-antonyms-dwim)]
     ["Spelling Tools"
      ("l" "Jinx" (lambda ()(interactive)
                    (flymake-proselint-setup)
                    (call-interactively 'jinx-mode)
                    (call-interactively 'writegood-mode)
                    (call-interactively 'flymake-mode)))
      ("j" "Jinx correct" jinx-correct)
      ("s" "Jinx correct" jinx-correct)]
     ["Dictionary"
      ("d" "Lookup" dictionary-lookup-definition)]
     ["languagetool"
      ("m" "Server Mode" languagetool-server-mode)
      ("c" "Correct" languagetool-correct-at-point)
      ("e" "Server Start" languagetool-server-start)
      ("p" "Server Stop" languagetool-server-stop)
     ]]
    )
  :bind
  ("C-c s" . my/transient-spelling))

(setq ispell-local-dictionary "en_GB")
(setq ispell-program-name "hunspell")
(setq dictionary-default-dictionary "*")
(setq dictionary-server "dict.org")
(setq dictionary-use-single-buffer t)

;;
;; -> gdb
;;

(setq gdb-display-io-nopopup 1)
(setq gdb-many-windows t)

(global-set-key (kbd "<f9>") 'gud-break)
(global-set-key (kbd "<f10>") 'gud-next)
(global-set-key (kbd "<f11>") 'gud-step)

;;
;; -> compilation
;;

(setq compilation-always-kill t)
(setq compilation-context-lines 3)
(setq compilation-scroll-output t)
;; ignore warnings
(setq compilation-skip-threshold 2)

(global-set-key (kbd "<f5>") 'my/project-compile)

;; (add-hook 'compilation-mode-hook #'my/project-create-compilation-search-path)

;;
;; -> auto-mode-alist
;;

(add-to-list 'auto-mode-alist '("\\.org_archive\\'" . org-mode))
(add-to-list 'auto-mode-alist '("config.rasi\\'" . js-json-mode))
(add-to-list 'auto-mode-alist '("theme.rasi\\'" . css-mode))
(add-to-list 'auto-mode-alist '("waybar.*/config\\'" . js-json-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-ts-mode))
(add-to-list 'auto-mode-alist '("CMakeLists.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.org_archive\\'" . org-mode))
(add-to-list 'auto-mode-alist '("/sway/.*config.*/" . i3wm-config-mode))
(add-to-list 'auto-mode-alist '("/sway/config\\'" . i3wm-config-mode))
(cl-loop for ext in '("\\.gpr$" "\\.ada$" "\\.ads$" "\\.adb$")
         do (add-to-list 'auto-mode-alist (cons ext 'ada-mode)))

;;
;; -> programming
;;

(setq my/old-ada-mode (concat user-emacs-directory "old/old-ada-mode"))
(when (file-exists-p my/old-ada-mode)
  (use-package ada-mode
    :load-path my/old-ada-mode))

(use-package yaml-mode)

(add-hook 'yaml-mode-hook
          #'(lambda ()
              (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(setq eldoc-echo-area-use-multiline-p nil)

(use-package flycheck)
(use-package package-lint)
(use-package cmake-mode)

(setq vc-handled-backends '(SVN Git))

;;
;; -> diff
;;

(use-package ztree)

(setq-default ztree-diff-filter-list
              '(
                "build" "\.dll" "\.iso" "\.xmp" "\.cache" "\.gnupg" "\.local"
                "\.mozilla" "\.thunderbird" "\.wine" "\.mp3" "\.mp4" "\.arpack"
                "\.git" "^Volume$" "^Games$" "^cache$" "^chromium$" "^elpa$" "^nas$"
                "^syncthing$" "bin"
                ))

;; (setq-default ztree-diff-additional-options '("-w" "-i"))
(setq-default ztree-diff-consider-file-size t)
(setq-default ztree-diff-consider-file-permissions nil)
(setq-default ztree-diff-show-equal-files nil)

(use-package diff-mode
  :hook
  ((diff-mode . (lambda ()
                  (define-key diff-mode-map (kbd "M-j") nil)
                  (define-key diff-mode-map (kbd "M-k") nil)
                  (define-key diff-mode-map (kbd "M-h") nil)
                  (define-key diff-mode-map (kbd "M-l") nil)))
   (ediff-prepare-buffer . org-fold-show-all)
   (ediff-prepare-buffer . (lambda () (visual-line-mode -1))))
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-highlight-all-diffs t)
  (ediff-split-window-function 'split-window-horizontally))

;;
;; -> ada
;;

(defun my/eglot-dir-locals ()
  "Create .dir-locals.el file for eglot ada-mode using the selected DIRED path."
  (interactive)
  (add-dir-local-variable
   'ada-mode
   'eglot-workspace-configuration
   `((ada . (:projectFile ,(dired-get-filename))))))

(setq xref-auto-jump-to-first-definition t)
(setq xref-auto-jump-to-first-xref t)

(defun my/xref--read-identifier (prompt)
  "Custom function to find definitions in Ada mode with PROMPT."
  (let ((def (xref-backend-identifier-at-point 'etags))
        (variations '("/t" "/k" "/f" "/p" "/b" "/s"))
        (ada-refs 'nil))
    (when def
      (dolist (variation variations)
        (if (xref-backend-definitions 'etags (concat def variation))
            (setq ada-refs (cons (concat identifier variation) ada-refs)))))
    (cond
     ((eq (length ada-refs) 0)
      (setq id def))
     ((eq (length ada-refs) 1)
      (setq id (nth 0 ada-refs)))
     (t
      (setq id (completing-read prompt ada-refs))))))

(defun my/xref-find-definitions (identifier)
  "Find Definition given IDENTIFIER."
  (interactive "p")
  (setq identifier (my/xref--read-identifier "Find definitions of: "))
  (xref-find-definitions identifier))

(defun buffer-in-eglot-mode-p ()
  (if (fboundp 'eglot-managed-p)
      (eglot-managed-p)
    nil))

(defun buffer-in-old-ada-mode-p ()
  (if (boundp 'ada-prj-default-project-file)
      t
    nil))

(defun buffer-in-tags-mode-p ()
  (if tags-table-list
      t
    nil))

(defun my/ada-find-definitions ()
  "Custom function to find definitions in Ada mode."
  (interactive)
  (cond
   ((buffer-in-eglot-mode-p)
    (message "xref: eglot")
    (xref-find-definitions (xref-backend-identifier-at-point 'eglot)))
   ((buffer-in-old-ada-mode-p)
    (message "xref: old-ada-mode")
    (setq ada-xref-other-buffer nil)
    (ada-goto-declaration (point)))
   ((buffer-in-tags-mode-p)
    (message "xref: etags")
    (my/xref-find-definitions (xref-backend-identifier-at-point 'etags)))
   (t
    (message "xref: fallback")
    (my/etags-load)
    (my/xref-find-definitions (xref-backend-identifier-at-point 'etags)))))

(defun my/xref-quit-xref-marker-stack ()
  "Quit *xref* buffer."
  (interactive)
  (save-excursion
    (let ((target-window (get-buffer-window "*xref*")))
      (when target-window
        (select-window target-window)
        (quit-window t)))))

(defun my/ada-find-definition-pop ()
  "Custom function to pop definitions in Ada mode."
  (interactive)
  (cond
   ((buffer-in-eglot-mode-p)
    (message "xref pop: eglot")
    (my/xref-quit-xref-marker-stack)
    (xref-quit-and-pop-marker-stack))
   ((buffer-in-old-ada-mode-p)
    (message "xref pop: old-ada-mode")
    (ada-xref-goto-previous-reference))
   ((buffer-in-tags-mode-p)
    (message "xref pop: etags")
    (my/xref-quit-xref-marker-stack)
    (xref-go-back))
   (t
    (message "xref pop: fallback")
    (my/xref-quit-xref-marker-stack)
    (xref-go-back))))

(with-eval-after-load 'ada-mode
  (define-key ada-mode-map (kbd "M-.") 'my/ada-find-definitions)
  (define-key ada-mode-map (kbd "M-,") 'my/ada-find-definition-pop))

;;
;; -> treesitter
;;
(setq treesit-language-source-alist
      '((ada "https://github.com/briot/tree-sitter-ada")
        (bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript"
                    "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript"
             "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript"
                    "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; (mapc #'treesit-install-language-grammar
;;      (mapcar #'car treesit-language-source-alist))

(setq major-mode-remap-alist
      '( ;;      (ada-mode . ada-ts-mode)
        ;;      (yaml-mode . yaml-ts-mode)
        (toml-mode . toml-ts-mode)
        ;;      (bash-mode . bash-ts-mode)
        ;;      (sh-mode . bash-ts-mode)
        ;;      (js2-mode . js-ts-mode)
        ;;      (typescript-mode . typescript-ts-mode)
        ;;      (conf-colon-mode . json-ts-mode)
        ;;      (json-mode . json-ts-mode)
        ;;      (css-mode . css-ts-mode)
        ;;      (python-mode . python-ts-mode)
        ))

;;
;; -> whitespace
;;

(setq-default whitespace-style
              '(face spaces empty tabs newline trailing space-mark tab-mark
                     newline-mark tab-width indentation::space))

;; Whitespace color corrections.
(require 'color)
(let* ((ws-lighten 20) ;; Amount in percentage to lighten up black.
       (ws-color (color-lighten-name "#555555" ws-lighten)))
  (custom-set-faces
   `(whitespace-newline                ((t (:foreground ,ws-color))))
   `(whitespace-missing-newline-at-eof ((t (:foreground ,ws-color))))
   `(whitespace-space                  ((t (:foreground ,ws-color))))
   `(whitespace-space-after-tab        ((t (:foreground ,ws-color))))
   `(whitespace-space-before-tab       ((t (:foreground ,ws-color))))
   `(whitespace-tab                    ((t (:foreground ,ws-color))))
   `(whitespace-trailing               ((t (:foreground ,ws-color))))))

;; Make these characters represent whitespace.
(setq-default whitespace-display-mappings
              '(
                ;; space -> · else .
                (space-mark 32 [183] [46])
                ;; new line -> ¬ else $
                (newline-mark ?\n [172 ?\n] [36 ?\n])
                ;; carriage return (Windows) -> ¶ else #
                (newline-mark ?\r [182] [35])
                ;; tabs -> » else >
                (tab-mark ?\t [187 ?\t] [62 ?\t])))

;; Don't enable whitespace for.
(setq-default whitespace-global-modes
              '(not shell-mode
                    help-mode
                    magit-mode
                    magit-diff-mode
                    ibuffer-mode
                    dired-mode
                    occur-mode))

;; Set whitespace actions.
(setq-default whitespace-action
              '(cleanup auto-cleanup))

;;
;; -> project
;;

(defun my/project-root ()
  "Return project root defined by user."
  (interactive)
  (let ((root default-directory)
        (project (project-current)))
    (when project
      (cond ((fboundp 'project-root)
             (setq root (project-root project)))))))

(add-to-list 'project-switch-commands '(project-dired "Dired") t)

(defun my/project-create-compilation-search-path ()
  "Populate the 'compilation-search-path' variable.
With directories under project root using find."
  (interactive)
  (let ((find-command
         (concat "find " (project-root (project-current t))
                 " \\( -path \\*/.local -o -path \\*/.config -o -path \\*/.svn -o -path \\*/.git -o -path \\*/nas \\) -prune -o -type d -print")))
    (setq compilation-search-path
          (split-string
           (shell-command-to-string find-command)
           "\n" t))))

(defun my/project-compile (arg)
  "Bespoke project compile based on ARG."
  (interactive "p")
  (let ((default-directory (project-root (project-current t))))
    (cond ((= arg 1)
           (setq compile-command
                 (concat "make " buffer-file-name)))
          (t
           (setq compile-command "make")))
    (compile compile-command)))

(setq project-vc-extra-root-markers '(".project"))

(global-set-key (kbd "C-x p c") 'my/project-compile)

;;
;; -> indentation
;;

(setq-default indent-tabs-mode nil)
(setq-default tab-width 3)
(add-hook 'sh-mode-hook
          (lambda () (setq sh-basic-offset 3)))

(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-method 'bitmap)
  (highlight-indent-guides-auto-odd-face-perc 55)
  (highlight-indent-guides-auto-even-face-perc 75)
  (highlight-indent-guides-auto-character-face-perc 70)
  (highlight-indent-guides-bitmap-function 'highlight-indent-guides--bitmap-dots)
  (highlight-indent-guides-responsive 'top))

;;
;; -> etags
;;

(defun my/etags-load ()
  "Load TAGS file from the first it can find up the directory stack."
  (interactive)
  (let ((my-tags-file (locate-dominating-file default-directory "TAGS")))
    (when my-tags-file
      (message "Loading tags file: %s" my-tags-file)
      (visit-tags-table my-tags-file))))

(when (executable-find "my-generate-etags.sh")
  (defun my/etags-update ()
    "Call external bash script to generate new etags for all languages it can find."
    (interactive)
    (async-shell-command "my-generate-etags.sh" "*etags*")))

(defun predicate-exclusion-p (dir)
  "exclusion of directories"
  (not
   (or
    (string-match "/home/jdyer/examples/CPPrograms/nil" dir)
    )))

(defun my/generate-etags ()
  "Generate TAGS file for various source files in `default-directory` and its subdirectories."
  (interactive)
  (message "Getting file list...")
  (let ((all-files
         (append
          (directory-files-recursively default-directory "\\(?:\\.cpp$\\|\\.c$\\|\\.h$\\)" nil 'predicate-exclusion-p)
          (directory-files-recursively default-directory "\\(?:\\.cs$\\|\\.cs$\\)" nil 'predicate-exclusion-p)
          (directory-files-recursively default-directory "\\(?:\\.ads$\\|\\.adb$\\)" nil 'predicate-exclusion-p)))
        (tags-file-path (expand-file-name (concat default-directory "TAGS"))))
    (unless (file-directory-p default-directory)
      (error "Default directory does not exist: %s" default-directory))
    ;; Generate TAGS file
    (dolist (file all-files)
      (message file)
      (shell-command (format "etags --append \%s -o %s" file tags-file-path)))))

;; (global-set-key (kbd "C-x p l") 'my/etags-load)
;; (global-set-key (kbd "C-x p u") 'my/etags-update)

;;
;; -> selected-window-accent-mode
;;

(use-package selected-window-accent-mode
  :config (selected-window-accent-mode 1)
  :custom
  (selected-window-accent-fringe-thickness 10)
  (selected-window-accent-percentage-darken 0)
  (selected-window-accent-percentage-desaturate 0)
  (selected-window-accent-smart-borders t)
  (selected-window-accent-use-blend-background nil)
  (selected-window-accent-use-blend-alpha 0)
  (selected-window-accent-tab-accent t)
  (selected-window-accent-use-pywal t)
  (selected-window-accent-custom-color "cyan4")
  (selected-window-accent-mode-style 'default))

(eval-after-load 'selected-window-accent-mode
  '(progn
     (define-key global-map (kbd "C-c w") 'selected-window-accent-transient)))

;;
;; -> push-block
;;

(setq my/push-block-spec
      '(
        (:hugo "~/DCIM/content/tagged--all.org" "" "" "" "" "" "")
        (:hugo "~/DCIM/content/art--all.org" "" "" "" "" "" "")
        (:hugo "~/DCIM/content/emacs--all.org" "" "" "" "" "" "")
        (:hugo "~/DCIM/content/kate--blog.org" "" "" "" "" "" "")
        (:hugo"~/DCIM/content/linux--all.org" "" "" "" "" "" "")
        (:hugo "~/DCIM/content/blog--all.org" "" "" "" "" "" "")
        )
      )

(setq my/push-block-flush-lines '("----" "====" "~~~~" "<file:"))

(setq dst-valid '(:raw :org :ascii :hugo))

(defun my/push-block (&optional value)
  "Export content from one file to another in various formats given VALUE."
  (interactive "p")
  (save-excursion
    (dolist (item my/push-block-spec)
      (let* ((format-spec (nth 0 item))
             (source-file (expand-file-name (nth 1 item)))
             (export-file (expand-file-name (nth 2 item)))
             (source-start-tag (nth 3 item))
             (source-end-tag (nth 4 item))
             (export-start-tag (nth 5 item))
             (export-end-tag (nth 6 item))
             (prefix-string (nth 7 item))
             (buff-contents "")
             (in-current (string-equal (expand-file-name (buffer-file-name)) source-file)))

        (access-file source-file "source file")
        (access-file export-file "export file")

        (if (or in-current (> value 1)) ;; should I process file?
            (if (memq format-spec dst-valid) ;; check for valid dst format
                (progn
                  (pcase format-spec
                    (:hugo
                     (without-gc #'org-hugo-export-wim-to-md)
                     ;; (org-hugo-export-wim-to-md)
                     (mapc 'shell-command
                           '("web rsync emacs" "web rsync art"
                             "web rsync dyerdwelling")))
                    (save-excursion ;; other org processing
                      (setq tmp-file (make-temp-file "tmp"))
                      (when (and (stringp source-start-tag) (stringp source-end-tag)
                                 (not (string-empty-p source-start-tag)) (not (string-empty-p source-end-tag)))
                        (goto-char (point-min))
                        (re-search-forward source-start-tag nil t)
                        (let ((start (point)))
                          (re-search-forward source-end-tag nil t)
                          (backward-char (length source-end-tag))
                          (narrow-to-region start (point))))

                      ;; perform the export
                      (pcase format-spec
                        (:raw (write-region (point-min)(point-max) tmp-file)) ;; just raw text
                        (progn
                          ;; lets go through org output
                          (org-export-to-file (pcase format-spec
                                                (:ascii 'ascii)
                                                (:html 'html)
                                                (:icalendar 'icalendar)
                                                (:latex 'latex)
                                                (:odt 'odt)) tmp-file)))
                      (widen)
                      ;; modify the export
                      (with-temp-buffer
                        (insert-file-contents tmp-file)
                        ;; remove duplicates
                        (delete-duplicate-lines (point-min)(point-max) nil t nil)
                        ;; filter lines
                        (mapcar (lambda (x)
                                  (goto-char (point-min))
                                  (flush-lines x)) my/push-block-flush-lines)
                        ;; apply prefix
                        (when (not (s-blank-p prefix-string))
                          (goto-char (point-min))
                          (while (search-forward-regexp "^" nil t)
                            (replace-match prefix-string)))
                        (whitespace-cleanup)
                        ;; write to file
                        (setq buff-contents (buffer-substring (point-min)(buffer-size))))

                      ;; transplant src block to dst block
                      (with-temp-buffer
                        (insert-file-contents export-file)
                        (goto-char (point-min))
                        (re-search-forward export-start-tag nil t)
                        ;; delete destination region
                        (let ((insert-point (point)))
                          (re-search-forward export-end-tag nil t)
                          (backward-char (length export-end-tag))
                          (delete-region insert-point (point)))
                        ;; insert transformed input
                        (insert buff-contents)
                        ;; write to file
                        (write-region (point-min)(point-max) export-file)))))
              (message (format "Invalid format-spec %s not in %s" format-spec dst-valid))))))))

;;
;; -> kurecolor
;;

(use-package kurecolor
  :ensure t ; Ensure the package is installed (optional)
  :bind (("M-<up>" . (lambda () (interactive) (kurecolor-increase-brightness-by-step 0.2)))
         ("M-<down>" . (lambda () (interactive) (kurecolor-decrease-brightness-by-step 0.2)))
         ("M-<prior>" . (lambda () (interactive) (kurecolor-increase-saturation-by-step 0.2)))
         ("M-<next>" . (lambda () (interactive) (kurecolor-decrease-saturation-by-step 0.2)))
         ("M-<left>" . (lambda () (interactive) (kurecolor-decrease-hue-by-step 0.2)))
         ("M-<right>" . (lambda () (interactive) (kurecolor-increase-hue-by-step 0.2))))
  :config
  (global-set-key (kbd "M-<home>") 'my/insert-random-color-at-point))

(defun my/insert-random-color-at-point ()
  "Generate random color and insert at current hex color under cursor."
  (interactive)
  (let* ((color (format "#%06x" (random (expt 16 6))))
         (bounds (bounds-of-thing-at-point 'sexp))
         (start (car bounds))
         (end (cdr bounds)))
    (if (and bounds (> end start))
        (progn
          (goto-char start)
          (unless (looking-at "#[0-9a-fA-F]\\{6\\}")
            (error "Not on a hex color code"))
          (delete-region start end)
          (insert color))
      (error "No hex color code at point"))))

;;
;; -> shell
;;

(when (file-exists-p "/usr/bin/fish")
  (setq explicit-shell-file-name "/usr/bin/fish"))

(when (file-exists-p "/bin/fish")
  (setq shell-file-name "/bin/fish"))

(use-package chatgpt-shell
  :defer 5
  :custom
  ((chatgpt-shell-openai-key
    (lambda ()
      (auth-source-pass-get 'secret "openai-key")))))

(defun my/eshell-hook ()
  "Set up company completions to be a little more fish like."
  (interactive)
  (setq-local completion-styles '(basic partial-completion))
  (setq-local corfu-auto nil)
  (corfu-mode)
  (setq-local completion-at-point-functions
              (list (cape-capf-super
                     #'pcomplete-completions-at-point
                     #'cape-history)))
  ;; (define-key eshell-mode-map (kbd "<tab>") #'company-complete)
  (define-key eshell-hist-mode-map (kbd "M-r") #'consult-history))

(defun my/shell-hook ()
  "Set up company completions to be a little more fish like."
  (interactive)
  ;; (define-key shell-mode-map (kbd "<tab>") #'company-complete)
  (define-key shell-mode-map (kbd "M-r") #'consult-history))

(use-package eshell
  :config
  (setq eshell-scroll-to-bottom-on-input t)
  (setq-local tab-always-indent 'complete)
  (setq eshell-history-size 10000) ;; Adjust size as needed
  (setq eshell-save-history-on-exit t) ;; Enable history saving on exit
  (setq eshell-hist-ignoredups t) ;; Ignore duplicates
  :hook
  (eshell-mode . my/eshell-hook))

(use-package shell
  :config
  (setq-local tab-always-indent 'complete)
  :hook
  (shell-mode . my/shell-hook))

(use-package popper
  :init
  (setq popper-reference-buffers
        '("\\*eshell.*"
          "\\*convert.*"
          "\\*eldoc.*"
          flymake-diagnostics-buffer-mode
          help-mode
          compilation-mode))
  (popper-mode 1)
  (popper-echo-mode 1)
  :custom
  (popper-window-height 15))

(bind-key* (kbd "C-'") #'popper-toggle)
(bind-key* (kbd "C-;") #'popper-toggle-type)

;;
;; -> calendar
;;

(use-package calfw)
(use-package calfw-org)
(use-package calfw-cal)

(setq calendar-holidays nil)
(setq calendar-week-start-day 1)

(setq cfw:org-capture-template
      '("c" "Calendar" plain
        (file+function
         "~/DCIM/content/aab--calendar.org"
         my-capture-top-level)
        "* TODO %?\n SCHEDULED: %(cfw:org-capture-day)\n"
        :prepend t :jump-to-captured t))

;;
;; —> proced
;;

(use-package proced
  :bind (("C-x x p" . proced)
         :map proced-mode-map
         ("f" . proced-narrow)
         ("P" . my/proced-toggle-update))
  :init
  (setq proced-auto-update-interval 1
        proced-enable-color-flag 1
        proced-format 'medium
        proced-sort 'rss)
  (defun my/proced-toggle-update()
    "Proced turn auto update on and off."
    (interactive)
    (if proced-auto-update-flag
        (proced-toggle-auto-update -1)
      (proced-toggle-auto-update 1)))
  (defun proced-settings()
    "Initial settings for proced-mode."
    (proced-toggle-auto-update 1))
  :hook
  ((proced-mode . (lambda ()
                    (interactive)
                    (proced-toggle-auto-update 1)))
   (proced-mode . proced-settings))
  :config
  (use-package proced-narrow
    :after proced))

;;
;; -> repeat
;;

(defun my/repeat-thing (func)
  "Call FUNC and set up a sparse keymap for repeating actions."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") (lambda () (interactive) (my/repeat-thing #'my/next-thing)))
    (define-key map (kbd "p") (lambda () (interactive) (my/repeat-thing #'my/previous-thing)))
    (define-key map (kbd "j") (lambda () (interactive) (my/repeat-thing #'tab-bar-history-back)))
    (define-key map (kbd "k") (lambda () (interactive) (my/repeat-thing #'tab-bar-history-forward)))
    (funcall func)
    (set-transient-map map t)))

(global-set-key (kbd "M-s n") (lambda () (interactive) (my/repeat-thing #'my/next-thing)))
(global-set-key (kbd "M-s p") (lambda () (interactive) (my/repeat-thing #'my/previous-thing)))
(bind-key* (kbd "C-x j") (lambda () (interactive) (my/repeat-thing #'tab-bar-history-back)))
(bind-key* (kbd "C-x k") (lambda () (interactive) (my/repeat-thing #'tab-bar-history-forward)))

;;
;; -> tab-bar
;;

(setq display-time-day-and-date t)
(setq display-time-interval 4)
(setq display-time-load-average-threshold 2.0)

(defun my-tab-line-buffer-name ()
  "Return the current buffer name for use in the tab line."
  (let ((buffer-name (buffer-name)))
    (propertize (concat buffer-name "  ") 'face 'default)))

(use-package tab-bar ;; 29.1
  :ensure nil ;; Since tab-bar is built-in, no package needs to be downloaded
  :init
  (tab-bar-mode 1) ;; 27.1
  (tab-bar-history-mode 1) ;; 27.1
  :custom
  (tab-bar-format '(tab-bar-format-tabs-groups
                    tab-bar-format-align-right
                    ;; my-tab-line-buffer-name
                    tab-bar-format-global)) ;; 28.1
  ;; (tab-bar-select-tab-modifiers '(control)) ;; 27.1
  (tab-bar-show 1)
  (tab-bar-new-button-show nil) ;; 27.1
  (tab-bar-close-button-show nil) ;; 27.1
  (tab-bar-history-limit 100) ;; 27.1
  (tab-bar-auto-width-max '(200 20)) ;; 29.1
  ;; (tab-bar-tab-hints t) ;; 27.1
  ;; (tab-bar-tab-name-format-function #'my-tab-bar-tab-name-format) ;; 28.1
  :config
  (defun my-tab-bar-tab-name-format (tab i)
    (propertize
     (format " %d " i)
     'face (funcall tab-bar-tab-face-function tab))))

(bind-key* (kbd "M-u") #'tab-bar-switch-to-prev-tab)
(bind-key* (kbd "M-i") #'tab-bar-switch-to-next-tab)

;;
;; -> finance
;;

(use-package csv)
(require 'csv)

(defvar payments '())
(defvar cat-tot (make-hash-table :test 'equal))

(setq cat-list-defines
      '(("kate" "kat")
        ("railw\\|railway\\|selfserve\\|train" "trn")
        ("paypal" "pay")
        ("royal-mail\\|postoffice\\|endsleigh\\|waste\\|lloyds\\|electric\\|sse\\|newsstand\\|privilege\\|pcc\\|licence\\|ovo\\|energy\\|bt\\|water" "utl")
        ("sky-betting\\|b365\\|races\\|bet365\\|racing" "bet")
        ("stakeholde\\|widows" "pen")
        ("nsibill\\|vines\\|ns&i\\|saver" "sav")
        ("streamline" "hlt")
        ("clifford" "thr")
        ("daltontags\\|dyer\\|julia" "fam")
        ("uber\\|aqua" "txi")
        ("magazine\\|specs\\|zinio\\|specsavers\\|publishing\\|anthem\\|kindle\\|news" "rdg")
        ("escape\\|deviant\\|cleverbridge\\|reddit\\|pixel\\|boox\\|ionos\\|microsoft\\|mobile\\|backmarket\\|cartridge\\|whsmith\\|dazn\\|my-picture\\|openai\\|c-date\\|ptitis\\|keypmt\\|billnt\\|fee2nor\\|assistance\\|boxise\\|billkt\\|paintstor\\|iet-main\\|ffnhelp\\|shadesgrey\\|venntro\\|vtsup\\|sunpts\\|apyse\\|palchrge\\|maypmt\\|filemodedesk\\|istebrak\\|connective\\|avangate\\|stardock\\|avg\\|123\\|web\\|a2" "web")
        ("anchrg\\|hilsea\\|withdrawal" "atm")
        ("finance" "fin")
        ("twitch\\|disney\\|box-office\\|discovery\\|tvplayer\\|vue\\|sky\\|netflix\\|audible\\|nowtv\\|channel\\|prime" "str")
        ("google" "goo")
        ("platinum\\|card" "crd")
        ("top-up\\|three\\|h3g" "phn")
        ("lockart\\|moment-house\\|yuyu\\|bushra\\|newhome\\|white-barn\\|skinnydip\\|mgs\\|river-island\\|spencer\\|lilian\\|jung\\|ikea\\|wayfair\\|neom\\|teespring\\|lick-home\\|matalan\\|devon-wick\\|united-arts\\|lush-retail\\|lisa-angel\\|sharkninja\\|fastspring\\|bonas\\|asos\\|emma\\|sofology\\|ebay\\|dunelm\\|coconut\\|semantical\\|truffle\\|nextltd\\|highland\\|little-crafts\\|papier\\|the-hut\\|new-look\\|samsung\\|astrid\\|pandora\\|waterstone\\|cultbeauty\\|24pymt\\|champo\\|costa\\|gollo\\|pumpkin\\|argos\\|the-range\\|biffa\\|moonpig\\|apple\\|itunes\\|gold\\|interflora\\|thortful" "shp")
        ("pets\\|pet" "pet")
        ("residential\\|rent\\|yeong" "rnt")
        ("amaz\\|amz" "amz")
        ("asda\\|morrison\\|sainsburys\\|waitrose\\|tesco\\|domino\\|deliveroo\\|just.*eat" "fod")
        (".*" "o")))

(length cat-list-defines)

(defun categorize-payment (name debit month)
  "Categorize payment based on name, month, and accumulate totals."
  (let* ((category-found)
         (split-key))
    (cl-block nil
      (dolist (category cat-list-defines)
        (when (string-match-p
               (nth 0 category) name)
          (setq category-found (nth 1 category))
          (cl-return))))
    (setq split-key (concat month "-" category-found))
    (insert (format "%s %s %s %.0f\n" category-found month name debit))
    (puthash split-key (+ (gethash split-key cat-tot 0) debit) cat-tot)))

(defun parse-csv-file (file)
  "Parse CSV file and store payments."
  (with-temp-buffer
    (insert-file-contents file)
    (setq payments (csv-parse-buffer t))))

(defun write-header-plot (year)
  "Generate a plot header for YEAR."
  (insert "-*- mode: org; eval: (visual-line-mode -1); -*-\n")
  (insert (format "#+PLOT: title:\"%s\" ind:1 deps:(%s) type:2d with:lines set:\"yrange [0:1000]\"\n"
                  year (concat (mapconcat 'number-to-string (number-sequence 3 (+ (length cat-list-defines) 2)) " ")))))

(defun write-footer-tblfm ()
  "Generate a plot footer."
  (insert "||\n")
  (insert (concat "#+TBLFM: @>=vmean(@I..@II);%.0f::$>=vsum($3..$" (format "%d" (+ (length cat-list-defines) 2)) ");\%.0f") ))

(defun write-header ()
  "Write table header."
  (insert "|date ")
  (dolist (category cat-list-defines)
    (insert (format "%s " (nth 1 category))))
  (insert " |\n"))

(defun write-body (index year month)
  "Write table body."
  (insert (format "%d %s " index (concat year "-" month)))
  (dolist (category cat-list-defines)
    (let* ((split-key (concat year "-" month "-" (nth 1 category))))
      (insert (format "%.0f " (gethash split-key cat-tot 0)))))
  (insert " |\n"))

(defun export-payments-to-org ()
  "Export categorized payments and totals to an Org table."
  (clrhash cat-tot)
  ;; calculate all payments and output all categories to payments-all.org
  (with-temp-buffer
    (dolist (payment payments)
      (let* ((date (cdr (nth 0 payment)))
             (month (format-time-string "%Y-%m" (date-to-time date)))
             (name (string-replace " " "-" (cdr (nth 4 payment))))
             (debit (string-to-number (cdr (nth 5 payment)))))
        (categorize-payment name debit month)))
    (write-file "payments-all.org"))

  ;; output entire payments table to payments.org
  (with-temp-buffer
    (write-header-plot 2024)
    (write-header)
    (let ((index 0))
      (dolist (year (seq-map '(lambda (value)
                                (format "%02d" value))
                             (nreverse (number-sequence 2016 2024 1))))
        (dolist (month (seq-map '(lambda (value)
                                   (format "%02d" value))
                                (nreverse (number-sequence 1 12 1))))
          (write-body index year month)
          (setq index (1+ index)))))
    (write-footer-tblfm)
    (write-file "payments.org"))

  ;; output payments to payments-<year>.org
  (dolist (year (seq-map '(lambda (value)
                            (format "%02d" value))
                         (nreverse (number-sequence 2016 2024 1))))
    (with-temp-buffer
      (write-header-plot year)
      (write-header)
      (let ((index 0))
        (dolist (month (seq-map '(lambda (value)
                                   (format "%02d" value))
                                (nreverse (number-sequence 1 12 1))))
          (write-body index year month)
          (setq index (1+ index))))
      (write-footer-tblfm)
      (write-file (concat "payments-" year ".org"))))

  ;; output payments to payments-<category>.org
  (dolist (category cat-list-defines)
    (with-temp-buffer
      (insert (format "#+PLOT: title:\"%s\" ind:1 deps:(3) type:2d with:lines set:\"yrange [0:1000]\"\n" (nth 1 category)))
      (insert "|date ")
      (insert (format "%s\n" (nth 1 category)))
      (let ((index 0))
        (dolist (year (seq-map '(lambda (value)
                                  (format "%02d" value))
                               (nreverse (number-sequence 2016 2024 1))))
          (dolist (month (seq-map '(lambda (value)
                                     (format "%02d" value))
                                  (nreverse (number-sequence 1 12 1))))
            (let* ((split-key (concat year "-" month "-" (nth 1 category))))
              (insert (format "%d %s " index (concat year "-" month)))
              (insert (format "%.0f\n" (gethash split-key cat-tot 0))))
            (setq index (1+ index)))))
      (write-file (concat "payments-" (nth 1 category) ".org")))))

;; Example usage
;; (parse-csv-file "payments.csv")
;; (export-payments-to-org)

(defun my/remove-negative-sign (input-line)
  "Remove the negative sign from the final column of a CSV line."
  (if (string-match "\\(.*\\),-\\([0-9.]+\\)$" input-line)
      (replace-match "\\1,\\2" nil nil input-line)
    input-line))

(defun my/remove-negative-sign-from-buffer ()
  "Remove the negative sign from the final column of all CSV lines in the current buffer."
  (interactive)
  (save-excursion  ; Preserve buffer and point position
    (goto-char (point-min))  ; Start at the beginning of the buffer
    (while (not (eobp))  ; While not at the end of the buffer
      (let ((line (thing-at-point 'line t)))
        (let ((processed-line (my/remove-negative-sign line)))
          (progn
            (beginning-of-line)
            (kill-line)
            (insert processed-line)))))))

(defun my/convert-date-format ()
  "Convert date formats from DD/MM/YYYY to YYYY-MM-DD in the current buffer."
  (interactive)
  (goto-char (point-min)) ; Start from the beginning of the buffer
  (while (re-search-forward "\\([0-3][0-9]\\)/\\([0-1][0-9]\\)/\\([0-9]\\{4\\}\\)" nil t)
    (let ((day (match-string 1))
          (month (match-string 2))
          (year (match-string 3)))
      (replace-match (concat year "-" month "-" day)))))

;;
;; -> plantuml
;;

(use-package plantuml-mode
  :custom
  (plantuml-default-exec-mode 'jar)
  (plantuml-jar-path (concat user-emacs-directory "plantuml.jar"))
  (org-plantuml-jar-path (concat user-emacs-directory "plantuml.jar")))

(add-to-list
 'org-src-lang-modes '("plantuml" . plantuml))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((plantuml . t)))

(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))

(add-hook 'plantuml-mode-hook (lambda ()
                                (setq tab-width 0)
                                (setq indent-tabs-mode nil)))


(defun my-org-confirm-babel-evaluate (lang body)
  (not (or (string= lang "plantuml")
           (string= lang "emacs-lisp"))))

(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

;;
;; -> next-thing
;;

(defun my/get-window-regex (regex)
  "Find the first window displaying a buffer whose name matches the given REGEX.
                   If no such window is found, return nil."
  (let ((windows (window-list))
        (found-window nil))
    (dolist (window windows found-window)
      (when (string-match-p regex (buffer-name (window-buffer window)))
        (setq found-window window)
        (cl-return found-window)))))

(defun my/navigate-thing (prev-or-next-funcs)
  "Navigate through things using a list of corresponding functions."
  (let ((window nil))
    (dolist (func prev-or-next-funcs)
      (setq window (my/get-window-regex (nth 0 func)))
      (when window
        (apply 'select-window (list window))
        (dolist (f (nth 1 func))
          (funcall f))))))

(defun my/previous-thing ()
  "Go to the previous thing, meaning warning, error, grep, etc."
  (interactive)
  (my/navigate-thing
   '(("compilation" (previous-error))
     ("compile-log" (previous-error-no-select compile-goto-error))
     ("dead" (deadgrep-backward-match deadgrep-visit-result-other-window org-fold-show-entry))
     ("xref" (xref-prev-line))
     ("org agenda" ((lambda () (org-agenda-previous-item 1)) org-agenda-goto org-fold-show-entry))
     ("consult-ripgrep" (backward-button push-button org-fold-show-entry))
     ("occur" (previous-error))
     ("flycheck errors" (previous-error)))))

(defun my/next-thing ()
  "Go to the next thing, meaning warning, error, grep, etc."
  (interactive)
  (my/navigate-thing
   '(("compilation" (next-error))
     ("compile-log" (next-error-no-select compile-goto-error))
     ("dead" (deadgrep-forward-match deadgrep-visit-result-other-window org-fold-show-entry))
     ("xref" (xref-next-line))
     ("org agenda" ((lambda () (org-agenda-next-item 1)) org-agenda-goto org-fold-show-entry))
     ("consult-ripgrep" (forward-button push-button org-fold-show-entry))
     ("occur" (next-error))
     ("flycheck errors" (next-error)))))

;;
;; -> word-count
;;

(use-package wc-mode
  ;; :hook
  ;; (org-mode . wc-mode)
  :custom
  (wc-modeline-format "WC:%tw"))

(defun my/word-count-function (rstart rend)
  "Counts words without lines containing 'DONE' 'PROPERTIES' 'END:' drawers.
Or indeed other filters as defined in the main unless from RSTART and REND."
  (let ((count 0))
    (save-excursion
      (goto-char rstart)
      (while (< (point) rend)
        (let ((line (buffer-substring-no-properties (line-beginning-position)
                                                    (line-end-position))))
          (unless (or
                   (string-match-p "\\* DONE" line)
                   (string-match-p "\\* TODO" line)
                   (string-match-p "file\:" line)
                   (and (string-match-p ":PROPERTIES:" line) (re-search-forward ":END:" nil t))
                   (and (string-match-p "\\#\\+begin_src" line) (re-search-forward "\\#\\+end_src" nil t))
                   (string-match-p "\\#\\+" line))
            (setq count (+ count (1+ (how-many " " (line-beginning-position) (line-end-position))))))
          ;; (setq count (+ count (length (split-string line "\\W+" t)))))
          (forward-line 1))))
    count))

(defun my/count-words ()
  "Counts words in current buffer"
  (interactive)
  (message
   (format
    "Found %s words"
    (my/word-count-function (point-min) (point-max)))))

;; Set the custom wc-mode counting function
(setq wc-count-words-function 'my/word-count-function)

;;
;; -> windows-specific
;;

(when (eq system-type 'windows-nt)
  (setq home-dir "c:/users/jimbo")
  (let ((xPaths
         `(,(expand-file-name "~/bin")
           ,(expand-file-name "~/bin/PortableGit/bin")
           ,(expand-file-name "~/bin/PortableGit/usr/bin")
           ,(expand-file-name "~/bin/Apache-Subversion/bin/")
           ,(expand-file-name "~/bin/svn2git-2.4.0/bin")
           ,(expand-file-name "~/bin/clang/bin")
           ,(expand-file-name "~/bin/find")
           ,(expand-file-name "~/bin/omnisharp-win-x64")
           "c:/GnuWin32/bin"
           "c:/GNAT/2021/bin")))
    (setenv "PATH" (mapconcat 'identity xPaths ";"))
    (setq exec-path (append xPaths (list "." exec-directory))))

  (custom-theme-set-faces
   'user
   '(variable-pitch ((t (:family "Consolas" :height 110 :weight normal))))
   '(fixed-pitch ((t ( :family "Consolas" :height 110)))))

  (setq font-general "Consolas 11")
  (set-frame-font font-general nil t)
  (add-to-list 'default-frame-alist `(font . ,font-general)))

(setq tab-bar-show 1)

;;
;; -> linux specific
;;

(when (eq system-type 'gnu/linux)
  (define-key my-jump-keymap (kbd "a") #'emms-browse-by-album)
  (define-key my-jump-keymap (kbd "b") (lambda () (interactive) (find-file "~/bin")))
  (define-key my-jump-keymap (kbd "c") (lambda () (interactive) (find-file "~/DCIM/Camera")))
  (define-key my-jump-keymap (kbd "g") (lambda () (interactive) (find-file "~/.config")))
  (define-key my-jump-keymap (kbd "j") (lambda () (interactive) (find-file "~/DCIM/content/aaa--todo.org")))
  (define-key my-jump-keymap (kbd "n") (lambda () (interactive) (find-file "~/DCIM/Screenshots")))
  (define-key my-jump-keymap (kbd "w") (lambda () (interactive) (find-file "~/DCIM/content/")))
  (define-key my-jump-keymap (kbd "y") #'emms)

  (setq diary-file "~/DCIM/content/diary.org")

  (custom-theme-set-faces
   'user
   '(variable-pitch ((t (:family "DejaVu Sans" :height 120 :weight normal))))
   '(fixed-pitch ((t ( :family "Source Code Pro" :height 110)))))

  ;; (setq font-general "Noto Sans Mono 11")
  (setq font-general "Source Code Pro 12")
  ;; (setq font-general "Source Code Pro Light 11")
  ;; (setq font-general "Nimbus Mono PS 11")
  ;; (setq font-general "Monospace 11")
  (set-frame-font font-general nil t)
  (add-to-list 'default-frame-alist `(font . ,font-general))

  (use-package all-the-icons-dired
    :diminish
    all-the-icons-dired-mode
    :hook
    (dired-mode . all-the-icons-dired-mode))

  (use-package all-the-icons-ibuffer
    :ensure t
    :hook
    (ibuffer-mode . all-the-icons-ibuffer-mode))

  (use-package all-the-icons-completion
    :ensure t
    :init
    (all-the-icons-completion-mode)
    :hook
    (marginalia-mode all-the-icons-completion-marginalia-setup))

  (when (file-exists-p "~/source/repos/fd-find")
    (use-package fd-find
      :load-path "~/source/repos/fd-find")))

;;
;; -> SWIG
;;

(require 'subr-x)

(defun swig--parse-include-directives (file)
  "Parse the #include directives from a header FILE."
  (let ((includes '()))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward "^#include [\"<]\\(.*?\\)[\">]" nil t)
        (let ((included-file (match-string 1)))
          (push included-file includes))))
    includes))

(defun swig--all-files (path)
  "Get all SWIG includes with some filtering based off PATH."
  (remove-if (lambda (x) (string-match-p "\\(?:MSVS\\|CigiProcessType\\|CigiCnvtInfoType\\)" x))
             (directory-files-recursively path "\\.h\\|\\.hpp")))

(defun swig--build-dependency-graph (include-dir)
  "Build a dependency graph for header files in INCLUDE-DIR."
  (let ((graph (make-hash-table :test 'equal))
        (all-headers '()))
    (dolist (file (swig--all-files include-dir))
      (let ((relative-file (file-relative-name file include-dir))
            (dependencies (swig--parse-include-directives file)))
        (puthash relative-file dependencies graph)
        (push relative-file all-headers)))
    (list graph all-headers)))

(defun swig--topological-sort (graph all-headers)
  "Perform topological sort on GRAPH with ALL-HEADERS."
  (let ((in-degree (make-hash-table :test 'equal))
        (sorted '())
        (queue '()))
    ;; Initialize in-degree of all headers
    (dolist (header all-headers)
      (puthash header 0 in-degree))
    ;; Calculate in-degrees
    (maphash (lambda (u dependencies)
               (dolist (v dependencies)
                 (when (gethash v in-degree)
                   (puthash v (1+ (gethash v in-degree)) in-degree))))
             graph)
    ;; Enqueue headers with in-degree 0
    (maphash (lambda (header degree)
               (when (= degree 0)
                 (push header queue)))
             in-degree)
    ;; Process the queue
    (while queue
      (let ((u (pop queue)))
        (push u sorted)
        (dolist (v (gethash u graph))
          (when (gethash v in-degree)
            (puthash v (1- (gethash v in-degree)) in-degree)
            (when (= (gethash v in-degree) 0)
              (push v queue))))))
    ;; Check for cycles
    (if (= (length sorted) (length all-headers))
        sorted
      (error "Cycle detected in the dependency graph"))))

(defun swig--generate-include-file-list (include-dir interface-file)
  "Generate an include file list in dependency order for headers in INCLUDE-DIR."
  (let* ((result (swig--build-dependency-graph include-dir))
         (graph (car result))
         (all-headers (cadr result))
         (sorted-headers (swig--topological-sort graph all-headers)))
    (with-temp-buffer
      (insert "%{\n")
      (dolist (header sorted-headers)
        (insert "#include \"" include-dir "/" header "\"\n"))
      (insert "%}\n")
      (insert "\n%module example_module\n\n")
      (dolist (header sorted-headers)
        (insert "%include \"" include-dir "/" header "\"\n"))
      (write-file interface-file))
    (message "Include file list generated at %s" interface-file)))

;; Usage
(defun swig-test ()
  ""
  (interactive)
  (swig--generate-include-file-list
   "/home/jdyer/source/repos/cigi-ccl_4_0/include"
   "/home/jdyer/source/repos/cigi-ccl_4_0/example.i"))

;;
;; -> transients
;;

(defvar cmake-preset
  "build/linux/debug"
  "cmake-preset")

(defun change-directory-and-run (dir command bufname)
  "Change to DIR and run the COMMAND."
  (let ((default-directory dir))
    (async-shell-command command bufname)
    (message "Running command: %s:%s" dir command)))

(defun run-exe-command (dir exe bufname)
  "Run EXE from a specified DIR."
  (message "run-exe-command: %s:%s:%s" dir exe bufname)
  (change-directory-and-run dir exe bufname))

(defun run-cmake-command (command)
  "Run COMMAND from the top level of the project."
  (message command)
  (change-directory-and-run (project-root (project-current t)) command "*cmake*"))

(defun run-cmake-compile-command (command)
  "Run compile COMMAND from the top level of the project."
  (message command)
  (let ((default-directory (project-root (project-current t))))
    (compile command)
    (message "Running command: %s:%s" dir command)))

(defun kill-async-buffer (buffer-name)
  "Kill the async buffer with BUFFER-NAME."
  (let ((buffer (get-buffer buffer-name)))
    (when buffer
      (kill-buffer buffer)
      (message "Killed buffer: %s" buffer-name))))

(defun list-cmake-presets ()
  "List available CMake presets using `cmake --list-presets=configure`."
  (let ((output (shell-command-to-string "cmake --list-presets=configure")))
    (delq nil
          (mapcar (lambda (line)
                    (if (string-match "^\\s-+\"\\([^\"]+\\)\"\\s-*$" line)
                        (match-string 1 line)))
                  (split-string output "\n")))))

(defun transient-select-cmake-preset ()
  "Function to select a CMake preset."
  (interactive)
  (let* ((presets (list-cmake-presets))
         (preset (completing-read "Select CMake preset: " presets nil t)))
    (setq cmake-preset preset)
    (message "Selected CMake preset: %s" preset)))

(transient-define-prefix build-transient ()
  "Build and Diagnostic transient commands."
  [:description (lambda () (project-root (project-current t)))
                ["CMake"
                 ("p" "Set Preset" transient-select-cmake-preset)
                 ("c" "Configure"
                  (lambda () (interactive)
                    (run-cmake-command (format "cmake --preset %s" cmake-preset))))
                 ("RET" "Build"
                  (lambda () (interactive)
                    (run-cmake-compile-command (format "cmake --build --preset %s" cmake-preset))))
                 ("i" "Install"
                  (lambda () (interactive)
                    (run-cmake-command (format "cmake --install %s" cmake-preset))))
                 ("g" "Refresh"
                  (lambda () (interactive)
                    (run-cmake-command (format "cmake --preset %s --fresh" cmake-preset))))
                 ("x" "Clean"
                  (lambda () (interactive)
                    (if (y-or-n-p "Are you sure you want to proceed? ")
                        (run-cmake-command "rm -rf build"))))
                 ;; ("m" "Toggle compilation"
                 ;;   (lambda () (interactive)
                 ;;     (let ((buffer (get-buffer "*compilation*")))
                 ;;       (if buffer
                 ;;         (if (get-buffer-window buffer 'visible)
                 ;;           (delete-windows-on buffer)
                 ;;           (display-buffer buffer))
                 ;;         (message "No *compilation* buffer found.")))))
                 ("s" "List Presets"
                  (lambda () (interactive)
                    (run-cmake-command "cmake --list-presets=configure")))]
                ["Actions"
                 ("SPC" "File Backup" my/dired-duplicate-backup-file)
                 ("f" "Toggle Flycheck" flymake-mode)
                 ("d" "Show Flycheck Diagnostics" flymake-show-buffer-diagnostics)]
                ["Coding"
                 ("e" "Fancy Stuff"
                  (lambda () (interactive)
                    (call-interactively 'eglot)
                    (company-mode 1)
                    (flymake-mode 1)))
                 ("u" "Undo Fancy Stuff"
                  (lambda () (interactive)
                    (eglot-shutdown-all)
                    (company-mode -1)
                    (flymake-mode -1)))
                 ("h" "Stop eglot"
                  (lambda () (interactive)
                    (eglot-shutdown-all)))]
                ["Run"
                 ("r" "All"
                  (lambda () (interactive)
                    (run-exe-command
                     (concat (project-root (project-current t))
                             "build/windows/debug/bin/Debug")
                     "CigiDummyIG.exe" "*Running CigiDummyIG.exe*")
                    (run-exe-command
                     (concat (project-root (project-current t))
                             "build/windows/debug/bin/Debug")
                     "CigiMiniHostCSharp.exe" "*Running CigiMiniHostCSharp.exe*")))
                 ("1" "CigiDummyIG"
                  (lambda () (interactive)
                    (run-exe-command
                     (concat (project-root (project-current t))
                             "build/windows/debug/bin/Debug")
                     "CigiDummyIG.exe"
                     "*Running CigiDummyIG.exe*")))
                 ("2" "CigiMiniHost"
                  (lambda () (interactive)
                    (run-exe-command
                     (concat (project-root (project-current t))
                             "build/windows/debug/bin/Debug")
                     "CigiMiniHost.exe"
                     "*Running CigiMiniHost.exe*")))
                 ("3" "CigiMiniHostCSharp"
                  (lambda () (interactive)
                    (run-exe-command
                     (concat (project-root (project-current t))
                             "build/windows/debug/bin/Debug")
                     "CigiMiniHostCSharp.exe"
                     "*Running CigiMiniHostCSharp.exe*")))]
                ["Kill"
                 ("5" "CigiDummyIG (k)"
                  (lambda () (interactive)
                    (kill-async-buffer "*Running CigiDummyIG.exe*")))
                 ("6" "CigiMiniHost (k)"
                  (lambda () (interactive)
                    (kill-async-buffer "*Running CigiMiniHost.exe*")))
                 ("7" "CigiMiniHostCSharp (k)"
                  (lambda () (interactive)
                    (kill-async-buffer "*Running CigiMiniHostCSharp.exe*")))
                 ("k" "All (k)"
                  (lambda () (interactive)
                    (kill-async-buffer "*Running CigiDummyIG.exe*")
                    (kill-async-buffer "*Running CigiMiniHost.exe*")
                    (kill-async-buffer "*Running CigiMiniHostCSharp.exe*")))]
                ])

(global-set-key (kbd "M-RET") #'build-transient)

(transient-define-prefix my/transient-outlining-and-folding ()
  "Transient menu for outline-mode."
  ["Outline Mode Commands"
   ["Cycle / Folding"
    ("g" "Cycle" outline-cycle)
    ("O" "Cycle Buffer" outline-cycle-buffer)
    ("F" "Global Folding at Point"
     (lambda () (interactive)
       (if (eq selective-display (1+ (current-column)))
           (set-selective-display 0)
         (set-selective-display (1+ (current-column))))))]
   ["Visibility"
    ("o" "Toggle Children" outline-toggle-children)
    ("h" "Hide Sublevels" outline-hide-sublevels)
    ("s" "Show All" outline-show-all)
    ("i" "Hide Body" outline-hide-body)
    ("e" "Show Entry" outline-show-entry)
    ("H" "Hide Entry" outline-hide-entry)
    ("c" "Hide Leaves" outline-hide-leaves)
    ("k" "Show Branches" outline-show-branches)
    ("t" "Hide Subtree" outline-hide-subtree)
    ("S" "Show Subtree" outline-show-subtree)]
   ["Motion"
    ("n" "Next Visible Heading" outline-next-visible-heading)
    ("p" "Previous Visible Heading" outline-previous-visible-heading)
    ("u" "Up Heading" outline-up-heading)
    ("f" "Forward Same Level" outline-forward-same-level)
    ("b" "Backward Same Level" outline-backward-same-level)]
   ["Structure"
    ("t" "Promote Heading" outline-promote)
    ("d" "Demote Heading" outline-demote)
    ("P" "Move Subtree Up" outline-move-subtree-up)
    ("N" "Move Subtree Down" outline-move-subtree-down)]
   ["Edit"
    ("a" "Add Heading" outline-insert-heading)
    ("r" "Rename Heading" outline-insert-heading)
    ("m" "Mark Subtree" outline-mark-subtree)]])

(bind-key* (kbd "C-c o") 'my/transient-outlining-and-folding)

(defun my/prog-folding ()
  "Enable and configure outline minor mode for code folding.

This function sets up the outline minor mode tailored for
programming modes based on basic space / tab indentation."
  (interactive)
  (setq-local outline-minor-mode-use-buttons nil)
  (setq-local outline-regexp (rx bol
                                 (zero-or-more (any " \t"))
                                 (not (any " \t\n"))))
  (outline-minor-mode 1))

(add-hook 'prog-mode-hook 'my/prog-folding)

(transient-define-prefix chatgpt-shell-transient ()
  "Transient for ChatGPT Shell commands."
  ["ChatGPT Shell Commands"
   ["Code and Text"
    ;; ("e" "Explain Code" chatgpt-shell-explain-code)
    ("p" "Proofread Region" chatgpt-shell-proofread-region)
    ("g" "Write Git Commit" chatgpt-shell-write-git-commit)
    ("s" "Send Region" chatgpt-shell-send-region)
    ("d" "Describe Code" chatgpt-shell-describe-code)
    ("r" "Refactor Code" chatgpt-shell-refactor-code)
    ("u" "Generate Unit Test" chatgpt-shell-generate-unit-test)
    ("a" "Send and Review Region" chatgpt-shell-send-and-review-region)]
   ["Shell Operations"
    ("l" "Start Shell" chatgpt-shell)
    ("m" "Swap Model Version" chatgpt-shell-swap-model-version)
    ("t" "Save Session Transcript" chatgpt-shell-save-session-transcript)]
   ["Eshell Integrations"
    ("o" "Summarize Last Command Output" chatgpt-shell-eshell-summarize-last-command-output)
    ("w" "What's Wrong With Last Command" chatgpt-shell-eshell-whats-wrong-with-last-command)]
   ["Miscellaneous"
    ("i" "Describe Image" chatgpt-shell-describe-image)]
   ])

(global-set-key (kbd "C-c g") 'chatgpt-shell-transient)

;;
;; -> development
;;

(defun subtract-weight (weight-str avg-loss)
  "Subtract AVG-LOSS pounds from WEIGHT-STR given in 'stones:pounds' format."
  (let* ((stones-pounds (split-string weight-str ":"))
         (stones (string-to-number (car stones-pounds)))
         (pounds (string-to-number (cadr stones-pounds)))
         (total-pounds (+ pounds (* stones 14)))
         (new-total-pounds (- total-pounds avg-loss))
         (new-stones (truncate (/ new-total-pounds 14)))
         (new-pounds (mod new-total-pounds 14)))
    (format "%d:%d" new-stones new-pounds)))

(defun extrapolate-weight-loss (num-weeks)
  "Extrapolate weight loss for NUM-WEEKS using 'av/pd' value in org-table."
  (interactive "p")
  (save-excursion
    (let ((last-weight)
          (last-avg-loss 2.9)
          (last-date "")
          (week 0)
          (next-date ""))
      (print num-weeks)
      (when (org-at-table-p)
        (goto-char (org-table-end))
        ;; Find the last date and week number
        (search-backward-regexp "|\\s-?\\([0-9]+\\)\\s-?|\\s-?<\\([0-9-]+\\)" nil t)
        (setq week (string-to-number (match-string 1)))
        (setq last-date (match-string 2))
        (setq last-weight "16:10")
        (goto-char (org-table-end))
        ;; Loop for num-weeks to generate new lines
        (dotimes (i num-weeks)
          (setq next-date
                (format-time-string "<%Y-%m-%d %a>"
                                    (time-add (org-time-string-to-time last-date)
                                              (days-to-time (+ (* i 7) 7))))) ;; add 7 days per week
          (setq week (+ week 1))
          (insert (format "| %d | %s | %s | | | | | | |\n"
                          week next-date (subtract-weight last-weight (* (+ i 1) last-avg-loss))))))))
  (org-table-align))

(setq org-icalendar-use-deadline
      '(event-if-not-todo event-if-todo event-if-todo-not-done todo-due))

(setq org-icalendar-use-scheduled
      '(event-if-not-todo event-if-todo event-if-todo-not-done todo-start))

(defun my/calc-subscription-cost ()
  (interactive)
  (let ((sum 0))
    (save-excursion
      (goto-char (point-min))
      ;; Search for the numeric pattern.
      (while (re-search-forward "\\([0-9]+\\.[0-9]+\\)" nil t)
        ;; Check if the current line does not contain "CANCELLED"
        (unless (save-excursion
                  (beginning-of-line)
                  (re-search-forward "CANCELLED" (line-end-position) t))
          ;; If "CANCELLED" is not found on the line, process the number.
          (let ((amount (string-to-number (substring-no-properties (match-string 1)))))
            (setq sum (+ sum amount))
            (message "Adding: %.2f, Total: %.2f" amount sum)))))
    (message "Total: %.2f" sum)))

(if (and (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (message "Native compilation is available")
  (message "Native complation is *not* available"))

(defvar consult--xref-history nil
  "History for the `consult-recent-xref' results.")

(defun consult-recent-xref (&optional markers)
  "Jump to a marker in MARKERS list (defaults to `xref--history'.
The command supports preview of the currently selected marker position.
The symbol at point is added to the future history."
  (interactive)
  (consult--read
   (consult--global-mark-candidates
    (or markers (flatten-list xref--history)))
   :prompt "Go to Xref: "
   :annotate (consult--line-prefix)
   :category 'consult-location
   :sort nil
   :require-match t
   :lookup #'consult--lookup-location
   :history '(:input consult--xref-history)
   :add-history (thing-at-point 'symbol)
   :state (consult--jump-state)))

(defun copy-vcxproj-and-sln-files (src-dir dst-dir)
  "Copy .vcxproj, .vcxproj.filters, and .sln files from SRC-DIR to DST-DIR recursively."
  (let ((files (directory-files-recursively src-dir
                                            "\\.\\(vcxproj\\|sln\\)$"
                                            nil
                                            (lambda (dir)
                                              (not
                                               (or
                                                (string-match "CMakeFiles" dir)
                                                ))))))
    ;; (prin1 files)
    (dolist (file files)
      (message file)
      (let* ((relative-path (file-relative-name file src-dir))
             (new-file (expand-file-name relative-path dst-dir)))
        (unless (file-directory-p (file-name-directory new-file))
          (make-directory (file-name-directory new-file) t))
        (copy-file file new-file t)))))

;; Usage example:
;; (copy-vcxproj-and-sln-files "c:/Users/jimbo/source/cigi-ccl_4_0/build/windows/debug" "c:/Users/jimbo/source/cigi-ccl_4_0")

(defun my/org-replace-inactive-timestamps ()
  "Replace all inactive timestamps in the current buffer with CLOSED and DEADLINE timestamps."
  (interactive)
  (save-excursion
    ;; (goto-char (point-min))
    (while (re-search-forward "\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}.*\\)\\]" nil t)
      (let ((timestamp (match-string 1)))
        (replace-match (format "CLOSED: [%s] SCHEDULED: <%s>" timestamp timestamp))))))

(require 'cus-edit)

(defun my/summarize-customize-group ()
  "Summarize the customization options of a group with their descriptions."
  (interactive)
  (let* ((all-groups (mapcar 'symbol-name (custom-group-members 'custom-group nil)))
         (group-name (completing-read "Select customization group: " all-groups))
         (group-symbol (intern group-name))
         (output-buffer (get-buffer-create "*Customize Group Summary*")))
    (with-current-buffer output-buffer
      (read-only-mode -1)
      (erase-buffer)
      (insert (format "Summary of Customize Group: %s\n\n" group-name))
      (dolist (item (custom-group-members group-symbol nil))
        (let ((symbol (car item))
              (type (cadr item))
              (description "No description available."))
          (cond
           ((custom-variable-p symbol)
            (setq description (or (documentation-property symbol 'variable-documentation)
                                  "No description available.")))
           ((get symbol 'face-documentation)
            (setq description (get symbol 'face-documentation)))
           ((custom-group-p symbol)
            (setq description (or (documentation-property symbol 'group-documentation)
                                  "No description available."))))
          (insert (format "** %s\n\n%s\n\n"
                          symbol
                          description))))
      (read-only-mode 1)
      (goto-char (point-min)))
    (display-buffer output-buffer)))

(setq native-comp-verbose 3)

;; Call the function now to set the faces initially
(my/sync-tab-bar-to-theme)

(when (file-exists-p "~/source/repos/hl-sentence-long-lines")
  (use-package hl-sentence-long-lines
    :load-path "~/source/repos/hl-sentence-long-lines")

  (eval-after-load 'hl-sentence-long-lines
    '(progn
       (define-key global-map (kbd "C-c l") 'hl-sentence-long-lines-transient))))

(when (file-exists-p "~/source/repos/indent-bars")
  (use-package indent-bars
    :load-path "~/source/repos/indent-bars"))

;; (benchmark-run (org-hugo-export-wim-to-md))
;; (benchmark-run (without-gc #'org-hugo-export-wim-to-md))
(defun without-gc (&rest args)
  (let ((gc-cons-threshold most-positive-fixnum))
    (apply args)))

(when (file-exists-p "~/source/repos/dired-compare")
  (use-package dired-compare
    :load-path "~/source/repos/dired-compare"
    :bind
    (:map dired-mode-map
          (("C-c p" . dired-compare)))))

;;
;; helpful
;;

(use-package helpful)

;; Note that the built-in `describe-function' includes both functions
;; and macros. `helpful-function' is functions only, so we provide
;; `helpful-callable' as a drop-in replacement.
(global-set-key (kbd "C-h f") #'helpful-callable)

(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-h x") #'helpful-command)

;;
;; activities
;;

(use-package activities
  :init
  (activities-mode)
  (activities-tabs-mode)
  ;; Prevent `edebug' default bindings from interfering.
  (setq edebug-inhibit-emacs-lisp-mode-bindings t)
  :bind
  (("C-x C-a C-n" . activities-new)
   ("C-x C-a C-d" . activities-define)
   ("C-x C-a C-a" . activities-resume)
   ("C-x C-a C-s" . activities-suspend)
   ("C-x C-a C-k" . activities-discard)
   ("C-x C-a RET" . activities-switch)
   ("C-x C-a b" . activities-switch-buffer)
   ("C-x C-a g" . activities-revert)
   ("C-x C-a l" . activities-list)))

(add-hook 'org-mode-hook 'org-indent-mode)

(setq org-indent-indentation-per-level 3)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)))

(use-package org-ql
  :ensure t)

(require 'org)
(require 'dired)

(defun my-copy-marked-images-to-blog (dir thumb)
  "Copy the marked files in dired buffer to a new directory named TITLE."
  (let* ((target-dir (concat "~/DCIM/content/" dir))
         (copied-files '())) ;; List to accumulate copied files.

    (message "THUMB : %s" thumb)

    ;; Create target directory if it doesn't exist.
    (make-directory target-dir t)

    ;; Copy the thumbnail image.
    (copy-file thumb (concat "~/DCIM/content/" dir ".jpg"))

    ;; Process each marked file.
    (dolist (file my/org-dired-marked-files)
      (let ((target-file (expand-file-name (file-name-nondirectory file) target-dir)))
        (copy-file file target-file)
        (push target-file copied-files)
        (message "Copied: %s to %s" file target-file)))

    ;; After copying, run PictureCrush on all copied files in one shell command.
    (when copied-files
      (let ((command (concat "PictureCrush " (mapconcat 'identity copied-files " "))))
        (async-shell-command command "*convert*")
        (message "Executed PictureCrush on: %s" (string-join copied-files ", "))))))

(defvar my/org-dired-marked-files nil
  "Stores the current dired marked files.")

(defun my/test-finalize ()
  (let ((key (plist-get org-capture-plist :key))
        (desc (plist-get org-capture-plist :description))
        (template (plist-get org-capture-plist :template))
        (thumb (nth (random (length my/org-dired-marked-files)) my/org-dired-marked-files))
        (export-hugo-section nil))

    (when (string-match ":EXPORT_HUGO_SECTION: \\(.*\\)$" template)
      (setq export-hugo-section (match-string 1 template)))

    (prin1 my/org-dired-marked-files)

    (message "Extracted %s : %s" export-hugo-section thumb)

    (if org-note-abort
        (progn
          (message "Template with key %s and description “%s” aborted" key desc))
      (progn
        (message "Template with key %s and description “%s” run successfully" key desc)
        (when (string= desc "Gallery")
          (my-copy-marked-images-to-blog export-hugo-section thumb))))))

(add-hook 'org-capture-after-finalize-hook 'my/test-finalize)

(defun my/org-capture-blog-with-gallery ()
  "Capture gallery triggering gallery image storage."
  (interactive)
  (setq my/org-dired-marked-files (dired-get-marked-files))
  (org-capture nil "g"))

(defun my/org-hugo-new-subtree-post-capture-template ()
  (let* ((date (format-time-string (org-time-stamp-format  :inactive) (org-current-time)))
         (title (read-from-minibuffer "Post Title: "))
         (fname (org-hugo-slug title)))
    (mapconcat #'identity
               `(
                 ,(concat "* DONE Photos " title " " (format-time-string "%Y-%m-%d") " :" (format-time-string "%Y") ":")
                 ":PROPERTIES:"
                 ":EXPORT_FILE_NAME: index"
                 ,(concat ":EXPORT_HUGO_SECTION: blog/%<%Y%m%d%H%M%S>-blog--" fname)
                 ,(concat ":EXPORT_HUGO_LASTMOD: " date)
                 ":EXPORT_HUGO_TYPE: gallery"
                 ,(concat ":EXPORT_HUGO_CUSTOM_FRONT_MATTER+: :thumbnail /blog/%<%Y%m%d%H%M%S>-blog--" fname ".jpg")
                 ":END:"
                 "%?\n\n")
               "\n")))

(use-package uniline)

(flycheck-define-checker proselint
  "A linter for prose."
  :command ("proselint" source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": "
            (id (one-or-more (not (any " "))))
            (message) line-end))
  :modes (gfm-mode
          markdown-mode
          org-mode
          text-mode))

(add-to-list 'flycheck-checkers 'proselint)

(use-package flymake-proselint)

(use-package flymake
  :bind (:map flymake-mode-map
              ("C-c ! l" . flymake-show-buffer-diagnostics)))

(setq global-eldoc-mode 1)

(server-mode 1)

(setq org-reverse-note-order t)

(use-package languagetool
  :ensure t
  :defer t
  :commands (languagetool-check
             languagetool-clear-suggestions
             languagetool-correct-at-point
             languagetool-correct-buffer
             languagetool-set-language
             languagetool-server-mode
             languagetool-server-start
             languagetool-server-stop)
  :config
  (setq languagetool-java-arguments '("-Dfile.encoding=UTF-8"
                                      "-cp" "/usr/share/languagetool:/usr/share/java/languagetool/*")
        languagetool-console-command "org.languagetool.commandline.Main"
        languagetool-server-command "org.languagetool.server.HTTPServer"))

(defun my/org-ql-tags-search-in-current-buffer ()
  "Prompt the user for a tag from the current buffer and generate a TODO list ordered by timestamp."
  (interactive)
  ;; Check if the buffer is in 'org-mode'
  (if (derived-mode-p 'org-mode)
      (let* ((tags (mapcar #'car (org-global-tags-completion-table (list (buffer-file-name)))))
             (chosen-tag (completing-read
                          "Choose a tag from the current buffer: "
                          tags)))
        (org-ql-search
          (current-buffer)
          `(and (tags ,chosen-tag))
          :title (format "tag: %s" chosen-tag)
          :sort 'todo))
    (message "This command must be run in an Org buffer.")))

(bind-key* (kbd "M-s s") #'my/org-ql-tags-search-in-current-buffer)
