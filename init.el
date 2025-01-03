;; -*- lexical-binding: t; -*-

;;
;; -> core-configuration
;;
(load-file "~/.emacs.d/Emacs-core/init.el")

;;
;; -> package-archives
;;

(require 'package)

(when (eq system-type 'gnu/linux)
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("elpa" . "https://elpa.gnu.org/packages/")
                           ("org" . "https://orgmode.org/elpa/"))))

(when (eq system-type 'windows-nt)
  (setq package-archives '(("melpa" . "~/emacs-pkgs/melpa")
                           ("elpa" . "~/emacs-pkgs/elpa")
                           ("org" . "~/emacs-pkgs/org-mode/lisp"))))

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
;; -> org-agenda
;;
(setq org-agenda-files '("~/DCIM/content/aaa--aaa.org"
                         "~/DCIM/content/aaa--calendar.org"
                         "~/DCIM/content/aaa--todo.org"
                         "~/DCIM/content/aab--house.org"
                         "~/DCIM/content/aac--baby.org"
                         "~/DCIM/content/aag--emacs-todo.org"))

;;
;; -> org-capture
;;
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

(defun my/capture-finalize ()
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

(add-hook 'org-capture-after-finalize-hook 'my/capture-finalize)

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

;;
;; -> use-package
;;
(use-package org-wc)
(use-package git-timemachine)
(use-package consult)

(use-package ox-hugo
  :defer t
  :config
  (setq org-hugo-front-matter-format "yaml"))

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

(use-package org-superstar
  :hook
  (org-mode . org-superstar-mode))

;;
;; -> keys-navigation
;;

(define-key my-jump-keymap (kbd "k")
            (lambda () (interactive)
              (find-file (concat user-emacs-directory "emacs--init.org"))))

;;
;; -> completion
;;

(use-package cape)

(use-package capf-autosuggest)

(use-package corfu
  :init (global-corfu-mode)
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

;;
;; -> keys-visual
;;
(define-key my-win-keymap (kbd "a") #'selected-window-accent-mode)
(define-key my-win-keymap (kbd "m") #'consult-theme)
(define-key my-win-keymap (kbd "w") #'org-wc-display)

;;
;; -> keys-other
;;
(global-set-key (kbd "M-s e") #'(lambda ()
                                  (interactive)
                                  (save-excursion
                                    (without-gc #'org-hugo-export-wim-to-md)
                                    (mapc 'shell-command
                                          '("web rsync emacs" "web rsync art"
                                            "web rsync dyerdwelling")))))

;;
;; -> defun
;;
(defun my/sync-tab-bar-to-theme ()
  "Synchronize tab-bar faces with the current theme."
  (interactive)
  (let ((default-bg (face-background 'default))
        (default-fg (face-foreground 'default))
        (inactive-fg (face-foreground 'mode-line-inactive))) ;; Fallback to mode-line-inactive
    (custom-set-faces
     `(tab-bar ((t (:inherit default :font "Monospace 12" :background ,default-bg :foreground ,default-fg))))
     `(tab-bar-tab ((t (:inherit default :background ,default-fg :foreground ,default-bg))))
     `(tab-bar-tab-inactive ((t (:inherit default :background ,default-bg :foreground ,inactive-fg)))))))
(my/sync-tab-bar-to-theme)

;;
;; -> org
;;
(setq org-hugo-base-dir "~/DCIM")
(setq org-todo-keywords
      '((sequence "TODO" "DOING" "ORDR" "SENT" "|" "DONE" "CANCELLED")))
(setq org-todo-keyword-faces
      '(("TODO" . "#ee5566")
        ("DOING" . "#5577aa")
        ("ORDR" . "#bb44ee")
        ("SENT" . "#bb44ee")
        ("DONE" . "#77aa66")
        ("CANCELLED" . "#426b3e")))

;;
;; -> visuals
;;
(set-frame-parameter nil 'alpha-background 90)
(add-to-list 'default-frame-alist '(alpha-background . 90))

;;
;; -> shell
;;
(defun my/eshell-hook ()
  "Set up completions to be a little more fish like."
  (interactive)
  (setq-local completion-styles '(basic partial-completion))
  (capf-autosuggest-mode)
  (setq-local completion-at-point-functions
              (list (cape-capf-super
                     #'pcomplete-completions-at-point
                     #'cape-history))))
(add-hook 'eshell-mode-hook 'my/eshell-hook)

;;
;; -> linux specific
;;

(when (eq system-type 'gnu/linux)
  (define-key my-jump-keymap (kbd "c") (lambda () (interactive) (find-file "~/DCIM/Camera")))
  (define-key my-jump-keymap (kbd "j") (lambda () (interactive) (find-file "~/DCIM/content/aaa--todo.org")))
  (define-key my-jump-keymap (kbd "n") (lambda () (interactive) (find-file "~/DCIM/Screenshots")))
  (define-key my-jump-keymap (kbd "w") (lambda () (interactive) (find-file "~/DCIM/content/")))
  ;; (setq font-general "Noto Sans Mono 11")
  (setq font-general "Source Code Pro 12")
  ;; (setq font-general "Source Code Pro Light 11")
  ;; (setq font-general "Monospace 11")
  ;;(setq font-general "Nimbus Mono PS 13")
  (set-frame-font font-general nil t)
  (add-to-list 'default-frame-alist `(font . ,font-general))
  (setq diary-file "~/DCIM/content/diary.org"))

;;
;; -> LLM
;;
(use-package chatgpt-shell
  :custom
  (chatgpt-shell-openai-key
   (lambda ()
     (auth-source-pass-get 'secret "openai-key")))
  (chatgpt-shell-models
   '(((:version . "chatgpt-4o-latest")
      (:short-version)
      (:label . "ChatGPT")
      (:provider . "OpenAI")
      (:path . "/v1/chat/completions")
      (:token-width . 3)
      (:context-window . 12800)
      (:handler . chatgpt-shell-openai--handle-chatgpt-command)
      (:filter . chatgpt-shell-openai--filter-output)
      (:payload . chatgpt-shell-openai--make-payload)
      (:headers . chatgpt-shell-openai--make-headers)
      (:url . chatgpt-shell-openai--make-url)
      (:key . chatgpt-shell-openai-key)
      (:url-base . chatgpt-shell-api-url-base)
      (:validate-command . chatgpt-shell-openai--validate-command))
     ((:provider . "Ollama")
      (:label . "Ollama-qwen")
      (:version . "qwen2.5-coder-7b-instruct-q5_k_m")
      (:short-version)
      (:token-width . 4)
      (:context-window . 8192)
      (:handler . chatgpt-shell-ollama--handle-ollama-command)
      (:filter . chatgpt-shell-ollama--extract-ollama-response)
      (:payload . chatgpt-shell-ollama-make-payload)
      (:url . chatgpt-shell-ollama--make-url))
     ((:provider . "Ollama")
      (:label . "Ollama-llama")
      (:version . "Llama-3.2-1B-Instruct-Q8_0")
      (:short-version)
      (:token-width . 4)
      (:context-window . 8192)
      (:handler . chatgpt-shell-ollama--handle-ollama-command)
      (:filter . chatgpt-shell-ollama--extract-ollama-response)
      (:payload . chatgpt-shell-ollama-make-payload)
      (:url . chatgpt-shell-ollama--make-url)))))

;;
;; -> programming
;;

(setq my/old-ada-mode (concat user-emacs-directory "old-ada-mode"))
(when (file-exists-p my/old-ada-mode)
  (use-package ada-mode
    :load-path my/old-ada-mode))

(use-package yaml-mode)

;;
;; -> themes
;;
(use-package doom-themes)
(use-package ef-themes)
(use-package gruvbox-theme)

;;
;; -> modes
;;
(server-mode 1)

;;
;; -> icons
;;
(use-package all-the-icons-dired
  :hook
  (dired-mode . all-the-icons-dired-mode))
(use-package all-the-icons-ibuffer
  :hook
  (ibuffer-mode . all-the-icons-ibuffer-mode))

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
   '(
     "https://www.emacs.dyerdwelling.family/public_html/feed.xml"
     )))
(defun my/show-elfeed (buffer)
  "Show Elfeed wrapper with BUFFER."
  (display-buffer buffer))

(setq elfeed-show-mode-hook
      (lambda ()
        (set-face-attribute 'variable-pitch (selected-frame)
                            :font (font-spec :family "Source Code Pro" :size 16))
        (setq elfeed-show-entry-switch #'my/show-elfeed)))
