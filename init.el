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

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(require 'use-package)
(setq load-prefer-newer t)

;;
;; -> selected-window-accent-mode
;;

(use-package selected-window-accent-mode
  :load-path "~/source/repos/selected-window-accent-mode"
  :config (selected-window-accent-mode 1)
  :custom
  (selected-window-accent-fringe-thickness 10)
  (selected-window-accent-custom-color nil)
  (selected-window-accent-mode-style 'default)
  (selected-window-accent-percentage-darken 20)
  (selected-window-accent-percentage-desaturate 20)
  (selected-window-accent-tab-accent t)
  (selected-window-accent-smart-borders t))

(global-set-key (kbd "C-c w") selected-window-accent-map)

;;
;; -> org-agenda
;;
(setq org-agenda-files '("~/DCIM/content/aaa--calendar.org"
                         "~/DCIM/content/aaa--calendar-repeat.org"
                         "~/DCIM/content/aaa--todo.org"
                         "~/DCIM/content/aab--house.org"
                         "~/DCIM/content/aab--move.org"
                         "~/DCIM/content/aac--emacs-todo.org"))

(setq org-agenda-sticky t)

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
(use-package async)
(use-package org-wc)
(use-package git-timemachine)
(use-package consult)
(use-package i3wm-config-mode)
(use-package yaml-mode)

(use-package ox-hugo
  :defer t
  :config
  (setq org-hugo-front-matter-format "yaml"
        org-hugo-base-dir "~/DCIM"))

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
              (find-file (concat user-emacs-directory "README.org"))))
(define-key my-jump-keymap (kbd "a")
            (lambda () (interactive)
              (find-file "~/DCIM/content/emacs--all.org")))

;;
;; -> completion
;;

(use-package eglot
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
(setq icomplete-in-buffer nil)
;;
(use-package corfu
  :init
  (global-corfu-mode 1)
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

;;
;; -> keys-visual
;;
(define-key my-win-keymap (kbd "m") #'consult-theme)
(define-key my-win-keymap (kbd "w") #'org-wc-display)

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
  (setq-local completion-styles '(basic partial-completion)))
(add-hook 'eshell-mode-hook 'my/eshell-hook)

;;
;; -> linux specific
;;

(when (eq system-type 'gnu/linux)
  (define-key my-jump-keymap (kbd "m") (lambda () (interactive) (find-file "~/DCIM/Camera")))
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
(defvar my-ollama-host "localhost:11434"
  "Host for the GPT backend.")

;; Define a list of models and token sizes
(defvar my-llm-models
  '(("qwen2.5-coder" . "7b")
    ("deepseek-r1" . "7b"))
  "List of LLM models and their token sizes to configure.")

(use-package shell-maker
  :ensure t)
(use-package chatgpt-shell
  :ensure t
  :after shell-maker
  :custom
  (chatgpt-shell-openai-key
   (lambda ()
     (auth-source-pass-get 'secret "openai-key")))
  ;; Dynamically configure chatgpt-shell models using the my-llm-models list
  (chatgpt-shell-models
   (let ((default-models
          '(
            ;; OpenAI example model pre-configured
            ((:version . "chatgpt-4o-latest")
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
             (:validate-command . chatgpt-shell-openai--validate-command))))
         (ollama-models
          (mapcar
           (lambda (model-token-pair)
             (let* ((model-name (car model-token-pair))
                    (token-size (cdr model-token-pair))
                    (model-version (format "%s:%s" model-name token-size))) ;; Full name
               `((:provider . "Ollama")
                 (:label . ,model-name)
                 (:version . ,model-version)
                 (:short-version . ,token-size)
                 (:token-width . 4) ;; Customize as needed
                 (:context-window . 8192) ;; Adjust if needed
                 (:handler . chatgpt-shell-ollama--handle-ollama-command)
                 (:filter . chatgpt-shell-ollama--extract-ollama-response)
                 (:payload . chatgpt-shell-ollama-make-payload)
                 (:url . chatgpt-shell-ollama--make-url))))
           my-llm-models)))
     (append default-models ollama-models))))

(use-package gptel
  :config
  (dolist (model-token-pair my-llm-models)
    (let* ((model-name (car model-token-pair))
           (token-size (cdr model-token-pair))
           (full-model-name (format "%s:%s" model-name token-size))
           (ollama-backend (gptel-make-ollama model-name
                             :host my-ollama-host
                             :stream t
                             :models `(,(intern full-model-name)))))
      (set (intern (format "gptel-backend-%s-%s" model-name token-size)) ollama-backend)
      (message "Configured Ollama backend for model: %s" full-model-name)))
  (let* ((default-model (car my-llm-models))
         (default-model-name (car default-model))
         (default-token-size (cdr default-model))
         (default-full-model (format "%s:%s" default-model-name default-token-size)))
    (setq gptel-model (intern default-full-model)
          gptel-backend (gptel-make-ollama default-model-name
                          :host my-ollama-host
                          :stream t
                          :models `(,(intern default-full-model))))))

(defun my/llm-shell-menu ()
  "Menu for ChatGPT Shell commands."
  (interactive)
  (let ((key (read-key
              (propertize
               "----- ChatGPT Shell Commands [q] Quit: -----
Model  [o] Start ChatGPT    [m] Swap Model
Check  [p] Proofread Region [r] Refactor Code
Ollama [l] Start Ollama     [n] Menu
       [k] Kill Request"
               'face 'minibuffer-prompt))))
    (pcase key
      (?o (call-interactively 'chatgpt-shell))
      (?m (call-interactively 'chatgpt-shell-swap-model))
      (?p (call-interactively 'chatgpt-shell-proofread-region))
      (?r (call-interactively 'chatgpt-shell-refactor-code))
      (?l (call-interactively 'gptel))
      (?n (call-interactively 'gptel-menu))
      (?k (call-interactively 'gptel-abort))
      (?q (message "Quit ChatGPT Shell menu."))
      (?\C-g (message "Quit ChatGPT Shell menu."))
      (_ (message "Invalid key: %c" key)))))

(global-set-key (kbd "C-c g") #'my/llm-shell-menu)

;;
;; -> programming
;;
(setq my/old-ada-mode (concat user-emacs-directory "old-ada-mode"))
(when (file-exists-p my/old-ada-mode)
  (use-package ada-mode
    :load-path my/old-ada-mode))

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
;; -> auto-mode-alist
;;
(add-to-list 'auto-mode-alist '("waybar.*/config\\'" . js-json-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.org_archive\\'" . org-mode))
(add-to-list 'auto-mode-alist '("/sway/.*config.*/" . i3wm-config-mode))
(add-to-list 'auto-mode-alist '("/sway/config\\'" . i3wm-config-mode))
(cl-loop for ext in '("\\.gpr$" "\\.ada$" "\\.ads$" "\\.adb$")
         do (add-to-list 'auto-mode-alist (cons ext 'ada-mode)))

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
     "https://www.emacs.dyerdwelling.family/index.xml"
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

;;
;; -> dired
;;
(require 'dired-async)
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C") 'dired-do-copy))
(dired-async-mode 1)

;;
;; -> spelling
;;
(use-package powerthesaurus)

(defun spelling-menu ()
  "Menu for spelling."
  (interactive)
  (let ((key (read-key
              (propertize
               "------- Spelling [q] Quit: -------
Run        [s] Spelling
Lookup     [d] Lookup
Reference  [t] Thesaurus
Dictionary [l] Check"
               'face 'minibuffer-prompt))))
    (pcase key
      ;; Spelling
      (?s (progn
            (flyspell-buffer)
            (call-interactively 'flyspell-mode)))
      ;; Lookup
      (?l (call-interactively 'ispell-word))
      ;; Reference
      (?t (call-interactively 'powerthesaurus-lookup-synonyms-dwim))
      ;; Dictionary
      (?d (call-interactively 'dictionary-lookup-definition))
      ;; Quit
      (?q (message "Quit Build menu."))
      (?\C-g (message "Quit Build menu."))
      ;; Default Invalid Key
      (_ (message "Invalid key: %c" key)))))

(global-set-key (kbd "C-c s") #'spelling-menu)
(global-set-key (kbd "C-9") #'powerthesaurus-lookup-synonyms-dwim)

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
;; -> development
;;
(defun export-menu ()
  "Menu for Export/Publishing commands."
  (interactive)
  (let ((key (read-key
              (propertize
               "--- Export Commands [q] Quit: ---
[h] Export to Hugo (with rsync)
[w] Export to HTML (with table highlighting)
[d] Export to DOCX (via ODT)"
               'face 'minibuffer-prompt))))
    (pcase key
      (?h (save-excursion
            (without-gc #'org-hugo-export-wim-to-md)
            (mapc 'shell-command
                  '("web rsync emacs" "web rsync art"
                    "web rsync dyerdwelling"))))
      (?w (progn
            (org-html-export-to-html)
            (my/html-promote-headers)
            (my/html-org-table-highlight)))
      (?d (progn
            (org-odt-export-to-odt)
            (async-shell-command
             (concat "libreoffice --headless --convert-to docx "
                     (file-name-with-extension
                      (file-name-nondirectory (buffer-file-name))
                      "odt")) "*create-docs*")))
      ;; Quit
      (?q (message "Quit Export menu."))
      (?\C-g (message "Quit Export menu."))
      ;; Default Invalid Key
      (_ (message "Invalid key: %c" key)))))

;; Bind the menu to C-c e
(global-set-key (kbd "C-c e") 'export-menu)

(my/sync-ui-accent-color "coral")

;;
;; -> ollama-buddy
;;
(setq ollama-buddy-command-definitions
      '(
        ;; General Commands
        (open-chat
         :key ?o
         :description "Open chat buffer"
         :action ollama-buddy--open-chat)
        
        (show-models
         :key ?v
         :description "View model status"
         :action ollama-buddy-show-model-status)
        
        (swap-model
         :key ?m
         :description "Swap model"
         :action ollama-buddy--swap-model)
        
        (help
         :key ?h
         :description "Help assistant"
         :action (lambda ()
                   (pop-to-buffer (get-buffer-create ollama-buddy--chat-buffer))
                   (goto-char (point-max))
                   (insert (ollama-buddy--create-intro-message))
                   (ollama-buddy--show-prompt)))
        
        (send-region
         :key ?l
         :description "Send region"
         :action (lambda () (ollama-buddy--send-with-command 'send-region)))

        (switch-role
         :key ?R
         :description "Switch roles"
         :model nil
         :action ollama-buddy-roles-switch-role)
        
        (create-role
         :key ?N
         :description
         "Create new role"
         :model nil
         :action ollama-buddy-role-creator-create-new-role)
        
        (open-roles-directory
         :key ?D
         :description "Open roles directory"
         :model nil
         :action ollama-buddy-roles-open-directory)
        
        ;; Specialized commands
        (refactor-code
         :key ?r
         :description "Refactor code"
         :model "qwen2.5-coder:7b"
         :prompt "refactor the following code:"
         :action (lambda () (ollama-buddy--send-with-command 'refactor-code)))
        
        (git-commit
         :key ?g
         :description "Git commit message"
         :model "qwen2.5-coder:3b"
         :prompt "write a concise git commit message for the following:"
         :action (lambda () (ollama-buddy--send-with-command 'git-commit)))
        
        (describe-code
         :key ?c
         :description "Describe code"
         :model "qwen2.5-coder:3b"
         :prompt "describe the following code:"
         :action (lambda () (ollama-buddy--send-with-command 'describe-code)))
        
        (dictionary-lookup
         :key ?d
         :description "Dictionary Lookup"
         :model "llama3.2:1b"
         :prompt "For the following word provide a typical dictionary definition:"
         :action (lambda () (ollama-buddy--send-with-command 'dictionary-lookup)))
        
        (synonym
         :key ?n
         :description "Word synonym"
         :model "llama3.2:1b"
         :prompt "list synonyms for word:"
         :action (lambda () (ollama-buddy--send-with-command 'synonym)))
        
        (proofread
         :key ?p
         :description "Proofread text"
         :model "deepseek-r1:7b"
         :prompt "proofread the following:"
         :action (lambda () (ollama-buddy--send-with-command 'proofread)))
        
        (make-concise
         :key ?z
         :description "Make concise"
         :prompt "reduce wordiness while preserving meaning:"
         :action (lambda () (ollama-buddy--send-with-command 'make-concise)))
        
        ;; System Commands
        (custom-prompt
         :key ?e
         :description "Custom prompt"
         :model "deepseek-r1:7b"
         :action (lambda ()
                   (when-let ((prefix (read-string "Enter prompt prefix: " nil nil nil t)))
                     (unless (use-region-p)
                       (user-error "No region selected. Select text to use with prompt"))
                     (unless (not (string-empty-p prefix))
                       (user-error "Input string is empty"))
                     (ollama-buddy--send
                      (concat prefix "\n\n"
                              (buffer-substring-no-properties 
                               (region-beginning) (region-end)))))))
        (minibuffer-prompt
         :key ?i
         :description "Minibuffer Prompt"
         :action (lambda ()
                   (when-let ((prefix (read-string "Enter prompt: " nil nil nil t)))
                     (unless (not (string-empty-p prefix))
                       (user-error "Input string is empty"))
                     (ollama-buddy--send prefix))))
        (save-chat
         :key ?s
         :description "Save chat"
         :action (lambda ()
                   (with-current-buffer ollama-buddy--chat-buffer
                     (write-region (point-min) (point-max)
                                   (read-file-name "Save conversation to: ")
                                   'append-to-file
                                   nil))))
        (kill-request
         :key ?x
         :description "Kill request"
         :action (lambda ()
                   (delete-process ollama-buddy--active-process)))

        (toggle-colors
         :key ?C
         :description "Toggle Colors"
         :action ollama-buddy-toggle-model-colors)

        (token-stats
         :key ?t
         :description "Token Usage Stats"
         :action ollama-buddy-display-token-stats)

        (quit
         :key ?q
         :description "Quit"
         :action (lambda () (message "Quit Ollama Shell menu.")))
        )
      )

(use-package ollama-buddy
  :load-path "~/source/repos/ollama-buddy/ollama-buddy-mini"
  ;; :load-path "~/source/repos/ollama-buddy"
  :bind ("C-c o" . ollama-buddy-menu)
  :custom ollama-buddy-default-model "llama3.2:1b")

;;
;; -> emacs-30.1
;;
(setq tab-bar-auto-width-max '((120) 20))

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
