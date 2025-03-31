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
  ;; :load-path "~/source/repos/selected-window-accent-mode"
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
                         "~/DCIM/content/aab--calendar-repeat.org"
                         "~/DCIM/content/aaa--todo.org"
                         "~/DCIM/content/aab--move.org"
                         "~/DCIM/content/aab--sell.org"
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
(set-frame-parameter nil 'alpha-background 80)
(add-to-list 'default-frame-alist '(alpha-background . 80))

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
 '(custom-enabled-themes '(doom-monokai-classic))
 '(warning-suppress-log-types '((frameset)))
 '(warning-suppress-types '((frameset))))

;;
;; -> ollama-buddy
;;
(use-package ollama-buddy
  ;; :load-path "~/source/repos/ollama-buddy/ollama-buddy-mini"
  :load-path "~/source/repos/ollama-buddy"
  :bind
  ("C-c o" . ollama-buddy-menu)
  ("C-c O" . ollama-buddy-transient-menu-wrapper)
  :custom
  (ollama-buddy-openai-api-key
   (auth-source-pick-first-password :host "ollama-buddy-openai" :user "apikey"))
  (ollama-buddy-default-model "GPT gpt-4o")
  (ollama-buddy-claude-api-key
   (auth-source-pick-first-password :host "ollama-buddy-claude" :user "apikey"))
  (ollama-buddy-claude-default-model "claude-3-sonnet-20240229")
  :config
  (require 'ollama-buddy-openai nil t)
  (require 'ollama-buddy-claude nil t)
  (ollama-buddy-update-menu-entry
   'git-commit :model "GPT gpt-4o")
  (ollama-buddy-update-menu-entry
   'describe-code :model "qwen2.5-coder:3b")
  (ollama-buddy-update-menu-entry
   'dictionary-lookup :model "llama3.2:3b")
  (ollama-buddy-update-menu-entry
   'synonym :model "llama3.2:3b")
  (ollama-buddy-update-menu-entry
   'proofread :model "GPT gpt-4o"))

;;
;; -> emacs-30.1
;;
(setq tab-bar-auto-width-max '((120) 20))

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

(use-package csv-mode)

;;
;; -> finance
;;

(use-package csv)
(require 'csv)

(defvar payments '())
(defvar cat-tot (make-hash-table :test 'equal))

(setq cat-list-defines
      '(("katherine\\|lucinda\\|kate" "kat")
        ("railw\\|railway\\|selfserve\\|train" "trn")
        ("paypal" "pay") ("virgin-media\\|uinsure\\|insurance\\|royal-mail\\|postoffice\\|endsleigh\\|waste\\|lloyds\\|electric\\|sse\\|newsstand\\|privilege\\|pcc\\|licence\\|ovo\\|energy\\|bt\\|water" "utl")
        ("sky-betting\\|b365\\|races\\|bet365\\|racing" "bet")
        ("stakeholde\\|widows" "pen")
        ("nsibill\\|vines\\|ns&i\\|saver" "sav")
        ("uber\\|aqua" "txi")
        ("magazine\\|specs\\|zinio\\|specsavers\\|publishing\\|anthem\\|kindle\\|news" "rdg") ("claude\\|escape\\|deviant\\|cleverbridge\\|reddit\\|pixel\\|boox\\|ionos\\|microsoft\\|mobile\\|backmarket\\|cartridge\\|whsmith\\|dazn\\|my-picture\\|openai\\|c-date\\|ptitis\\|keypmt\\|billnt\\|fee2nor\\|assistance\\|boxise\\|billkt\\|paintstor\\|iet-main\\|ffnhelp\\|shadesgrey\\|venntro\\|vtsup\\|sunpts\\|apyse\\|palchrge\\|maypmt\\|filemodedesk\\|istebrak\\|connective\\|avangate\\|stardock\\|avg\\|123\\|web\\|a2" "web")
        ("notemachine\\|anchrg\\|hilsea\\|withdrawal" "atm")
        ("finance" "fin")
        ("youtube\\|entertai\\|twitch\\|disney\\|box-office\\|discovery\\|tvplayer\\|vue\\|sky\\|netflix\\|audible\\|nowtv\\|channel\\|prime" "str")
        ("platinum\\|card" "crd")
        ("top-up\\|three\\|h3g" "phn")
        ("amaz\\|amz" "amz")        
        ("pets\\|pet" "pet")
        ("mydentist\\|dentist" "dnt")
        ("natwest-bank-reference\\|residential\\|rent\\|yeong" "hse")
        ("mardin\\|starbuck\\|gillett-copnor\\|asda\\|morrison\\|sainsburys\\|waitrose\\|tesco\\|domino\\|deliveroo\\|just.*eat" "fod") ("retail-ltd\\|vinted\\|lockart\\|moment-house\\|yuyu\\|bushra\\|newhome\\|white-barn\\|skinnydip\\|mgs\\|river-island\\|spencer\\|lilian\\|jung\\|ikea\\|wayfair\\|neom\\|teespring\\|lick-home\\|matalan\\|devon-wick\\|united-arts\\|lush-retail\\|lisa-angel\\|sharkninja\\|fastspring\\|bonas\\|asos\\|emma\\|sofology\\|ebay\\|dunelm\\|coconut\\|semantical\\|truffle\\|nextltd\\|highland\\|little-crafts\\|papier\\|the-hut\\|new-look\\|samsung\\|astrid\\|pandora\\|waterstone\\|cultbeauty\\|24pymt\\|champo\\|costa\\|gollo\\|pumpkin\\|argos\\|the-range\\|biffa\\|moonpig\\|apple\\|itunes\\|gold\\|interflora\\|thortful" "shp")
        ("js-law" "law")
        ("anyvan" "hmv")
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
    (write-header-plot 2025)
    (write-header)
    (let ((index 0))
      (dolist (year (seq-map '(lambda (value)
                                (format "%02d" value))
                             (nreverse (number-sequence 2016 2025 1))))
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
                         (nreverse (number-sequence 2016 2025 1))))
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
                               (nreverse (number-sequence 2016 2025 1))))
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

(use-package gnuplot)

(add-to-list 'display-buffer-alist
             '("\\*Ollama Buddy Chat" display-buffer-same-window))
