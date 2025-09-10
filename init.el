;; -*- lexical-binding: t; -*-

;;
;; -> core-configuration
;;
(load-file "~/.emacs.d/Emacs-vanilla/init.el")

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
  (selected-window-accent-use-pywal t)
  (selected-window-accent-smart-borders nil))

;;
;; -> org-agenda
;;
(setq org-agenda-files '(
                         "~/DCIM/content/aaa--calendar.org"
                         "~/DCIM/content/aab--repeat.org"
                         "~/DCIM/content/aaa--todo.org"
                         "~/DCIM/content/aab--move.org"
                         "~/DCIM/content/aab--sell.org"
                         "~/DCIM/content/aac--emacs-todo.org"
                         "~/DCIM/content/aaa--calendar.org"
                         ))

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

(defun my/org-hugo-new-subtree-post-capture-template ()
  (let* ((default-date (format-time-string "%Y-%m-%d" (current-time)))
         (input-date (read-string (format "Date (%s): " default-date) nil nil default-date))
         (parsed-date (org-read-date nil t input-date))
         (date (format-time-string (org-time-stamp-format :inactive) parsed-date))
         (date-string (format-time-string "%Y-%m-%d" parsed-date))
         (year-string (format-time-string "%Y" parsed-date))
         (timestamp-string (format-time-string "%Y%m%d%H%M%S" parsed-date))
         (title (read-from-minibuffer "Post Title: "))
         (fname (org-hugo-slug title)))
    (mapconcat #'identity
               `(
                 ,(concat "* DONE Photos " title " " date-string " :" year-string ":")
                 ":PROPERTIES:"
                 ":EXPORT_FILE_NAME: index"
                 ,(concat ":EXPORT_HUGO_SECTION: blog/" timestamp-string "-blog--" fname)
                 ,(concat ":EXPORT_HUGO_LASTMOD: " date)
                 ":EXPORT_HUGO_TYPE: gallery"
                 ,(concat ":EXPORT_HUGO_CUSTOM_FRONT_MATTER+: :thumbnail /blog/" timestamp-string "-blog--" fname ".jpg")
                 ":END:"
                 "%?\n")
               "\n")))

(add-to-list 'org-capture-templates
             '("g" "Gallery" plain
               (file+function
                "~/DCIM/content/blog--all.org"
                my-capture-top-level)
               (function my/org-hugo-new-subtree-post-capture-template)
               :prepend t :jump-to-captured t))

(defun my/external-org-capture-blog-with-gallery (files-string)
  "Capture gallery triggering gallery image storage."
  (interactive)
  (setq my/org-dired-marked-files (split-string files-string ";" t))
  (org-capture nil "g"))

(defun my/create-gallery ()
  "Tag pictures from any context (dired or image-dired)."
  (interactive)
  (let ((files (my/get-files-from-context)))
    (if files
        (let ((files-string (mapconcat 'identity files ";")))
          (my/external-org-capture-blog-with-gallery files-string))
      (message "No files found to tag"))))

;;
;; -> use-package
;;
(use-package flycheck)
(use-package async)
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
;; -> visuals
;;
(set-frame-parameter nil 'alpha-background 85)
(add-to-list 'default-frame-alist '(alpha-background . 85))

;;
;; -> linux specific
;;

(when (eq system-type 'gnu/linux)
  (define-key my-jump-keymap (kbd "m") (lambda () (interactive) (find-file "~/DCIM/Camera")))
  (define-key my-jump-keymap (kbd "j") (lambda () (interactive) (find-file "~/DCIM/content/aaa--todo.org")))
  (define-key my-jump-keymap (kbd "n") (lambda () (interactive) (find-file "~/DCIM/Screenshots")))
  (define-key my-jump-keymap (kbd "w") (lambda () (interactive) (find-file "~/DCIM/content/")))
  ;; (setq font-general "Noto Sans Mono 11")
  (setq font-general "Source Code Pro 10")
  ;; (setq font-general "Source Code Pro Light 11")
  ;; (setq font-general "Monospace 11")
  ;;(setq font-general "Nimbus Mono PS 13")
  (set-frame-font font-general nil t)
  (add-to-list 'default-frame-alist `(font . ,font-general))
  (setq diary-file "~/DCIM/content/diary.org"))

;;
;; -> themes
;;
(use-package doom-themes)
(use-package ef-themes)
(use-package gruvbox-theme)

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
;; -> dired
;;
(require 'dired-async)
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C") 'dired-do-copy))
(dired-async-mode 1)

;;
;; -> custom-settings
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes '(doom-oceanic-next))
 '(tab-bar-mode t)
 '(tool-bar-mode nil)
 '(warning-suppress-log-types '((frameset)))
 '(warning-suppress-types '((frameset))))

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
            (my/html-org-table-highlight)
            (my/html-flush-divs)))
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
(use-package package-lint)

(use-package bank-buddy
  :load-path "~/source/repos/bank-buddy"
  :custom
  (bank-buddy-core-top-spending-categories 20)
  (bank-buddy-core-top-merchants 20)
  (bank-buddy-core-large-txn-threshold 1200)
  (bank-buddy-core-monthly-spending-bar-width 160)
  (bank-buddy-core-monthly-spending-max-bar-categories 20)
  (bank-buddy-core-cat-list-defines
   '(("katherine\\|james\\|kate" "prs") ("carpet" "hse") ("railw\\|railway\\|train" "trn") ("paypal" "pay") ("electric\\|energy\\|water" "utl") ("racing" "bet") ("pension" "pen") ("savings\\|saver" "sav") ("uber" "txi") ("magazine\\|news" "rdg") ("claude\\|reddit\\|mobile\\|backmarket\\|openai\\|web" "web") ("notemachine\\|withdrawal" "atm") ("finance" "fin") ("youtube\\|netflix" "str") ("card" "crd") ("top-up\\|phone" "phn") ("amaz\\|amz" "amz") ("pets\\|pet" "pet") ("dentist" "dnt") ("residential\\|rent\\|mortgage" "hse") ("deliveroo\\|just.*eat" "fod") ("ebay\\|apple\\|itunes" "shp") ("law" "law") ("anyvan" "hmv") ("CHANNEL-4" "str") ("GOOGLE-\\*Google-Play" "web") ("NOW-" "str") ("SALISBURY-CAFE-LOCAL" "fod") ("SAVE-THE-PENNIES" "sav") ("SOUTHAMPTON-GENERAL" "fod") ("TO-Evie" "sav") ("WH-Smith-Princess-Anne" "fod") ("SP-WAXMELTSBYNIC" "shp") ("WWW\\.SSE" "utl") ("THORTFUL" "shp") ("SCOTTISH-WIDOWS" "pen") ("WM-MORRISONS" "fod") ("H3G-REFERENCE" "phn") ("DOMINO" "fod") ("Prime-Video" "str") ("PRIVILEGE" "utl") ("PCC-COLLECTION" "utl") ("MORRISON" "fod") ("BT-GROUP" "web") ("ANTHROPIC" "web") ("INSURE" "utl") ("GOOGLE-Google-Play" "web") ("GILLETT-COPNOR-RD" "fod") ("TV-LICENCE" "utl") ("SAINSBURYS" "fod") ("TESCO" "shp") ("Vinted" "shp") ("PUMPKIN-CAFE" "fod") ("SP-CHAMPO" "shp") ("THE-RANGE" "shp") ("UNIVERSITY-HOSPITA" "fod") ("VIRGIN-MEDIA" "utl") ("GOLDBOUTIQUE" "shp") ("Surveyors" "law") ("Surveyors" "hse") ("INTERFLORA" "shp") ("INSURANCE" "utl") ("LUCINDA-ELLERY" "shp") ("MARKS&SPENCER" "fod") ("SW-PLC-STAKEHOLDE" "pen") ("JUST-MOVE" "hse") ("B&M" "shp") ("PASSPORT-OFFICE" "hse") ("PHARMACY" "shp") ("ONLINE-REDIRECTIONS" "hse") ("SERENATA-FLOWERS" "shp") ("SNAPPER-DESIGN" "shp") ("LOVEFORSLEEP" "shp") ("TJ-WASTE" "hse") ("M-&-S" "fod") ("MARDIN" "fod") ("MOVEWITHUS" "hse") ("STARBUCKS" "fod") ("CD-2515" "shp") ("DEBIT-INTEREST-ARRANGED" "atm") ("ME-GROUP-INTERNATIONAL" "shp") ("COSTA" "fod") ("NYX" "shp") ("NATWEST-BANK-REFERENCE" "hse") ("Streamline" "shp") ("BETHANIE-YEONG" "hse") ("Roofoods" "fod") ("Wayfair" "shp") ("WHSmith" "shp") ("The-Hut" "shp") ("Sky-Betting" "bet") ("NextLtd" "shp") ("NEW-LOOK-RETAILERS" "shp") ("Marks-and-Spencer" "fod") ("DisneyPlus" "str") ("DAZN-LIMITED" "str") ("Astrid-&-Miyu" "shp") ("ASOS\\.COM-Ltd" "shp") ("Cartridge-Tech-Ltd" "shp") ("Dplay-Entertainment-Ltd" "str") ("DeviantArt" "web") ("Dunelm" "shp") ("Asda-Stores" "shp") ("Argos" "shp") ("IKEA-Limited" "shp") ("Lisa-Angel-Limited" "shp") ("Matalan-Retail-Ltd" "shp") ("Royal-Mail-Group-Limited" "utl") ("SCHOTT-PACKAGING" "hse") ("Samsung-Electronics" "shp") ("Boohoo\\.com" "shp") ("Bizzy-Balloons-LLP" "shp") ("BRANDS-IN-BLOOM-LTD" "shp") ("Highland-and-Honey" "shp") ("Homebaked-Limited" "shp") ("Little-Crafts-London-LTD" "shp") ("Lush-Retail-Ltd" "shp") ("Mamas-&-Papas" "shp") ("Mi-Baby" "shp") ("NEOM-Ltd" "shp") ("Oliver-Bonas-Limited" "shp") ("Pandora-Jewellery-UK-Ltd" "shp") ("Papier" "shp") ("Peggy's-Difference" "shp") ("PlanetArt-Ltd" "shp") ("Pretty-Pastels" "shp") ("Royal-Mail-Group-Ltd" "hse") ("SAINSBURY" "fod") ("Sofology" "shp") ("Sostrene-Grenes" "shp") ("Their-Nibs" "shp") ("melodymaison" "shp") ("AO-Retail-Ltd" "shp") ("Abbott-Lyon" "shp") ("Bellaboo" "shp") ("Devon-wick-Candle-Co\\.-Ltd" "shp") ("Hugo-&-Me-Ltd" "shp") ("Lick-Home-Ltd" "shp") ("Mabel-&-Fox" "shp") ("THE-KID-COLLECTIVE-LTD" "shp") ("TruffleShuffle-Retail-Ltd" "shp") ("UM-Fashion" "shp") ("littledaisydream" "shp") ("Coconut-Lane" "shp") ("Eleanor-Bowmer" "shp") ("Emma-Matratzen" "shp") ("SharkNinja" "shp") ("lookfantastic" "shp") ("cleverbridge" "web") ("Select-Specs" "shp") ("Green-Sheep-Group-Limited" "shp") ("FastSpring-Limited" "shp") ("Hair-Solutions" "har") ("URBN-UK-LIMITED" "shp") ("Semantical-Ltd" "shp") ("United-Arts" "shp") (".*" "o"))))

(with-eval-after-load 'bank-buddy
  (add-hook 'org-mode-hook 'bank-buddy-cat-maybe-enable))

(define-key my-jump-keymap (kbd "l") #'consult-theme)

;;
;; -> ollama-buddy
;;
(use-package ollama-buddy
  ;; :load-path "~/source/repos/ollama-buddy/ollama-buddy-mini"
  :load-path "~/source/repos/ollama-buddy"
  :bind
  ("C-c o" . ollama-buddy-menu)
  ("C-c O" . ollama-buddy-transient-menu-wrapper)
  :config
  ;; Load curl backend first
  (require 'ollama-buddy-curl nil t)
  
  ;; Then set the backend
  (setq ollama-buddy-communication-backend 'curl)
  (setq ollama-buddy-default-model "c:claude-sonnet-4-20250514")
  (setq ollama-buddy-autocomplete-model "o:tinyllama:latest")
  (setq ollama-buddy-openai-api-key
        (auth-source-pick-first-password :host "ollama-buddy-openai" :user "apikey"))
  (setq ollama-buddy-claude-api-key
        (auth-source-pick-first-password :host "ollama-buddy-claude" :user "apikey"))
  (setq ollama-buddy-gemini-api-key
        (auth-source-pick-first-password :host "ollama-buddy-gemini" :user "apikey"))
  (setq ollama-buddy-grok-api-key
        (auth-source-pick-first-password :host "ollama-buddy-grok" :user "apikey"))
  
  (add-to-list 'ollama-buddy-command-definitions
               '(OpenHere
                 :key ?O
                 :description "Open Here"
                 :action (lambda () (switch-to-buffer "*Ollama Buddy Chat*")
                           (ollama-buddy--initialize-chat-buffer)
                           (goto-char (point-max)))))
  
  (require 'ollama-buddy-openai nil t)
  (require 'ollama-buddy-claude nil t)
  (require 'ollama-buddy-gemini nil t)
  (require 'ollama-buddy-grok nil t)
  
  (ollama-buddy-update-menu-entry
   'git-commit :model "a:gpt-4o")
  (ollama-buddy-update-menu-entry
   'describe-code :model "o:qwen2.5-coder:3b")
  (ollama-buddy-update-menu-entry
   'dictionary-lookup :model "o:llama3.2:3b")
  (ollama-buddy-update-menu-entry
   'synonym :model "o:llama3.2:3b")
  (ollama-buddy-update-menu-entry
   'proofread :model "a:gpt-4.1"))

(eval-after-load 'dired
  '(progn
     (define-key dired-mode-map (kbd "C-c C-a") #'ollama-buddy-dired-attach-marked-files)))

;;
;; -> other
;;
(defun convert-weight (weight)
  "Convert WEIGHT from string to pounds."
  (let* ((parts (split-string weight ":"))
         (stone (string-to-number (car parts)))
         (pounds (string-to-number (cadr parts))))
    (+ (* stone 14) pounds)))

(use-package cursor-heatmap
  :load-path "~/source/repos/cursor-heatmap"
  :config
  (cursor-heatmap-mode 1)
  :custom
  (cursor-heatmap-grid-width 20)
  (cursor-heatmap-grid-height 20)
  (cursor-heatmap-use-colors t))

(use-package simply-annotate
  :load-path "~/source/repos/simply-annotate"
  :hook
  (find-file-hook . simply-annotate-mode)
  :bind
  ("C-c A" . simply-annotate-mode)
  ("C-c 0" . simply-annotate-show-all))

(setq ollama-buddy-communication-backend 'curl)

(setq flymake-show-diagnostics-at-end-of-line nil)

(tiny-diminish 'selected-window-accent-mode)
(tiny-diminish 'cursor-heatmap-mode)
(tiny-diminish 'simply-annotate-mode)
(tiny-diminish 'simple-autosuggest-mode)
(tiny-diminish 'org-indent-mode)

;; (use-package evie
;;   :load-path "~/source/repos/evie"
;;   :config
;;   ;; Enable EVIE globally
;;   (evie-mode 1)

;;   ;; Customize behavior
;;   (setq evie-auto-view-files t                    ; Auto-enable view-mode
;;         evie-return-to-view-after-save t          ; Return to view after save
;;         evie-visual-feedback t                    ; Change cursor type
;;         evie-scroll-lines 3                       ; Lines to scroll with u/d
;;         evie-mode-line-indicator " 👶"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "ADBO" :slant normal :weight regular :height 100 :width normal))))
 '(completions-common-part ((t (:foreground "#87ceeb"))))
 '(completions-first-difference ((t (:foreground "#ffb6c1"))))
 '(cursor ((t (:background "coral"))))
 '(ediff-current-diff-A ((t (:extend t :background "#b5daeb" :foreground "#000000"))))
 '(ediff-even-diff-A ((t (:background "#bafbba" :foreground "#000000" :extend t))))
 '(ediff-fine-diff-A ((t (:background "#f4bd92" :foreground "#000000" :extend t))))
 '(ediff-odd-diff-A ((t (:background "#b8fbb8" :foreground "#000000" :extend t))))
 '(fixed-pitch ((t (:family "Source Code Pro" :height 110))))
 '(font-lock-warning-face ((t (:foreground "#930000" :inverse-video nil))))
 '(fringe ((t (:foreground "#2d3743" :background "#2d3743"))))
 '(hl-line ((t (:background "#3d4753"))))
 '(icomplete-first-match ((t (:foreground "#7c7c75" :background "#3a3a3a" :weight bold))))
 '(icomplete-selected-match ((t (:foreground "#ffffff" :background "#5f87af" :weight bold))))
 '(indent-guide-face ((t (:background "#282828" :foreground "#666666"))))
 '(mode-line ((t (:height 140 :underline nil :overline nil :box nil))))
 '(mode-line-inactive ((t (:height 140 :underline nil :overline nil :box nil))))
 '(org-level-1 ((t (:inherit default :weight regular :height 1.0))))
 '(org-level-2 ((t (:inherit default :weight light :height 1.0))))
 '(org-level-3 ((t (:inherit default :weight light :height 1.0))))
 '(org-level-4 ((t (:inherit default :weight light :height 1.0))))
 '(org-level-5 ((t (:inherit default :weight light :height 1.0))))
 '(org-level-6 ((t (:inherit default :weight light :height 1.0))))
 '(org-link ((t (:underline nil))))
 '(org-tag ((t (:height 0.9))))
 '(tab-bar ((t (:inherit default :background "#2d3743" :foreground "#e1e1e0"))))
 '(tab-bar-tab ((t (:inherit 'highlight :background "coral" :foreground "#000000"))))
 '(tab-bar-tab-inactive ((t (:inherit default :background "#2d3743" :foreground "#e1e1e0" :box (:line-width 1 :color "#2d3743" :style flat-button)))))
 '(variable-pitch ((t (:family "DejaVu Sans" :height 120 :weight normal))))
 '(vertical-border ((t (:foreground "#000000"))))
 '(widget-button ((t (:inherit fixed-pitch :weight regular))))
 '(window-divider ((t (:foreground "black")))))
