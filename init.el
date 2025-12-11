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
                           ("org" . "https://orgmode.org/elpa/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/"))))

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
  (select1ed-window-accent-fringe-thickness 10)
  (selected-window-accent-custom-color nil)
  (selected-window-accent-mode-style 'default)
  (selected-window-accent-percentage-darken 20)
  (selected-window-accent-percentage-desaturate 20)
  (selected-window-accent-tab-accent t)
  (selected-window-accent-use-pywal t)
  (selected-window-accent-smart-borders nil))

(global-set-key (kbd "C-c w") selected-window-accent-map)

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
  ;; (setq font-general "Source Code Pro 10")
  ;; (setq font-general "Source Code Pro Light 11")
  ;; (setq font-general "Monospace 10")
  ;;(setq font-general "Nimbus Mono PS 13")
  ;; (set-frame-font font-general nil t)
  ;; (add-to-list 'default-frame-alist `(font . ,font-general))
  (setq diary-file "~/DCIM/content/diary.org"))

;;
;; -> themes
;;
(use-package doom-themes)
(use-package ef-themes)
(use-package gruvbox-theme)
(use-package timu-caribbean-theme)

(use-package timu-spacegrey-theme
  :config
  ;; (setq timu-spacegrey-flavour "light")
  (setq timu-spacegrey-scale-org-document-title 1.8)
  (setq timu-spacegrey-scale-org-document-info 1.4)
  (setq timu-spacegrey-scale-org-level-1 1.8)
  (setq timu-spacegrey-scale-org-level-2 1.4)
  (setq timu-spacegrey-scale-org-level-3 1.2))

(use-package timu-rouge-theme
  :config
  ;; (setq timu-rouge-org-intense-colors t)
  (setq timu-rouge-mode-line-border t)
  (setq timu-rouge-scale-org-document-title 1.8)
  (setq timu-rouge-scale-org-document-info 1.4)
  (setq timu-rouge-scale-org-level-1 1.8)
  (setq timu-rouge-scale-org-level-2 1.4)
  (setq timu-rouge-scale-org-level-3 1.2))

(load-theme 'timu-spacegrey t)

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
 '(package-selected-packages
   '(async claude-code consult corfu csv-mode dape dirvish doom-themes
           eat eca ef-themes flycheck gptel gruvbox-theme
           i3wm-config-mode org-social org-superstar ox-hugo
           package-lint ready-player timu-caribbean-theme
           timu-rouge-theme timu-spacegrey-theme vterm yaml-mode ztree))
 '(package-vc-selected-packages
   '((claude-code :url "https://github.com/stevemolitor/claude-code.el")))
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

;; (my/sync-ui-accent-color "coral")

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
  (setq ollama-buddy-default-model "c:claude-sonnet-4-5-20250929")
  (setq ollama-buddy-autocomplete-model "o:tinyllama:latest")
  (setq ollama-buddy-openai-api-key
        (auth-source-pick-first-password :host "ollama-buddy-openai" :user "apikey"))
  (setq ollama-buddy-claude-api-key
        (auth-source-pick-first-password :host "ollama-buddy-claude" :user "apikey"))
  (setq ollama-buddy-gemini-api-key
        (auth-source-pick-first-password :host "ollama-buddy-gemini" :user "apikey"))
  (setq ollama-buddy-grok-api-key
        (auth-source-pick-first-password :host "ollama-buddy-grok" :user "apikey"))
  (setq ollama-buddy-codestral-api-key
        (auth-source-pick-first-password :host "ollama-buddy-codestral" :user "apikey"))

  (setq ollama-buddy-codestral-api-endpoint "https://api.mistral.ai/v1/chat/completions")
  ;; (setq ollama-buddy-codestral-default-model "codestral-latest")
  
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
  (require 'ollama-buddy-codestral nil t)
  
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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Monospace" :foundry "ADBO" :slant normal :weight regular :height 95 :width normal))))
 '(completions-common-part ((t (:foreground "#87ceeb"))))
 '(completions-first-difference ((t (:foreground "#ffb6c1"))))
 '(cursor ((t (:background "coral"))))
 '(ediff-current-diff-A ((t (:extend t :background "#b5daeb" :foreground "#000000"))))
 '(ediff-even-diff-A ((t (:background "#bafbba" :foreground "#000000" :extend t))))
 '(ediff-fine-diff-A ((t (:background "#f4bd92" :foreground "#000000" :extend t))))
 '(ediff-odd-diff-A ((t (:background "#b8fbb8" :foreground "#000000" :extend t))))
 '(fixed-pitch ((t (:family "Monospace" :height 110))))
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

(use-package meal-planner
  :load-path "~/source/repos/meal-planner")

(use-package org-social
  :config
  (setq org-social-file "https://host.org-social.org/vfile?token=31841933dc6ee63665fdd874eb60088a21b1ef066939f2b73dea8b51d8ce268d&ts=1764570277&sig=c7a678e4123f8ae5608de3d527fb507029325bce560556cb6465732f481e6de6")
  (setq org-social-relay "https://relay.org-social.org/")
  (setq org-social-my-public-url "https://host.org-social.org/captainflasmr/social.org"))

(use-package dired-video-thumbnail
  :load-path "~/source/repos/dired-video-thumbnail"
  :bind (:map dired-mode-map
              ("C-t v" . dired-video-thumbnail))
  :custom
  (dired-video-thumbnail-size 250)
  (dired-video-thumbnail-display-height 150)
  (dired-video-thumbnail-columns 8)
  (dired-video-thumbnail-timestamp "00:00:02")
  (dired-video-thumbnail-video-player "mpv")
  (dired-video-thumbnail-mark-border-width 5)
  :custom-face
  (dired-video-thumbnail-mark ((t (:foreground "orange")))))

(use-package dirvish)

;; install required inheritenv dependency:
(use-package inheritenv
  :vc (:url "https://github.com/purcell/inheritenv" :rev :newest))

;; for eat terminal backend:
(use-package eat :ensure t)

;; for vterm terminal backend:
(use-package vterm :ensure t)

;; install claude-code.el
(use-package claude-code :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :config
  ;; optional IDE integration with Monet
  (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  (monet-mode 1)

  (claude-code-mode)
  :bind-keymap ("C-c C" . claude-code-command-map)

  ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
  :bind
  (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode)))

(global-set-key (kbd "C-c C") 'claude-code-transient)

(use-package gptel
  :ensure t
  :config
  ;; OpenAI API key (used by default backend)
  (setq gptel-api-key (auth-source-pick-first-password
                       :host "ollama-buddy-openai"
                       :user "apikey"))

  ;; Claude backend
  (gptel-make-anthropic "Claude"
    :stream t
    :key (auth-source-pick-first-password
          :host "ollama-buddy-claude"
          :user "apikey"))

  ;; Set Claude as default
  (setq gptel-model 'claude-3-7-sonnet-20250219
        gptel-backend (gptel-get-backend "Claude"))
  
  (setq gptel-default-mode 'org-mode)
  (setq gptel-use-tools t)
  (setq gptel-include-tool-results t)
  (setq gptel-confirm-tool-calls t)

  (setq gptel-directives
        '((default . "You are a helpful assistant.")
          (code-review . "You are a code review assistant.")
          (engineer . "You are an expert software engineer. Use your tools to read files, search code, and help with programming tasks.")))

  ;; ============================================================
  ;; EDIFF-BASED FILE EDITING
  ;; ============================================================

  (defvar gptel-tool--ediff-target-file nil
    "The file being edited via gptel ediff.")
  
  (defvar gptel-tool--ediff-result nil
    "Result of the last gptel ediff operation.")

  (defvar gptel-tool--ediff-buffers nil
    "Cons of (original-buf . modified-buf) for cleanup.")

  (defun gptel-tool--ediff-quit-hook ()
    "Hook run when ediff quits. Prompts to save and cleans up."
    (when gptel-tool--ediff-target-file
      (let ((modified-content (with-current-buffer ediff-buffer-B
                                (buffer-substring-no-properties (point-min) (point-max))))
            (original-content (with-current-buffer ediff-buffer-A
                                (buffer-substring-no-properties (point-min) (point-max)))))
        (if (string= modified-content original-content)
            (setq gptel-tool--ediff-result
                  (format "No changes made to %s" gptel-tool--ediff-target-file))
          (if (y-or-n-p (format "Save changes to %s? " gptel-tool--ediff-target-file))
              (progn
                (with-temp-file gptel-tool--ediff-target-file
                  (insert modified-content))
                ;; Revert the buffer if it's visiting this file
                (when-let ((buf (find-buffer-visiting gptel-tool--ediff-target-file)))
                  (with-current-buffer buf
                    (revert-buffer t t t)))
                (setq gptel-tool--ediff-result
                      (format "Applied changes to %s" gptel-tool--ediff-target-file)))
            (setq gptel-tool--ediff-result
                  (format "Discarded changes to %s" gptel-tool--ediff-target-file)))))
      
      ;; Cleanup
      (when gptel-tool--ediff-buffers
        (ignore-errors (kill-buffer (car gptel-tool--ediff-buffers)))
        (ignore-errors (kill-buffer (cdr gptel-tool--ediff-buffers))))
      (setq gptel-tool--ediff-target-file nil)
      (setq gptel-tool--ediff-buffers nil)))

  (defun gptel-tool--ediff-replace (path old-string new-string)
    "Replace OLD-STRING with NEW-STRING in file at PATH using ediff."
    (let* ((full-path (expand-file-name path))
           (original-content (condition-case err
                                 (with-temp-buffer
                                   (insert-file-contents full-path)
                                   (buffer-string))
                               (error (format "Error reading file: %s" (error-message-string err))))))
      
      ;; Check for read errors
      (when (string-prefix-p "Error" original-content)
        (cl-return-from gptel-tool--ediff-replace original-content))
      
      (let ((count (with-temp-buffer
                     (insert original-content)
                     (how-many (regexp-quote old-string) (point-min) (point-max)))))
        (cond
         ((= count 0)
          (format "Error: String not found in %s" path))
         ((> count 1)
          (format "Error: String found %d times in %s. Must be unique for safe replacement." count path))
         (t
          (let* ((new-content (replace-regexp-in-string
                               (regexp-quote old-string) new-string original-content t t))
                 (original-buf (generate-new-buffer
                                (format "*original: %s*" (file-name-nondirectory path))))
                 (modified-buf (generate-new-buffer
                                (format "*proposed: %s*" (file-name-nondirectory path)))))
            
            ;; Populate original buffer
            (with-current-buffer original-buf
              (insert original-content)
              (goto-char (point-min))
              (let ((buffer-file-name full-path))
                (set-auto-mode)
                (font-lock-ensure))
              (set-buffer-modified-p nil)
              (setq buffer-read-only t))
            
            ;; Populate modified buffer
            (with-current-buffer modified-buf
              (insert new-content)
              (goto-char (point-min))
              (let ((buffer-file-name full-path))
                (set-auto-mode)
                (font-lock-ensure))
              (set-buffer-modified-p nil))
            
            ;; Prompt user for action
            (let ((action (read-char-choice
                           (format "Change in %s:\n\n  -%s\n  +%s\n\n[a]pply, [e]diff, [s]kip: "
                                   (file-name-nondirectory path)
                                   (truncate-string-to-width old-string 70 nil nil "...")
                                   (truncate-string-to-width new-string 70 nil nil "..."))
                           '(?a ?e ?s))))
              (pcase action
                (?a
                 ;; Apply directly without ediff
                 (with-temp-file full-path
                   (insert new-content))
                 (kill-buffer original-buf)
                 (kill-buffer modified-buf)
                 (when-let ((buf (find-buffer-visiting full-path)))
                   (with-current-buffer buf
                     (revert-buffer t t t)))
                 (format "Applied change to %s" path))
                
                (?e
                 ;; Launch ediff
                 (setq gptel-tool--ediff-target-file full-path)
                 (setq gptel-tool--ediff-buffers (cons original-buf modified-buf))
                 (setq gptel-tool--ediff-result nil)
                 
                 ;; Add our quit hook
                 (add-hook 'ediff-quit-hook #'gptel-tool--ediff-quit-hook)
                 
                 ;; Start ediff - A is original, B is proposed changes
                 (ediff-buffers original-buf modified-buf)
                 
                 ;; Ediff runs asynchronously, return a pending message
                 ;; The actual result will be shown when ediff quits
                 (format "Ediff started for %s. Use 'n'/'p' to navigate hunks, 'a'/'b' to choose version, 'q' to quit and save." path))
                
                (?s
                 ;; Skip
                 (kill-buffer original-buf)
                 (kill-buffer modified-buf)
                 (format "Skipped change to %s" path))))))))))

  ;; ============================================================
  ;; SIMPLER DIFF ALTERNATIVE (if you prefer)
  ;; ============================================================

  (defun gptel-tool--diff-replace (path old-string new-string)
    "Replace OLD-STRING with NEW-STRING in file at PATH, showing diff for confirmation."
    (let* ((full-path (expand-file-name path))
           (original-content (condition-case err
                                 (with-temp-buffer
                                   (insert-file-contents full-path)
                                   (buffer-string))
                               (error nil))))
      (unless original-content
        (cl-return-from gptel-tool--diff-replace
          (format "Error: Cannot read file %s" path)))
      
      (let ((count (with-temp-buffer
                     (insert original-content)
                     (how-many (regexp-quote old-string) (point-min) (point-max)))))
        (cond
         ((= count 0)
          (format "Error: String not found in %s" path))
         ((> count 1)
          (format "Error: String found %d times, must be unique" count))
         (t
          (let* ((new-content (replace-regexp-in-string
                               (regexp-quote old-string) new-string original-content t t))
                 (diff-output (let ((old-temp (make-temp-file "gptel-old"))
                                    (new-temp (make-temp-file "gptel-new")))
                                (unwind-protect
                                    (progn
                                      (with-temp-file old-temp (insert original-content))
                                      (with-temp-file new-temp (insert new-content))
                                      (shell-command-to-string
                                       (format "diff -u --color=never %s %s | tail -n +3"
                                               (shell-quote-argument old-temp)
                                               (shell-quote-argument new-temp))))
                                  (delete-file old-temp)
                                  (delete-file new-temp)))))
            
            ;; Show diff in a buffer
            (with-current-buffer (get-buffer-create "*gptel-diff*")
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert (propertize (format "Proposed changes to: %s\n" path)
                                    'face 'font-lock-keyword-face))
                (insert (make-string 60 ?─) "\n\n")
                (insert diff-output)
                (goto-char (point-min))
                (diff-mode)
                (view-mode 1))
              (display-buffer (current-buffer)
                              '((display-buffer-reuse-window
                                 display-buffer-below-selected)
                                (window-height . 0.4))))
            
            (if (y-or-n-p (format "Apply change to %s? " path))
                (progn
                  (with-temp-file full-path
                    (insert new-content))
                  (when-let ((buf (find-buffer-visiting full-path)))
                    (with-current-buffer buf
                      (revert-buffer t t t)))
                  (when-let ((diff-buf (get-buffer "*gptel-diff*")))
                    (kill-buffer diff-buf))
                  (format "Applied change to %s" path))
              (when-let ((diff-buf (get-buffer "*gptel-diff*")))
                (kill-buffer diff-buf))
              (format "Skipped change to %s" path))))))))

  ;; ============================================================
  ;; TOOLS
  ;; ============================================================

  (setq gptel-tools
        (list
         ;; ============ FILE OPERATIONS ============
         (gptel-make-tool
          :function (lambda (path)
                      (condition-case err
                          (with-temp-buffer
                            (insert-file-contents (expand-file-name path))
                            (buffer-string))
                        (error (format "Error reading file: %s" (error-message-string err)))))
          :name "read_file"
          :description "Read the contents of a file."
          :args (list '(:name "path" :type string :description "The path to the file"))
          :category "filesystem")

         (gptel-make-tool
          :function (lambda (path content)
                      (let* ((full-path (expand-file-name path))
                             (exists (file-exists-p full-path))
                             (prompt (if exists
                                         (format "Overwrite %s (%d bytes)? " path (length content))
                                       (format "Create %s (%d bytes)? " path (length content)))))
                        (if (y-or-n-p prompt)
                            (progn
                              (make-directory (file-name-directory full-path) t)
                              (with-temp-file full-path
                                (insert content))
                              (format "Wrote %d bytes to %s" (length content) path))
                          (format "Skipped writing to %s" path))))
          :name "write_file"
          :description "Write content to a file, creating or overwriting it."
          :args (list '(:name "path" :type string :description "The path to the file")
                      '(:name "content" :type string :description "The content to write"))
          :category "filesystem")

         (gptel-make-tool
          :function #'gptel-tool--ediff-replace
          :name "str_replace_in_file"
          :description "Replace a unique string in a file with another string. Shows diff and asks for confirmation."
          :args (list '(:name "path" :type string :description "The path to the file")
                      '(:name "old_string" :type string :description "The exact string to replace (must be unique)")
                      '(:name "new_string" :type string :description "The replacement string"))
          :category "filesystem")

         (gptel-make-tool
          :function (lambda (directory)
                      (condition-case err
                          (mapconcat (lambda (f)
                                       (if (file-directory-p (expand-file-name f directory))
                                           (concat f "/") f))
                                     (directory-files (expand-file-name directory) nil "^[^.]")
                                     "\n")
                        (error (format "Error: %s" (error-message-string err)))))
          :name "list_directory"
          :description "List files and directories. Directories have trailing slash."
          :args (list '(:name "directory" :type string :description "The directory path"))
          :category "filesystem")

         (gptel-make-tool
          :function (lambda (directory pattern)
                      (shell-command-to-string
                       (format "find %s -type f -name %s 2>/dev/null | head -50"
                               (shell-quote-argument (expand-file-name directory))
                               (shell-quote-argument pattern))))
          :name "find_files"
          :description "Find files matching a glob pattern."
          :args (list '(:name "directory" :type string :description "The root directory")
                      '(:name "pattern" :type string :description "Glob pattern like '*.py'"))
          :category "filesystem")

         ;; ============ CODE SEARCH ============
         (gptel-make-tool
          :function (lambda (pattern directory)
                      (let ((cmd (if (executable-find "rg")
                                     (format "rg -n --no-heading %s %s 2>/dev/null | head -100"
                                             (shell-quote-argument pattern)
                                             (shell-quote-argument (expand-file-name directory)))
                                   (format "grep -rn %s %s 2>/dev/null | head -100"
                                           (shell-quote-argument pattern)
                                           (shell-quote-argument (expand-file-name directory))))))
                        (let ((result (shell-command-to-string cmd)))
                          (if (string-empty-p result) "No matches found." result))))
          :name "search_code"
          :description "Search for a pattern in files using ripgrep or grep."
          :args (list '(:name "pattern" :type string :description "The search pattern")
                      '(:name "directory" :type string :description "Directory to search"))
          :category "code")

         (gptel-make-tool
          :function (lambda (symbol directory)
                      (let ((cmd (if (executable-find "rg")
                                     (format "rg -n --no-heading -w %s %s 2>/dev/null | head -50"
                                             (shell-quote-argument symbol)
                                             (shell-quote-argument (expand-file-name directory)))
                                   (format "grep -rnw %s %s 2>/dev/null | head -50"
                                           (shell-quote-argument symbol)
                                           (shell-quote-argument (expand-file-name directory))))))
                        (let ((result (shell-command-to-string cmd)))
                          (if (string-empty-p result) "No matches found." result))))
          :name "find_symbol"
          :description "Find a symbol (function, variable, class) as a whole word."
          :args (list '(:name "symbol" :type string :description "The symbol name")
                      '(:name "directory" :type string :description "Directory to search"))
          :category "code")

         ;; ============ SHELL ============
         (gptel-make-tool
          :function (lambda (command directory)
                      (let ((default-directory (expand-file-name (or directory default-directory))))
                        (if (y-or-n-p (format "Run: %s\nin: %s? " command default-directory))
                            (shell-command-to-string command)
                          "Command cancelled by user")))
          :name "run_shell_command"
          :description "Run a shell command and return output."
          :args (list '(:name "command" :type string :description "The shell command")
                      '(:name "directory" :type string :description "Working directory (optional)"))
          :category "system")

         ;; ============ GIT ============
         (gptel-make-tool
          :function (lambda (directory)
                      (let ((default-directory (expand-file-name directory)))
                        (shell-command-to-string "git status --short 2>/dev/null || echo 'Not a git repo'")))
          :name "git_status"
          :description "Get git status."
          :args (list '(:name "directory" :type string :description "Path to git repository"))
          :category "git")

         (gptel-make-tool
          :function (lambda (directory)
                      (let ((default-directory (expand-file-name directory)))
                        (shell-command-to-string "git diff 2>/dev/null")))
          :name "git_diff"
          :description "Show unstaged git changes."
          :args (list '(:name "directory" :type string :description "Path to git repository"))
          :category "git")

         (gptel-make-tool
          :function (lambda (directory n)
                      (let ((default-directory (expand-file-name directory)))
                        (shell-command-to-string (format "git log --oneline -n %d 2>/dev/null" (or n 10)))))
          :name "git_log"
          :description "Show recent git commits."
          :args (list '(:name "directory" :type string :description "Path to git repository")
                      '(:name "n" :type integer :description "Number of commits (default 10)"))
          :category "git")

         ;; ============ EMACS ============
         (gptel-make-tool
          :function (lambda (buffer)
                      (if (buffer-live-p (get-buffer buffer))
                          (with-current-buffer buffer
                            (buffer-substring-no-properties (point-min) (point-max)))
                        (format "Buffer '%s' not found" buffer)))
          :name "read_buffer"
          :description "Read contents of an Emacs buffer."
          :args (list '(:name "buffer" :type string :description "Buffer name"))
          :category "emacs")

         (gptel-make-tool
          :function (lambda ()
                      (format "Buffer: %s\nFile: %s\nMode: %s\nDirectory: %s"
                              (buffer-name)
                              (or buffer-file-name "none")
                              major-mode
                              default-directory))
          :name "current_context"
          :description "Get current buffer/file/directory info."
          :args nil
          :category "emacs")

         (gptel-make-tool
          :function (lambda ()
                      (mapconcat (lambda (b)
                                   (format "%s [%s]" (buffer-name b)
                                           (buffer-local-value 'major-mode b)))
                                 (seq-filter (lambda (b)
                                               (not (string-prefix-p " " (buffer-name b))))
                                             (buffer-list))
                                 "\n"))
          :name "list_buffers"
          :description "List open Emacs buffers."
          :args nil
          :category "emacs"))))

;; (use-package aidermacs
;;   :config
;;   (setq aidermacs-extra-args 
;;         '("--model" "anthropic/claude-sonnet-4-5"
;;           "--map-tokens" "2048"))

;;   (setenv "OPENAI_API_KEY" 
;;           (auth-source-pick-first-password
;;            :host "ollama-buddy-openai" 
;;            :user "apikey"))

;;   (setenv "ANTHROPIC_API_KEY" 
;;           (auth-source-pick-first-password
;;            :host "ollama-buddy-claude" 
;;            :user "apikey")))

(my/sync-ui-accent-color "orange")

(use-package corfu
  :custom
  (corfu-auto nil)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-separator ?\s)
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match nil)
  (corfu-preview-current nil)
  (corfu-preselect 'first)
  (corfu-on-exact-match nil)
  (corfu-scroll-margin 5))

(use-package ztree
  :config
  (setq-default ztree-diff-filter-list
                '(
                  "build" "\.dll" "\.iso" "\.xmp" "\.cache" "\.gnupg" "\.local"
                  "\.mozilla" "\.thunderbird" "\.wine" "\.mp3" "\.mp4" "\.arpack"
                  "\.git" "^Volume$" "^Games$" "^cache$" "^chromium$" "^elpa$" "^nas$"
                  "^syncthing$" "bin" "obj"
                  ))
  ;; (setq-default ztree-diff-additional-options '("-w" "-i"))
  (setq-default ztree-diff-consider-file-size t)
  (setq-default ztree-diff-consider-file-permissions nil)
  (setq-default ztree-diff-show-equal-files nil)
  
  ;; Add key binding for 'g' to full rescan
  (with-eval-after-load 'ztree-diff
    (define-key ztree-mode-map (kbd "g") 'ztree-diff-full-rescan))
  
  ;; Helper function to get directories from dired windows
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
  
  ;; Enhanced ztree-diff with directory DWIM
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
  
  ;; Optionally bind the enhanced function to a key
  (global-set-key (kbd "C-c z d") 'ztree-diff-dwim))

(with-eval-after-load 'ztree

  (define-key ztree-mode-map (kbd "n") #'ztree-next-line)
  (define-key ztree-mode-map (kbd "p") #'ztree-previous-line)

  ;; Preserve point in ztree buffers when switching tab history
  ;; Some window-configuration changes (eg. `tab-bar-history-back') can
  ;; redisplay buffers and reset their point. Save ztree buffer points
  ;; before the history change and restore them for visible windows after.
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

  (defun my/ztree--around-tab-history (orig-fun &rest args)
    "Save/restore ztree points around tab history commands.
ORIG-FUN is the original command and ARGS are its arguments."
    (my/ztree-save-all-points)
    (prog1
        (apply orig-fun args)
      (my/ztree-restore-visible-points)))

  (advice-add 'tab-bar-history-back :around #'my/ztree--around-tab-history)
  (advice-add 'tab-bar-history-forward :around #'my/ztree--around-tab-history))

;; Remap ztree faces to sensible theme faces so ztree matches the current theme.
(defun my/ztree-remap-faces ()
  "Map ztree/ztreep faces to theme faces for coherence with current theme."
  (dolist (fn (face-list))
    (let ((name (symbol-name fn)))
      (when (or (string-prefix-p "ztree" name)
                (string-prefix-p "ztreep" name))
        (cond
         ((string-match-p "add\\|added\\|add-face" name)
          (set-face-attribute fn nil :foreground 'unspecified :background 'unspecified :inherit 'success))
         ((string-match-p "remove\\|del\\|delete\\|missing\\|removed" name)
          (set-face-attribute fn nil :foreground 'unspecified :background 'unspecified :inherit 'error))
         ((string-match-p "diff\\|model-diff\\|equal" name)
          (set-face-attribute fn nil :foreground 'unspecified :background 'unspecified :inherit 'font-lock-comment-face))
         ((string-match-p "model\\|name" name)
          (set-face-attribute fn nil :foreground 'unspecified :background 'unspecified :inherit 'font-lock-function-name-face))
         (t
          (set-face-attribute fn nil :foreground 'unspecified :background 'unspecified :inherit 'default)))))))

;; Run remapping after any theme is loaded, and now if a theme is already active.
(advice-add 'load-theme :after (lambda (&rest _) (my/ztree-remap-faces)))
(when custom-enabled-themes
  (my/ztree-remap-faces))

(use-package highlight-indent-guides
  :load-path "z:/SharedVM/source/highlight-indent-guides-master"
  :mode "\\.cshtml?\\'"
  :hook (html-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character)
  ;; (setq highlight-indent-guides-method 'fill)
  ;; highlight-indent-guides-character ?\|))
  ;; (setq highlight-indent-guides-auto-enabled nil)
  (setq highlight-indent-guides-auto-enabled nil)
  (set-face-background 'highlight-indent-guides-odd-face "#e8e8e8")
  (set-face-background 'highlight-indent-guides-even-face "#dedede")
  (set-face-foreground 'highlight-indent-guides-character-face "#4e535e"))

(defun insert-default-background-color ()
  "Insert the default background color at point."
  (interactive)
  (insert (downcase (face-attribute 'default :background))))

(global-set-key (kbd "C-q i") 'highlight-indent-guides-mode)
(global-set-key (kbd "M-s b") ' insert-default-background-color)

(use-package web-mode
  :load-path "z:/SharedVM/source/web-mode-master"
  :mode "\\.cshtml?\\'"
  :hook (html-mode . web-mode)
  :bind (:map web-mode-map ("M-;" . nil)))

(add-to-list 'auto-mode-alist '("\\.cshtml\\'" . html-mode))

(setq eglot-ignored-server-capabilities
      '(
        ;; :hoverProvider                    ; Documentation on hover
        ;; :completionProvider               ; Code completion
        ;; :signatureHelpProvider            ; Function signature help
        ;; :definitionProvider               ; Go to definition
        ;; :typeDefinitionProvider           ; Go to type definition
        ;; :implementationProvider           ; Go to implementation
        ;; :declarationProvider              ; Go to declaration
        ;; :referencesProvider               ; Find references
        ;; :documentHighlightProvider        ; Highlight symbols automatically
        ;; :documentSymbolProvider           ; List symbols in buffer
        ;; :workspaceSymbolProvider          ; List symbols in workspace
        ;; :codeActionProvider               ; Execute code actions
        ;; :codeLensProvider                 ; Code lens
        ;; :documentFormattingProvider       ; Format buffer
        ;; :documentRangeFormattingProvider  ; Format portion of buffer
        ;; :documentOnTypeFormattingProvider ; On-type formatting
        ;; :renameProvider                   ; Rename symbol
        ;; :documentLinkProvider             ; Highlight links in document
        ;; :colorProvider                    ; Decorate color references
        ;; :foldingRangeProvider             ; Fold regions of buffer
        ;; :executeCommandProvider           ; Execute custom commands
        ;; :inlayHintProvider                ; Inlay hints
        ))

(use-package ibuffer
  :bind (:map ibuffer-mode-map ("M-o" . nil)))

(setq recentf-max-menu-items 40)
(setq recentf-max-saved-items 40)

(setq max-mini-window-height 6)

(setq recentf-max-menu-items 40)
(setq recentf-max-saved-items 40)

(setq max-mini-window-height 12)
(use-package dape
  :init
  ;; Set key prefix BEFORE loading dape
  (setq dape-key-prefix (kbd "C-c d"))
  :config
  ;; Define common configuration
  (defvar mimesis-netcoredbg-path "d:/source/emacs-30.1/bin/netcoredbg/netcoredbg.exe"
    "Path to netcoredbg executable.")
  (defvar mimesis-netcoredbg-log "d:/source/emacs-30.1/bin/netcoredbg/netcoredbg.log"
    "Path to netcoredbg log file.")
  (defvar mimesis-project-root "d:/source/MIMESIS-OVC"
    "Root directory of MIMESIS-OVC project.")
  (defvar mimesis-build-config "Debug"
    "Build configuration (Debug or Release).")
  (defvar mimesis-target-arch "x64"
    "Target architecture (x64, x86, or AnyCPU).")
  (defvar mimesis-vsdbg-path "C:/Users/vm.j.dyer/.vscode/extensions/ms-dotnettools.csharp-2.80.16-win32-x64/.debugger/x86_64/vsdbg.exe"
    "Path to vsdbg from VSCode installation.")

  ;; Helper function to create component configs
  (defun mimesis-dape-config (component-name dll-name &optional stop-at-entry)
    "Create a dape configuration for a component.
COMPONENT-NAME is the component directory name
DLL-NAME is the DLL filename without extension.
STOP-AT-ENTRY if non-nil, stops at program entry point."
    (let* ((component-dir (format "%s/%s" mimesis-project-root component-name))
           (bin-path (format "%s/bin/%s/%s/net9.0"
                             component-dir
                             mimesis-target-arch
                             mimesis-build-config))
           (dll-path (format "%s/%s.dll" bin-path dll-name))
           (config-name (intern (format "netcoredbg-launch-%s" 
                                        (downcase component-name)))))
      `(,config-name
        modes (csharp-mode csharp-ts-mode)
        command ,mimesis-netcoredbg-path
        command-args (,(format "--interpreter=vscode")
                      ,(format "--engineLogging=%s" mimesis-netcoredbg-log))
        normalize-path-separator 'windows
        :type "coreclr"
        :request "launch"
        :program ,dll-path
        :cwd ,component-dir
        :console "externalTerminal"
        :internalConsoleOptions "neverOpen"
        :suppressJITOptimizations t
        :requireExactSource nil
        :justMyCode t
        :stopAtEntry ,(if stop-at-entry t :json-false))))

  ;; Register all component configurations
  (dolist (config (list
                   (mimesis-dape-config "IGC" "MIMESIS.IGC" t)
                   (mimesis-dape-config "MSS" "MIMESIS.MSS" t)
                   (mimesis-dape-config "IGM" "MIMESIS.IGM" t)
                   (mimesis-dape-config "VDS" "VDS.MSS" t)
                   (mimesis-dape-config "DM" "DM.MSS" t)
                   (mimesis-dape-config "Demo" "Demo.MSS" t)
                   (mimesis-dape-config "Test_001" "Test" t)))
    (add-to-list 'dape-configs config))
  
  ;; Set buffer arrangement and other options
  (setq dape-buffer-window-arrangement 'gud)
  (setq dape-debug t)
  (setq dape-repl-echo-shell-output t))

(use-package eca
  :config
  (setq eca-chat-diff-tool 'ediff)
  (setq eca-completion-idle-delay 0.1))
