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

(global-set-key (kbd "C-c w") selected-window-accent-map)
(define-key selected-window-accent-map (kbd "m") 'consult-theme)

;;
;; -> org-agenda
;;
(setq org-agenda-files '(
                         "~/DCIM/content/aaa--calendar.org"
                         "~/DCIM/content/aab--calendar-repeat.org"
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
(use-package flycheck)
(use-package gnuplot)
(use-package async)
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
  (setq font-general "Source Code Pro 11")
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
;; -> modes
;;
(server-mode 1)

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
 '(custom-enabled-themes '(doom-oceanic-next))
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

(require 'ob-gnuplot)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((gnuplot . t)))

(use-package bank-buddy
  :load-path "~/source/repos/bank-buddy"
  :custom
  (bank-buddy-core-top-spending-categories 20)
  (bank-buddy-core-top-merchants 20)
  (bank-buddy-core-large-txn-threshold 1200)
  (bank-buddy-core-monthly-spending-bar-width 160)
  (bank-buddy-core-monthly-spending-max-bar-categories 20)
  (bank-buddy-core-cat-list-defines
   '(("katherine\\|james\\|kate" "prs") ("railw\\|railway\\|train" "trn") ("paypal" "pay") ("electric\\|energy\\|water" "utl") ("racing" "bet") ("pension" "pen") ("savings\\|saver" "sav") ("uber" "txi") ("magazine\\|news" "rdg") ("claude\\|reddit\\|mobile\\|backmarket\\|openai\\|web" "web") ("notemachine\\|withdrawal" "atm") ("finance" "fin") ("youtube\\|netflix" "str") ("card" "crd") ("top-up\\|phone" "phn") ("amaz\\|amz" "amz") ("pets\\|pet" "pet") ("dentist" "dnt") ("residential\\|rent\\|mortgage" "hse") ("deliveroo\\|just.*eat" "fod") ("ebay\\|apple\\|itunes" "shp") ("law" "law") ("anyvan" "hmv") ("CHANNEL-4" "str") ("GOOGLE-\\*Google-Play" "web") ("NOW-" "str") ("SALISBURY-CAFE-LOCAL" "fod") ("SAVE-THE-PENNIES" "sav") ("SOUTHAMPTON-GENERAL" "fod") ("TO-Evie" "sav") ("WH-Smith-Princess-Anne" "fod") ("SP-WAXMELTSBYNIC" "shp") ("WWW\\.SSE" "utl") ("THORTFUL" "shp") ("SCOTTISH-WIDOWS" "pen") ("WM-MORRISONS" "fod") ("H3G-REFERENCE" "phn") ("DOMINO" "fod") ("Prime-Video" "str") ("PRIVILEGE" "utl") ("PCC-COLLECTION" "utl") ("MORRISON" "fod") ("BT-GROUP" "web") ("ANTHROPIC" "web") ("INSURE" "utl") ("GOOGLE-Google-Play" "web") ("GILLETT-COPNOR-RD" "fod") ("TV-LICENCE" "utl") ("SAINSBURYS" "fod") ("TESCO" "shp") ("Vinted" "shp") ("PUMPKIN-CAFE" "fod") ("SP-CHAMPO" "shp") ("THE-RANGE" "shp") ("UNIVERSITY-HOSPITA" "fod") ("VIRGIN-MEDIA" "utl") ("GOLDBOUTIQUE" "shp") ("Surveyors" "law") ("Surveyors" "hse") ("INTERFLORA" "shp") ("INSURANCE" "utl") ("LUCINDA-ELLERY" "shp") ("MARKS&SPENCER" "fod") ("SW-PLC-STAKEHOLDE" "pen") ("JUST-MOVE" "hse") ("B&M" "shp") ("PASSPORT-OFFICE" "hse") ("PHARMACY" "shp") ("ONLINE-REDIRECTIONS" "hse") ("SERENATA-FLOWERS" "shp") ("SNAPPER-DESIGN" "shp") ("LOVEFORSLEEP" "shp") ("TJ-WASTE" "hse") ("M-&-S" "fod") ("MARDIN" "fod") ("MOVEWITHUS" "hse") ("STARBUCKS" "fod") ("CD-2515" "shp") ("DEBIT-INTEREST-ARRANGED" "atm") ("ME-GROUP-INTERNATIONAL" "shp") ("COSTA" "fod") ("NYX" "shp") ("NATWEST-BANK-REFERENCE" "hse") ("Streamline" "shp") ("BETHANIE-YEONG" "hse") ("Roofoods" "fod") ("Wayfair" "shp") ("WHSmith" "shp") ("The-Hut" "shp") ("Sky-Betting" "bet") ("NextLtd" "shp") ("NEW-LOOK-RETAILERS" "shp") ("Marks-and-Spencer" "fod") ("DisneyPlus" "str") ("DAZN-LIMITED" "str") ("Astrid-&-Miyu" "shp") ("ASOS\\.COM-Ltd" "shp") ("Cartridge-Tech-Ltd" "shp") ("Dplay-Entertainment-Ltd" "str") ("DeviantArt" "web") ("Dunelm" "shp") ("Asda-Stores" "shp") ("Argos" "shp") ("IKEA-Limited" "shp") ("Lisa-Angel-Limited" "shp") ("Matalan-Retail-Ltd" "shp") ("Royal-Mail-Group-Limited" "utl") ("SCHOTT-PACKAGING" "hse") ("Samsung-Electronics" "shp") ("Boohoo\\.com" "shp") ("Bizzy-Balloons-LLP" "shp") ("BRANDS-IN-BLOOM-LTD" "shp") ("Highland-and-Honey" "shp") ("Homebaked-Limited" "shp") ("Little-Crafts-London-LTD" "shp") ("Lush-Retail-Ltd" "shp") ("Mamas-&-Papas" "shp") ("Mi-Baby" "shp") ("NEOM-Ltd" "shp") ("Oliver-Bonas-Limited" "shp") ("Pandora-Jewellery-UK-Ltd" "shp") ("Papier" "shp") ("Peggy's-Difference" "shp") ("PlanetArt-Ltd" "shp") ("Pretty-Pastels" "shp") ("Royal-Mail-Group-Ltd" "hse") ("SAINSBURY" "fod") ("Sofology" "shp") ("Sostrene-Grenes" "shp") ("Their-Nibs" "shp") ("melodymaison" "shp") ("AO-Retail-Ltd" "shp") ("Abbott-Lyon" "shp") ("Bellaboo" "shp") ("Devon-wick-Candle-Co\\.-Ltd" "shp") ("Hugo-&-Me-Ltd" "shp") ("Lick-Home-Ltd" "shp") ("Mabel-&-Fox" "shp") ("THE-KID-COLLECTIVE-LTD" "shp") ("TruffleShuffle-Retail-Ltd" "shp") ("UM-Fashion" "shp") ("littledaisydream" "shp") ("Coconut-Lane" "shp") ("Eleanor-Bowmer" "shp") ("Emma-Matratzen" "shp") ("SharkNinja" "shp") ("lookfantastic" "shp") ("cleverbridge" "web") ("Select-Specs" "shp") ("Green-Sheep-Group-Limited" "shp") ("FastSpring-Limited" "shp") ("Hair-Solutions" "har") ("URBN-UK-LIMITED" "shp") ("Semantical-Ltd" "shp") ("United-Arts" "shp") (".*" "o"))))

(with-eval-after-load 'bank-buddy
  (add-hook 'org-mode-hook 'bank-buddy-cat-maybe-enable))

(setq pixel-scroll-precision-mode 1)

(defadvice dired-sort-toggle-or-edit (after dired-sort-move-to-first-file activate)
  "Move point to the first file or directory after sorting, skipping . and .."
  (goto-char (point-min))
  (dired-next-line 2)  ;; Skip past header and move to first entry
  (while (and (not (eobp))
              (looking-at-p ".*\\.\\.?$"))  ;; Check if line is . or ..
    (dired-next-line 1)))

(defun transform-bank-buddy-vars (old-prefix new-prefix &rest var-names)
  "Transform bank-buddy variables to use a new prefix.
OLD-PREFIX is the current prefix (e.g., 'bank-buddy-').
NEW-PREFIX is the new prefix to use (e.g., 'bank-buddy-core-').
VAR-NAMES is a list of variable names to transform."
  (interactive "sEnter old prefix: \nsEnter new prefix: \nXEnter variable names (space-separated): ")
  (let ((count 0))
    (dolist (var var-names)
      (when (string-prefix-p old-prefix var)
        (let* ((from (regexp-quote var))
               (to (concat new-prefix (substring var (length old-prefix))))
               (msg (format "Replacing %s with %s" from to)))
          (message msg)
          (project-do-replace-regexp from to)
          (setq count (1+ count)))))
    (message "Completed %d replacements" count)))

(defun project-do-replace-regexp (from to)
  "Non-interactive version of `project-query-replace-regexp'."
  (let* ((proj (project-current t))
         (dirs (list (project-root proj)))
         (files (project-files proj dirs)))
    (dolist (file files)
      (when (file-readable-p file)
        (with-current-buffer (find-file-noselect file)
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward from nil t)
              (replace-match to))))))))

(defun jr ()
  "Run the transform-bank-buddy-vars function with predefined arguments."
  (interactive)
  (transform-bank-buddy-vars 
   "bank-buddy-"
   "bank-buddy-core-"
   "bank-buddy-cat-list-defines"
   "bank-buddy-category-names"
   "bank-buddy-subscription-patterns"))

(define-key my-jump-keymap (kbd "l") #'consult-theme)

(use-package xkb-mode)

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
  (ollama-buddy-default-model "a:gpt-4.1")
  (ollama-buddy-openai-api-key
   (auth-source-pick-first-password :host "ollama-buddy-openai" :user "apikey"))
  (ollama-buddy-claude-api-key
   (auth-source-pick-first-password :host "ollama-buddy-claude" :user "apikey"))
  (ollama-buddy-gemini-api-key
   (auth-source-pick-first-password :host "ollama-buddy-gemini" :user "apikey"))
  (ollama-buddy-grok-api-key
   (auth-source-pick-first-password :host "ollama-buddy-grok" :user "apikey"))
  :config
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
;; -> LLM
;;
(defvar llm-base-dir "/mnt/hgfs/SharedVM/source/"
  "Name of the base directory")

(defvar llm-source-dir nil
  "Full name of the source directory.")

(defvar package-source-mode 'melpa
  "Current package source mode. Can be 'local or 'melpa.")

(defvar package-source-packages '()
  "List of packages with tracked source configurations.
Each entry is of form (PACKAGE-NAME LOCAL-DIR MELPA-NAME).")

(defvar my-llm-models
  '(
    ("tinyllama" . "latest")
    ("gemma3" . "4b")
    ("llama3.2" . "3b")
    )
  "List of LLM models and their token sizes to configure.")

(defvar my-ollama-host "localhost:11434"
  "Host for the backend.")

;; Make sure the setup-local-package function still exists
(defun setup-local-package (package-dir)
  "Set up a local package directory similar to how MELPA would.
Compile all .el files and generate autoloads."
  (interactive "DPackage directory: ")
  ;; Ensure the directory exists
  (unless (file-directory-p package-dir)
    (error "Directory %s does not exist" package-dir))
  
  ;; Add to load path
  (add-to-list 'load-path package-dir)
  
  ;; Byte-compile all .el files in the directory that don't have
  ;; an up-to-date .elc file
  (dolist (file (directory-files package-dir t "\\.el$"))
    (let ((elc-file (concat file "c")))
      (when (or (not (file-exists-p elc-file))
                (file-newer-than-file-p file elc-file))
        (byte-compile-file file))))
  
  ;; Generate autoloads file
  (let* ((autoload-file
          (expand-file-name (format "%s-autoloads.el" 
                                    (file-name-nondirectory
                                     (directory-file-name package-dir)))
                            package-dir)))
    (setq generated-autoload-file autoload-file)
    (update-directory-autoloads package-dir)
    
    ;; Load the generated autoloads file
    (when (file-exists-p autoload-file)
      (load-file autoload-file))))

(defun package-source-register (package-name local-dir &optional melpa-name)
  "Register a package for source switching.
PACKAGE-NAME is the symbol name of the package.
LOCAL-DIR is the path to the local development directory.
MELPA-NAME is the package name in MELPA, defaults to PACKAGE-NAME."
  (let ((melpa (or melpa-name (symbol-name package-name))))
    (add-to-list 'package-source-packages 
                 (list package-name local-dir melpa))))

;; Function to switch all packages to a specific source
(defun package-source-switch-all (source)
  "Switch all registered packages to SOURCE.
SOURCE can be 'local or 'melpa."
  (interactive (list (intern (completing-read "Switch to source: " 
                                              '(local melpa) nil t))))
  (unless (memq source '(local melpa))
    (error "Invalid source: %s. Must be 'local or 'melpa" source))
  
  (setq package-source-mode source)
  
  ;; First remove all tracked packages from load-path
  (dolist (pkg package-source-packages)
    (let* ((package-name (car pkg))
           (local-dir (nth 1 pkg))
           (melpa-name (nth 2 pkg)))
      ;; Remove from load path if present
      (setq load-path (delete local-dir load-path))
      
      ;; Unload package features if loaded
      (when (featurep package-name)
        (unload-feature package-name t))))
  
  ;; Now set up packages according to selected mode
  (if (eq source 'local)
      (package-source-setup-local)
    (package-source-setup-melpa))
  
  ;; Reload init file to apply changes
  (message "Reloading configuration to apply changes...")
  (load user-init-file))

(defun package-source-setup-local ()
  "Set up all registered packages from local sources."
  (dolist (pkg package-source-packages)
    (let* ((package-name (car pkg))
           (local-dir (nth 1 pkg)))
      (setup-local-package local-dir))))

(defun package-source-setup-melpa ()
  "Set up all registered packages from MELPA."
  (dolist (pkg package-source-packages)
    (let* ((package-name (car pkg))
           (melpa-name (nth 2 pkg)))
      ;; Make sure package.el is initialized
      (require 'package)
      (unless package--initialized
        (package-initialize))
      
      ;; Ensure package is installed
      (unless (package-installed-p (intern melpa-name))
        (package-refresh-contents)
        (package-install (intern melpa-name)))
      
      ;; Load the package
      (require package-name))))

;; Enhanced setup-my-package function that registers the package
(defun setup-my-package (pkg &optional melpa-name)
  "Set up a local package and register it for source switching.
PKG is the package directory name under llm-base-dir.
MELPA-NAME is the package name in MELPA, defaults to PKG."
  (setq llm-source-dir (concat llm-base-dir pkg))
  (package-source-register (intern (file-name-base pkg)) 
                           llm-source-dir 
                           (or melpa-name (file-name-base pkg)))
  (when (eq package-source-mode 'local)
    (setup-local-package llm-source-dir)))

;; Interactive command to toggle between local and MELPA
(defun package-source-toggle ()
  "Toggle between local and MELPA package sources."
  (interactive)
  (if (eq package-source-mode 'local)
      (package-source-switch-all 'melpa)
    (package-source-switch-all 'local))
  (message "Switched to %s package source" package-source-mode))

;; Create separate macros for MELPA and local configurations
(defmacro use-package-local-or-melpa (name &rest args)
  "Set up a package with different configurations based on package-source-mode.
NAME is the package name. 
ARGS are passed to use-package based on the current mode."
  (declare (indent 1))
  `(progn
     ;; Define the local version
     (when (eq package-source-mode 'local)
       (use-package ,name
         :load-path llm-source-dir
         ,@args))
     
     ;; Define the MELPA version
     (when (eq package-source-mode 'melpa)
       (use-package ,name
         :ensure t
         ,@args))))

(setup-my-package "gptel-master" "gptel")
(use-package-local-or-melpa gptel
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

(setup-my-package "plz.el-master" "plz")
(use-package-local-or-melpa plz)

(setup-my-package "plz-media-type-main" "plz-media-type")
(use-package-local-or-melpa plz-media-type)

(setup-my-package "plz-event-source-main" "plz-event-source")
(use-package-local-or-melpa plz-event-source)

(setup-my-package "llm-main" "llm")
(use-package-local-or-melpa llm)

(setup-my-package "ellama-main" "ellama")
(use-package-local-or-melpa ellama
  :init
  (setopt ellama-language "English")
  (require 'llm-ollama)
  (setopt ellama-provider
	      (make-llm-ollama
	       :chat-model "tinyllama:latest"))
  :config
  (setq ellama-sessions-directory "~/.config/emacs/ellama-sessions/"
        ellama-sessions-auto-save t))

(setup-my-package "shell-maker-main" "shell-maker")
(use-package-local-or-melpa shell-maker)

(setup-my-package "chatgpt-shell-main" "chatgpt-shell")
(use-package-local-or-melpa chatgpt-shell
  :after shell-maker
  :custom
  (chatgpt-shell-openai-key
   (lambda ()
     (auth-source-pass-get 'secret "openai-key")))
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
                    (model-version (format "%s:%s" model-name token-size)))
               `((:provider . "Ollama")
                 (:label . ,model-name)
                 (:version . ,model-version)
                 (:short-version . ,token-size)
                 (:token-width . 4)
                 (:context-window . 8192)
                 (:handler . chatgpt-shell-ollama--handle-ollama-command)
                 (:filter . chatgpt-shell-ollama--extract-ollama-response)
                 (:payload . chatgpt-shell-ollama-make-payload)
                 (:url . chatgpt-shell-ollama--make-url))))
           my-llm-models)))
     (append default-models ollama-models))))

(require 'transient)

;;; Main LLM Menu
(transient-define-prefix llm-menu ()
  "LLM Client Selection."
  ["Select LLM Client"
   ("o" "ollama buddy" llm-ollama-buddy-menu)
   ("g" "GPTel" llm-gptel-menu)
   ("c" "ChatGPT Shell" llm-chatgpt-menu)
   ("e" "Ellama" llm-ellama-menu)
   ("t" "Toggle package source (Local/MELPA)" package-source-toggle)])

  ;;; ollama-buddy
(transient-define-prefix llm-ollama-buddy-menu ()
  "ChatGPT Shell."
  ["ChatGPT Shell"
   ("o" "Open chat" ollama-buddy--open-chat)
   ("m" "Swap model" ollama-buddy--swap-model)
   ("q" "Quit" transient-quit-one)])  

;;; GPTel
(transient-define-prefix llm-gptel-menu ()
  "GPTel."
  ["GPTel"
   ("o" "Open chat" gptel)
   ("m" "Menu" gptel-menu)
   ("q" "Quit" transient-quit-one)])
  
;;; ChatGPT Shell Menu
(transient-define-prefix llm-chatgpt-menu ()
  "ChatGPT Shell."
  ["ChatGPT Shell"
   ("o" "Open chat" chatgpt-shell)
   ("m" "Swap model" chatgpt-shell-swap-model)
   ("q" "Quit" transient-quit-one)])

;;; Ellama Menu
(transient-define-prefix llm-ellama-menu ()
  "Ellama."
  ["Ellama"
   ("o" "Open chat" ellama-chat)
   ("m" "Swap model" ellama-select-ollama-model)
   ("q" "Quit" transient-quit-one)])

;; Bind the main transient menu to a global key
(global-set-key (kbd "C-c g") 'llm-menu)

;; Keep your original completion styles for non-minibuffer contexts
;; (setq completion-styles '(flex basic substring))

;;
;; -> other
;;
(defun convert-weight (weight)
  "Convert WEIGHT from string to pounds."
  (let* ((parts (split-string weight ":"))
         (stone (string-to-number (car parts)))
         (pounds (string-to-number (cadr parts))))
    (+ (* stone 14) pounds)))

(defun my-capture-top-level ()
  "Function to capture a new entry at the top level of the given file."
  (goto-char (point-min))
  (or (outline-next-heading)
      (goto-char (point-max)))
  (unless (bolp) (insert "\n")))

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

(use-package cursor-heatmap
  :load-path "~/source/repos/cursor-heatmap"
  :config
  (cursor-heatmap-mode 1)
  :custom
  (cursor-heatmap-grid-width 20)
  (cursor-heatmap-grid-height 20)
  (cursor-heatmap-use-colors t))
