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
  (selected-window-accent-fringe-minimum 10)
  (selected-window-accent-fringe-thickness 10)
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
        ("b" "Blog" plain
         (file+function
          "~/publish/hugo-unified/blog.org"
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

        ("e" "Emacs" plain
         (file+function
          "~/publish//hugo-unified/README.org"
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
          "/publish/hugo-unified/linux.org"
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
          "~/publish/hugo-unified/art.org"
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
          "~/publish/hugo-unified/art.org"
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

(defvar my/org-dired-marked-files nil
  "Stores the current dired marked files.")

(defvar my/gallery-hugo-section nil
  "Stores the HUGO section for the current gallery capture.")

(defun my/org-hugo-new-subtree-post-capture-template ()
  (let* ((default-date (format-time-string "%Y-%m-%d" (current-time)))
         (input-date (read-string (format "Date (%s): " default-date) nil nil default-date))
         (parsed-date (org-read-date nil t input-date))
         (date (format-time-string (org-time-stamp-format :inactive) parsed-date))
         (date-string (format-time-string "%Y-%m-%d" parsed-date))
         (year-string (format-time-string "%Y" parsed-date))
         (timestamp-string (format-time-string "%Y%m%d%H%M%S" parsed-date))
         (title (read-from-minibuffer "Post Title: "))
         (fname (org-hugo-slug title))
         (section (concat "blog/" timestamp-string "-blog--" fname)))
    (setq my/gallery-hugo-section section)
    (mapconcat #'identity
               `(,(concat "* DONE Photos " title " " date-string " :" year-string ":")
                 ":PROPERTIES:"
                 ":EXPORT_FILE_NAME: index"
                 ,(concat ":EXPORT_HUGO_SECTION: " section)
                 ,(concat ":EXPORT_HUGO_LASTMOD: " date)
                 ":EXPORT_HUGO_TYPE: gallery"
                 ,(concat ":EXPORT_HUGO_CUSTOM_FRONT_MATTER+: :thumbnail /" section ".jpg")
                 ":END:"
                 "%?\n")
               "\n")))

(defun my/gallery-after-finalize ()
  "Copy gallery images to Hugo static dir and run PictureCrush."
  (unless org-note-abort
    (let* ((target-dir (concat "~/publish/hugo-unified/static/" my/gallery-hugo-section))
           (thumb (nth (random (length my/org-dired-marked-files)) my/org-dired-marked-files))
           (copied-files '()))
      (make-directory target-dir t)
      (copy-file thumb (concat "~/publish/hugo-unified/static/" my/gallery-hugo-section ".jpg"))
      (dolist (file my/org-dired-marked-files)
        (let ((target-file (expand-file-name (file-name-nondirectory file) target-dir)))
          (copy-file file target-file)
          (push target-file copied-files)))
      (when copied-files
        (async-shell-command (concat "PictureCrush " (mapconcat #'identity copied-files " "))
                             "*convert*")))))

(add-to-list 'org-capture-templates
             '("g" "Gallery" plain
               (file+function
                "~/publish/hugo-unified/blog.org"
                my-capture-top-level)
               (function my/org-hugo-new-subtree-post-capture-template)
               :prepend t :jump-to-captured t :after-finalize my/gallery-after-finalize))

(defun my/create-gallery ()
  "Create a gallery blog post from marked files in dired or image-dired."
  (interactive)
  (let ((files (my/get-files-from-context)))
    (if files
        (progn
          (setq my/org-dired-marked-files files)
          (org-capture nil "g"))
      (message "No files found for gallery"))))

;;
;; -> use-package
;;
(use-package async)
(use-package i3wm-config-mode)
(use-package yaml-mode)

(use-package ox-hugo
  :demand t
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

;;
;; -> keys-navigation
;;

(define-key my-jump-keymap (kbd "k")
            (lambda () (interactive)
              (find-file (concat user-emacs-directory "README.org"))))
(define-key my-jump-keymap (kbd "a")
            (lambda () (interactive)
              (find-file "~/publish/hugo-unified/README.org")))

;;
;; -> visuals
;;
(set-frame-parameter nil 'alpha-background 90)
(add-to-list 'default-frame-alist '(alpha-background . 90))

;;
;; -> linux specific
;;

(when (eq system-type 'gnu/linux)
  (define-key my-jump-keymap (kbd "m") (lambda () (interactive) (find-file "~/DCIM/Camera")))
  (define-key my-jump-keymap (kbd "j") (lambda () (interactive) (find-file "~/DCIM/content/aaa--todo.org")))
  (define-key my-jump-keymap (kbd "n") (lambda () (interactive) (find-file "~/DCIM/Screenshots")))
  (define-key my-jump-keymap (kbd "w") (lambda () (interactive) (find-file "~/DCIM/content/")))
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

;;
;; -> auto-mode-alist
;;
(add-to-list 'auto-mode-alist '("waybar.*/config\\'" . js-json-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.org_archive\\'" . org-mode))
(add-to-list 'auto-mode-alist '("/sway/.*config.*/" . i3wm-config-mode))
(add-to-list 'auto-mode-alist '("/sway/config\\'" . i3wm-config-mode))
(cl-loop for ext in '("\\.gpr$" "\\.ada$" "\\.ads$" "\\.adb$")
         do (add-to-list 'auto-mode-alist (cons ext 'ada-light-mode)))

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

;;
;; -> emacs-30.1
;;
(setq tab-bar-auto-width-max '((120) 20))

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
   '(("katherine\\|james\\|kate" "prs") ("carpet" "hse") ("railw\\|railway\\|train" "trn") ("paypal" "pay") ("electric\\|energy\\|water" "utl") ("racing" "bet") ("pension" "pen") ("savings\\|saver" "sav") ("uber" "txi") ("magazine\\|news" "rdg") ("claude\\|reddit\\|mobile\\|backmarket\\|openai\\|web" "web") ("notemachine\\|withdrawal" "atm") ("finance" "fin") ("youtube\\|netflix" "str") ("card" "crd") ("top-up\\|phone" "phn") ("amaz\\|amz" "amz") ("pets\\|pet" "pet") ("dentist" "dnt") ("residential\\|rent\\|mortgage" "hse") ("lidl" "fod") ("co\\-op" "fod") ("deliveroo\\|just.*eat" "fod") ("PIPINGROCK" "shp") ("ebay\\|apple\\|itunes" "shp") ("law" "law") ("anyvan" "hmv") ("CHANNEL-4" "str") ("GOOGLE-\\*Google-Play" "web") ("NOW-" "str") ("SALISBURY-CAFE-LOCAL" "fod") ("SAVE-THE-PENNIES" "sav") ("SOUTHAMPTON-GENERAL" "fod") ("TO-Evie" "sav") ("WH-Smith-Princess-Anne" "fod") ("SP-WAXMELTSBYNIC" "shp") ("THORTFUL" "shp") ("SCOTTISH-WIDOWS" "pen") ("WM-MORRISONS" "fod") ("H3G-REFERENCE" "phn") ("DOMINO" "fod") ("Prime-Video" "str") ("PRIVILEGE" "utl") ("PCC-COLLECTION" "utl") ("MORRISON" "fod") ("BT-GROUP" "web") ("ANTHROPIC" "web") ("INSURE" "utl")  ("WWW\\.SSE" "utl") ("GAS" "utl") ("GOOGLE-Google-Play" "web") ("GILLETT-COPNOR-RD" "fod") ("TV-LICENCE" "utl") ("SAINSBURYS" "fod") ("OCADO" "fod") ("TESCO" "fod") ("Vinted" "shp") ("PUMPKIN-CAFE" "fod") ("SP-CHAMPO" "shp") ("THE-RANGE" "shp") ("UNIVERSITY-HOSPITA" "fod") ("VIRGIN-MEDIA" "utl") ("GOLDBOUTIQUE" "shp") ("Surveyors" "law") ("Surveyors" "hse") ("INTERFLORA" "shp") ("INSURANCE" "utl") ("LUCINDA-ELLERY" "shp") ("MARKS&SPENCER" "fod") ("SW-PLC-STAKEHOLDE" "pen") ("JUST-MOVE" "hse") ("B&M" "shp") ("PASSPORT-OFFICE" "hse") ("PHARMACY" "shp") ("ONLINE-REDIRECTIONS" "hse") ("SERENATA-FLOWERS" "shp") ("SNAPPER-DESIGN" "shp") ("LOVEFORSLEEP" "shp") ("TJ-WASTE" "hse") ("M-&-S" "fod") ("MARDIN" "fod") ("MOVEWITHUS" "hse") ("STARBUCKS" "fod") ("CD-2515" "shp") ("DEBIT-INTEREST-ARRANGED" "atm") ("ME-GROUP-INTERNATIONAL" "shp") ("COSTA" "fod") ("NYX" "shp") ("NATWEST-BANK-REFERENCE" "hse") ("Streamline" "shp") ("BETHANIE-YEONG" "hse") ("Roofoods" "fod") ("Wayfair" "shp") ("WHSmith" "shp") ("The-Hut" "shp") ("Sky-Betting" "bet") ("NextLtd" "shp") ("NEW-LOOK-RETAILERS" "shp") ("Marks-and-Spencer" "fod") ("DisneyPlus" "str") ("DAZN-LIMITED" "str") ("Astrid-&-Miyu" "shp") ("ASOS\\.COM-Ltd" "shp") ("Cartridge-Tech-Ltd" "shp") ("Dplay-Entertainment-Ltd" "str") ("DeviantArt" "web") ("Dunelm" "shp") ("Asda-Stores" "shp") ("Argos" "shp") ("IKEA-Limited" "shp") ("Lisa-Angel-Limited" "shp") ("Matalan-Retail-Ltd" "shp") ("Royal-Mail-Group-Limited" "utl") ("SCHOTT-PACKAGING" "hse") ("Samsung-Electronics" "shp") ("Boohoo\\.com" "shp") ("Bizzy-Balloons-LLP" "shp") ("BRANDS-IN-BLOOM-LTD" "shp") ("Highland-and-Honey" "shp") ("Homebaked-Limited" "shp") ("Little-Crafts-London-LTD" "shp") ("Lush-Retail-Ltd" "shp") ("Mamas-&-Papas" "shp") ("Mi-Baby" "shp") ("NEOM-Ltd" "shp") ("Oliver-Bonas-Limited" "shp") ("Pandora-Jewellery-UK-Ltd" "shp") ("Papier" "shp") ("Peggy's-Difference" "shp") ("PlanetArt-Ltd" "shp") ("Pretty-Pastels" "shp") ("Royal-Mail-Group-Ltd" "hse") ("SAINSBURY" "fod") ("Sofology" "shp") ("Sostrene-Grenes" "shp") ("Their-Nibs" "shp") ("melodymaison" "shp") ("AO-Retail-Ltd" "shp") ("Abbott-Lyon" "shp") ("Bellaboo" "shp") ("Devon-wick-Candle-Co\\.-Ltd" "shp") ("Hugo-&-Me-Ltd" "shp") ("Lick-Home-Ltd" "shp") ("Mabel-&-Fox" "shp") ("THE-KID-COLLECTIVE-LTD" "shp") ("TruffleShuffle-Retail-Ltd" "shp") ("UM-Fashion" "shp") ("littledaisydream" "shp") ("Coconut-Lane" "shp") ("Eleanor-Bowmer" "shp") ("Emma-Matratzen" "shp") ("SharkNinja" "shp") ("lookfantastic" "shp") ("cleverbridge" "web") ("Select-Specs" "shp") ("Green-Sheep-Group-Limited" "shp") ("FastSpring-Limited" "shp") ("Hair-Solutions" "har") ("URBN-UK-LIMITED" "shp") ("Semantical-Ltd" "shp") ("United-Arts" "shp") (".*" "o"))))

(with-eval-after-load 'bank-buddy
  (add-hook 'org-mode-hook 'bank-buddy-cat-maybe-enable))

;;
;; -> ollama-buddy
;;

(use-package ollama-buddy
  :load-path "~/source/repos/ollama-buddy"
  :demand t
  :bind
  ("C-c o" . ollama-buddy-role-transient-menu)
  ("C-c O" . ollama-buddy-transient-menu)
  :config
  ;; overall default model
  (setq ollama-buddy-default-model "deepseek-v3.1:671b-cloud")

  (require 'ollama-buddy-annotate nil t)
  
  ;; acp2ollama tesitng
  ;; (setq ollama-buddy-port 12345)
  ;; (setq ollama-buddy-host "localhost")
  
  ;; cloud / web-search keys (not provider-managed)
  (setq ollama-buddy-cloud-api-key
        (auth-source-pick-first-password :host "ollama-buddy-cloud" :user "apikey"))
  (setq ollama-buddy-web-search-api-key
        (auth-source-pick-first-password :host "ollama-buddy-web-search" :user "apikey"))
  (setq ollama-buddy-cloud-session-token "YWdlLWVuY3J5cHRpb24ub3JnL3YxCi0-IFgyNTUxOSBnVXZ6cFN5NFREWE5jOTgwenNnRlNpTzRKZkR0MVdkTkJRRnA5LytCVVJRCmt2NXpOcVkxSEVZMFloUitGdFNlcmN6Tmg2eVlhNUE1cHQ1cFk3SWRjMG8KLS0tIDhxSmJHR293ci9hMWJRUHZQRlpETmgxS1QySFdQK3JLNWVQUGVqRjY5WmcKStpAGR0Zb4FGLsfhJFv_4Ud_jcuWyJsUdaiMr6Cq-RGokYe4U_EvJjwJEDbuM3kRiIpJYv3cwms-FGvwhhPsnckty5SBuAE4FDWN1Wx0ngFmk9ynTXPjmYVMYfaZjurlDL52zFuzAz_O5CvMd_CFcdPgSc07RgVwXiru4dre7-CSFuaAajB6OobXVQ==")

  (setq ollama-buddy-max-history-length 999)
  
  ;; ;; Generic provider registration (replaces individual require files)
  ;; (require 'ollama-buddy-provider)

  ;; (ollama-buddy-provider-create
  ;;  :name "LM Studio"
  ;;  :prefix "l:"
  ;;  :endpoint "http://localhost:1234/v1/chat/completions"
  ;;  :models-endpoint "http://localhost:1234/v1/models")
  
  ;; (ollama-buddy-provider-create
  ;;  :name "OpenAI" :prefix "a:"
  ;;  :api-key (lambda () (auth-source-pick-first-password
  ;;                       :host "ollama-buddy-openai" :user "apikey"))
  ;;  :endpoint "https://api.openai.com/v1/chat/completions"
  ;;  :models-endpoint "https://api.openai.com/v1/models"
  ;;  :models-filter (lambda (id) (string-match-p "\\(gpt\\|o[0-9]\\)" id)))

  ;; (ollama-buddy-provider-create
  ;;  :name "Claude" :prefix "c:" :api-type 'claude
  ;;  :api-key (lambda () (auth-source-pick-first-password
  ;;                       :host "ollama-buddy-claude" :user "apikey"))
  ;;  :endpoint "https://api.anthropic.com/v1/messages"
  ;;  :models-endpoint "https://api.anthropic.com/v1/models")

  ;; (ollama-buddy-provider-create
  ;;  :name "Gemini" :prefix "g:" :api-type 'gemini
  ;;  :api-key (lambda () (auth-source-pick-first-password
  ;;                       :host "ollama-buddy-gemini" :user "apikey"))
  ;;  :endpoint "https://generativelanguage.googleapis.com/v1beta/models/%s:generateContent"
  ;;  :models-endpoint "https://generativelanguage.googleapis.com/v1/models"
  ;;  :models-filter (lambda (id) (string-match-p "gemini" id)))

  ;; (ollama-buddy-provider-create
  ;;  :name "Grok" :prefix "k:"
  ;;  :api-key (lambda () (auth-source-pick-first-password
  ;;                       :host "ollama-buddy-grok" :user "apikey"))
  ;;  :endpoint "https://api.x.ai/v1/chat/completions"
  ;;  :models-endpoint "https://api.x.ai/v1/models")

  ;; (ollama-buddy-provider-create
  ;;  :name "Codestral" :prefix "s:"
  ;;  :api-key (lambda () (auth-source-pick-first-password
  ;;                       :host "ollama-buddy-codestral" :user "apikey"))
  ;;  :endpoint "https://api.mistral.ai/v1/chat/completions"
  ;;  :models '("codestral-latest"))

  ;; (ollama-buddy-provider-create
  ;;  :name "OpenRouter" :prefix "r:"
  ;;  :api-key (lambda () (auth-source-pick-first-password
  ;;                       :host "ollama-buddy-openrouter" :user "apikey"))
  ;;  :endpoint "https://openrouter.ai/api/v1/chat/completions"
  ;;  :models-endpoint "https://openrouter.ai/api/v1/models"
  ;;  :extra-headers '(("HTTP-Referer" . "https://github.com/captainflasmr/ollama-buddy")
  ;;                   ("X-Title" . "ollama-buddy")))

  ;; (ollama-buddy-provider-create
  ;;  :name "DeepSeek" :prefix "d:"
  ;;  :api-key (lambda () (auth-source-pick-first-password
  ;;                       :host "ollama-buddy-deepseek" :user "apikey"))
  ;;  :endpoint "https://api.deepseek.com/chat/completions"
  ;;  :models '("deepseek-chat" "deepseek-reasoner"))

  (require 'ollama-buddy-completion)

  (setq ollama-buddy-completion-model "qwen3-coder-next:cloud")

  ;; setup default custom menu for preferred models
  (ollama-buddy-update-menu-entry 'refactor-code     :model "minimax-m2.1:cloud")
  (ollama-buddy-update-menu-entry 'git-commit        :model "glm-4.7:cloud")
  (ollama-buddy-update-menu-entry 'describe-code     :model "minimax-m2.1:cloud")
  (ollama-buddy-update-menu-entry 'dictionary-lookup :model "minimax-m2.1:cloud")
  (ollama-buddy-update-menu-entry 'synonym           :model "minimax-m2.1:cloud")
  (ollama-buddy-update-menu-entry 'proofread         :model "minimax-m2.1:cloud")

  ;; dired integration
  (with-eval-after-load 'dired
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
  :demand t
  :load-path "~/source/repos/simply-annotate"
  :hook (find-file-hook . simply-annotate-mode)
  :config
  (global-set-key (kbd "M-s") simply-annotate-command-map)
  (setq simply-annotate-inline-position 'above)
  (setq simply-annotate-tint-amount 20)
  
  ;; ;; Heavy L-bracket
  (setq simply-annotate-inline-pointer-above "┗━▶")
  (setq simply-annotate-inline-pointer-after "┏━▶")
  ;; (setq simply-annotate-inline-pointer-after "▲")
  ;; (setq simply-annotate-inline-pointer-above "▼")
  (setq simply-annotate-author-list '("John Doe" "Jane Smith" "James Dyer"))
  (setq simply-annotate-prompt-for-author 'threads-only)  ; Prompt only for replies
  (setq simply-annotate-database-strategy 'both)
  ;; (setq simply-annotate-display-style '(bracket))
  (setq simply-annotate-inline-default t))

(with-eval-after-load 'simply-annotate
  (add-hook 'dired-mode-hook #'simply-annotate-dired-mode))

(tiny-diminish 'cursor-heatmap-mode)
;; (tiny-diminish 'simply-annotate-mode)
(tiny-diminish 'simple-autosuggest-mode)
(tiny-diminish 'org-indent-mode)

(use-package meal-planner
  :load-path "~/source/repos/meal-planner")

(use-package org-social
  :config
  (setq org-social-file "https://host.org-social.org/vfile?token=31841933dc6ee63665fdd874eb60088a21b1ef066939f2b73dea8b51d8ce268d&ts=1764570277&sig=c7a678e4123f8ae5608de3d527fb507029325bce560556cb6465732f481e6de6")
  (setq org-social-relay "https://relay.org-social.org/")
  (setq org-social-my-public-url "https://host.org-social.org/captainflasmr/social.org"))

(use-package dired-video-thumbnail
  :load-path "/home/jdyer/source/repos/dired-video-thumbnail"
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

(my/sync-ui-accent-color "orange")

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

(my/ztree-remap-faces)

(defun insert-default-background-color ()
  "Insert the default background color at point."
  (interactive)
  (insert (downcase (face-attribute 'default :background))))

(global-set-key (kbd "M-s b") ' insert-default-background-color)

(use-package web-mode
  :mode "\\.cshtml?\\'"
  :hook (html-mode . web-mode)
  :bind (:map web-mode-map ("M-;" . nil)))

(add-to-list 'auto-mode-alist '("\\.cshtml\\'" . html-mode))

(use-package ibuffer
  :bind (:map ibuffer-mode-map ("M-o" . nil)))

(setq recentf-max-menu-items 40)
(setq recentf-max-saved-items 40)

(setq max-mini-window-height 6)

;; (use-package dape
;;   :init
;;   ;; Set key prefix BEFORE loading dape
;;   (setq dape-key-prefix (kbd "C-c d"))
;;   :config
;;   ;; Define common configuration
;;   (defvar mimesis-netcoredbg-path "d:/source/emacs-30.1/bin/netcoredbg/netcoredbg.exe"
;;     "Path to netcoredbg executable.")
;;   (defvar mimesis-netcoredbg-log "d:/source/emacs-30.1/bin/netcoredbg/netcoredbg.log"
;;     "Path to netcoredbg log file.")
;;   (defvar mimesis-project-root "d:/source/MIMESIS-OVC"
;;     "Root directory of MIMESIS-OVC project.")
;;   (defvar mimesis-build-config "Debug"
;;     "Build configuration (Debug or Release).")
;;   (defvar mimesis-target-arch "x64"
;;     "Target architecture (x64, x86, or AnyCPU).")
;;   (defvar mimesis-vsdbg-path "C:/Users/vm.j.dyer/.vscode/extensions/ms-dotnettools.csharp-2.80.16-win32-x64/.debugger/x86_64/vsdbg.exe"
;;     "Path to vsdbg from VSCode installation.")

;;   ;; Helper function to create component configs
;;   (defun mimesis-dape-config (component-name dll-name &optional stop-at-entry)
;;     "Create a dape configuration for a component.
;; COMPONENT-NAME is the component directory name
;; DLL-NAME is the DLL filename without extension.
;; STOP-AT-ENTRY if non-nil, stops at program entry point."
;;     (let* ((component-dir (format "%s/%s" mimesis-project-root component-name))
;;            (bin-path (format "%s/bin/%s/%s/net9.0"
;;                              component-dir
;;                              mimesis-target-arch
;;                              mimesis-build-config))
;;            (dll-path (format "%s/%s.dll" bin-path dll-name))
;;            (config-name (intern (format "netcoredbg-launch-%s" 
;;                                         (downcase component-name)))))
;;       `(,config-name
;;         modes (csharp-mode csharp-ts-mode)
;;         command ,mimesis-netcoredbg-path
;;         command-args (,(format "--interpreter=vscode")
;;                       ,(format "--engineLogging=%s" mimesis-netcoredbg-log))
;;         normalize-path-separator 'windows
;;         :type "coreclr"
;;         :request "launch"
;;         :program ,dll-path
;;         :cwd ,component-dir
;;         :console "externalTerminal"
;;         :internalConsoleOptions "neverOpen"
;;         :suppressJITOptimizations t
;;         :requireExactSource nil
;;         :justMyCode t
;;         :stopAtEntry ,(if stop-at-entry t :json-false))))

;;   ;; Register all component configurations
;;   (dolist (config (list
;;                    (mimesis-dape-config "IGC" "MIMESIS.IGC" t)
;;                    (mimesis-dape-config "MSS" "MIMESIS.MSS" t)
;;                    (mimesis-dape-config "IGM" "MIMESIS.IGM" t)
;;                    (mimesis-dape-config "VDS" "VDS.MSS" t)
;;                    (mimesis-dape-config "DM" "DM.MSS" t)
;;                    (mimesis-dape-config "Demo" "Demo.MSS" t)
;;                    (mimesis-dape-config "Test_001" "Test" t)))
;;     (add-to-list 'dape-configs config))

;;   ;; Set buffer arrangement and other options
;;   (setq dape-buffer-window-arrangement 'gud)
;;   (setq dape-debug t)
;;   (setq dape-repl-echo-shell-output t))

(use-package dired-image-thumbnail
  :load-path "/home/jdyer/.emacs.d/offline-packages/local-packages/dired-image-thumbnail"
  :demand t
  :config
  (setq dired-image-thumbnail-auto-accept t)
  (setq dired-image-thumbnail-sort-by 'date)
  (setq dired-image-thumbnail-sort-order 'descending)
  (setq dired-image-thumbnail-window-layout 'left-right)
  (setq dired-image-thumbnail-window-ratio 0.6)
  :bind
  (:map dired-mode-map
        ("C-t d" . dired-image-thumbnail)  ; m for modern/enhanced
        ("C-t s" . dired-image-thumbnail-insert-image-subdirs)  ; s for smart subdirs
        ("C-t z" . dired-image-thumbnail-insert-subdir-recursive)  ; z for all (last letter)
        ("C-t k" . dired-image-thumbnail-kill-all-subdirs)))  ; k for kill

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(misterioso))
 '(package-selected-packages
   '(agent-shell async csv-mode dape diff-hl diff-hl-mode doom-themes
                 ef-themes elpa-mirror gruvbox-theme i3wm-config-mode
                 org-social ox-hugo package-lint ready-player
                 timu-caribbean-theme timu-rouge-theme
                 timu-spacegrey-theme web-mode yaml-mode ztree))
 '(warning-suppress-log-types '((frameset)))
 '(warning-suppress-types '((frameset))))

;;
;; -> typescript-support
;;

(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

(use-package agent-shell
  :ensure t
  :config
  ;; Use login-based auth (uses your Claude Code Pro subscription)
  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :login t))

  ;; Inherit environment from Emacs (picks up PATH etc.)
  (setq agent-shell-anthropic-claude-environment
        (agent-shell-make-environment-variables :inherit-env t))

  ;; Make Claude Code the default agent
  (setq agent-shell-preferred-agent-config
        (agent-shell-anthropic-make-claude-code-config))

  ;; Show usage info at end of each turn
  (setq agent-shell-show-usage-at-turn-end t)

  (setq agent-shell-prefer-session-resume t)
  (setq agent-shell-show-session-id t)
  (setq agent-shell-session-strategy 'prompt)

  :bind (:map agent-shell-mode-map
              ;; Match ollama-buddy: RET for newline, C-c RET to send
              ("RET"     . newline)
              ("C-c m" . agent-shell-set-session-model)
              ("C-c RET" . shell-maker-submit)
              ("C-c C-c" . shell-maker-submit)          ; ollama-buddy also uses C-c C-c
              ("C-c C-k" . agent-shell-interrupt)        ; same as ollama-buddy cancel
              ;; History navigation (same as ollama-buddy M-p/M-n)
              ("M-p"     . comint-previous-input)
              ("M-n"     . comint-next-input)))

;;
;; -> qvm
;;
(use-package qemu-manager
  :load-path "/home/jdyer/.emacs.d/offline-packages/local-packages/qemu-manager"
  :bind ("C-c q" . qemu-manager-list))

(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs
               `(java-mode . ("/home/jdyer/.emacs.d/bin/jdtls/bin/jdtls"
                              :initializationOptions
                              (:bundles ["/home/jdyer/.emacs.d/bin/jdtls/com.microsoft.java.debug.plugin-0.53.2.jar"]))))

  ;; Disable resource-intensive features for the target system
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
          )))

;; Configure Eglot to use the TypeScript server for web-mode buffers.
;; We add this to eglot-server-programs so Eglot knows what command to run.
;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                '(web-mode . ("typescript-language-server" "--stdio")))
;;   (add-to-list 'eglot-server-programs
;;                `(java-mode . ("/home/jdyer/.emacs.d/bin/jdtls/bin/jdtls"))))

(defun dape--jdtls-start-debug-session (config)
  "Resolve classpath via JDTLS, start a debug session, and configure dape."
  (let ((server (eglot-current-server)))
    (unless server
      (user-error "Eglot is not active, run M-x eglot first"))
    (let* ((main-class (plist-get config :mainClass))
           (project-name (plist-get config :projectName))
           ;; Ask JDTLS to resolve classpath for the main class
           (classpath-result (eglot-execute-command
                              server "vscode.java.resolveClasspath"
                              (vector main-class project-name)))
           ;; Keep as vectors so they serialize to JSON arrays (not null)
           (module-paths (aref classpath-result 0))
           (class-paths (aref classpath-result 1))
           ;; Start the debug adapter session inside JDTLS
           (port (eglot-execute-command
                  server "vscode.java.startDebugSession" nil)))
      (setq config (plist-put config 'port port))
      (setq config (plist-put config 'host "localhost"))
      (setq config (plist-put config :modulePaths module-paths))
      (setq config (plist-put config :classPaths class-paths))
      config)))

(use-package dape
  :ensure t
  :bind
  (("<f5>"    . dape)                       ; Start/continue debugging
   ("<S-f5>"  . dape-kill)                  ; Stop debugging
   ("<f9>"    . dape-breakpoint-toggle)     ; Toggle breakpoint
   ("<f10>"   . dape-next)                  ; Step over
   ("<f11>"   . dape-step-in)              ; Step into
   ("<S-f11>" . dape-step-out)             ; Step out
   ("<C-f5>"  . dape-restart))            ; Restart session
  :config
  ;; Java VM startup can be slow; increase from the default 10 seconds
  (setq dape-request-timeout 30)

  ;; FedProClient: HLA 4 Chat sample
  (add-to-list 'dape-configs
               '(fedpro-sample
                 modes (java-mode java-ts-mode)
                 fn dape--jdtls-start-debug-session
                 :type "java"
                 :request "launch"
                 :mainClass "se.pitch.oss.chat1516_4.Chat"
                 :projectName "sample"
                 :cwd "/home/jdyer/source/FedProClient-main/java"
                 :console "internalConsole"
                 :stopAtEntry t))

  ;; FedProClient: HLA Evolved Chat sample
  (add-to-list 'dape-configs
               '(fedpro-sample-evolved
                 modes (java-mode java-ts-mode)
                 fn dape--jdtls-start-debug-session
                 :type "java"
                 :request "launch"
                 :mainClass "se.pitch.oss.chat1516e.Chat"
                 :projectName "sample_evolved"
                 :cwd "/home/jdyer/source/FedProClient-main/java"
                 :console "internalConsole"
                 :stopAtEntry t))

  ;; FedProClient: CUIS Server
  (add-to-list 'dape-configs
               '(fedpro-cuis-server
                 modes (java-mode java-ts-mode)
                 fn dape--jdtls-start-debug-session
                 :type "java"
                 :request "launch"
                 :mainClass "se.pitch.oss.fedpro.cuis.server.CuisMain"
                 :projectName "cuis-server"
                 :cwd "/home/jdyer/source/FedProClient-main/java"
                 :console "internalConsole"
                 :stopAtEntry t))

  ;; FedProClient: CUIS Demo/Test
  (add-to-list 'dape-configs
               '(fedpro-cuis-demo
                 modes (java-mode java-ts-mode)
                 fn dape--jdtls-start-debug-session
                 :type "java"
                 :request "launch"
                 :mainClass "se.pitch.oss.fedpro.cuis.test.demo.CuisDemo"
                 :projectName "cuis-test"
                 :cwd "/home/jdyer/source/FedProClient-main/java"
                 :console "internalConsole"
                 :stopAtEntry t))

  ;; FedProClient: Attach to Gradle --debug-jvm (port 5005)
  (add-to-list 'dape-configs
               '(fedpro-attach
                 modes (java-mode java-ts-mode)
                 host "localhost"
                 port 5005
                 :type "java"
                 :request "attach"
                 :hostName "localhost"
                 :port 5005)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Monospace" :foundry "ADBO" :slant normal :weight regular :height 100 :width normal))))
 '(completions-common-part ((t (:foreground "#87ceeb"))))
 '(completions-first-difference ((t (:foreground "#ffb6c1"))))
 '(cursor ((t (:background "coral"))))
 '(eca-chat-context-cursor-face ((t (:foreground "gainsboro" :underline t :height 1.0))) t)
 '(ediff-current-diff-A ((t (:extend t :background "#b5daeb" :foreground "#000000"))))
 '(ediff-even-diff-A ((t (:background "#bafbba" :foreground "#000000" :extend t))))
 '(ediff-fine-diff-A ((t (:background "#f4bd92" :foreground "#000000" :extend t))))
 '(ediff-odd-diff-A ((t (:background "#b8fbb8" :foreground "#000000" :extend t))))
 '(fixed-pitch ((t (:family "Monospace" :height 110))))
 '(font-lock-warning-face ((t (:foreground "#930000" :inverse-video nil))))
 '(fringe ((t (:foreground "#2d3743" :background "#2d3743"))))
 '(hl-line ((t (:background "#3636424250b6"))))
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

(load-theme 'doom-old-hope t)

(use-package dwell-map
  :load-path "/home/jdyer/.emacs.d/offline-packages/local-packages/dwell-map")

 (dwell-map-mode 1)

(use-package diff-hl
  :ensure t
  :hook (dired-mode . diff-hl-dired-mode)
  :config
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode 1)
  (unless (display-graphic-p)
    (diff-hl-margin-mode 1)))

(repeat-mode 1)
