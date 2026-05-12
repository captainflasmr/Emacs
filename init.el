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
          "~/DCIM/content/blog.org"
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
          "~/DCIM/content/emacs.org"
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
          "~/DCIM/content/linux.org"
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
          "~/DCIM/content/art.org"
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
          "~/DCIM/content/art.org"
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
;; The "g" (Gallery) capture template is now in transmute.el

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

;;
;; -> keys-navigation
;;

(define-key my-jump-keymap (kbd "k")
            (lambda () (interactive)
              (find-file (concat user-emacs-directory "README.org"))))
(define-key my-jump-keymap (kbd "a")
            (lambda () (interactive)
              (find-file "~/DCIM/content/emacs.org")))

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
;; -> custom-settings
;;

;;
;; -> emacs-30.1
;;

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

  ;;
  ;; Opencode
  ;;
  (require 'ollama-buddy-opencode)
  ;; ~/.authinfo:  machine ollama-buddy-opencode login apikey password <KEY>
  (setq ollama-buddy-opencode-api-key
        (auth-source-pick-first-password
         :host "opencode-go" :user "apikey"))
  (setq ollama-buddy-opencode-usage-url 
        "https://opencode.ai/workspace/wrk_01KQ0AFJ2GSES8D7J3CRMAW8B8/go")
  (setq ollama-buddy-opencode-session-token 
        "Fe26.2**314c635ffa48dbd4f81553efa5605ac419b102dbfe621e5902efd5bbda9ea426*jdixt0qMDKLPrmLzKTrEnA*ZB9JeXsxqujAs2NJlp52zshH-0BFZZx-eGuvC5dceCKDI77bMItxiyug9gyuOdFuKF44AvyYyJwYLYRybD1eXZXsCDWHmI_nNz0pV0Bdtbay0YWryinJUZSvmUastDNe-t15b8xLBnVzpb-g3ugkyz1vIqTM9-YonQ9r6mV0BgwRLeCcJvHrdc0XkJ1sEqjWYTPIEvGRAfub0o0XUDkwUm0NoF_oAewYGOm02m4y9-g_ZAaxpug_8IIqOKLVoyaTaXx3eKsIyNjrRzkjFUeWX7DUZF25mtuwglJ-ZthyId50bFb0BiVI75LE6W3OS1kPQ6VlMqxYmI6_gJ93hla4Qw*1808589715821*7f39437238687e955f9eb5a1922dff7d499de599ae629d53608c8d45c555379c*6ymCS_3rePNS61-jiI3cJ4BCUKhGNNQYbv75Z697v4k")
  
  (require 'ollama-buddy-annotate nil t)
  
  ;; acp2ollama tesitng
  ;; (setq ollama-buddy-port 12345)
  ;; (setq ollama-buddy-host "localhost")
  
  ;; cloud / web-search keys (not provider-managed)
  (setq ollama-buddy-cloud-api-key
        (auth-source-pick-first-password :host "ollama-buddy-cloud" :user "apikey"))
  (setq ollama-buddy-web-search-api-key
        (auth-source-pick-first-password :host "ollama-buddy-web-search" :user "apikey"))
  (setq ollama-buddy-cloud-session-token "YWdlLWVuY3J5cHRpb24ub3JnL3YxCi0-IFgyNTUxOSBqcS9mTW45SEtVS050KzAwNHpxMWFWL0tWR1F1RVc3Q1QrekgrcmQvN2c0ClA3bWhhMER1MXlXYVVrb2trQnJOTUNQZkdWYTAxd1dvZTN1bGlWQXBBVUUKLS0tIGRNTDNQSGVxWUdJZXlmMjE0UHAwVVdGRnJCRzNzeHZFMUtKd05lSEN3VzgKnQ2wx2bYbVHYNrtHMs_3U2WRtYdWOsgpg8-cblC4qwaNgzlJnwPpQ7LRyD92HX9JtH7cQIOCeHwu43y38X17busj8g5JHnGjOVczzNeR65w6VBRsDf_ol65GO_K2UTzZqNh0Bpgxv0RtuhW7mSUMXTOsmPqg2YtuyiZjfdC_tTtfG7Oli__1Av9r4A==")

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

;; ;;
;; ;; -> dashboard
;; ;;
;; (use-package dashboard
;;   :ensure t
;;   :init
;;   (setq dashboard-set-heading-icons nil)
;;   (setq dashboard-set-file-icons nil)
;;   (setq dashboard-set-navigator-icons nil)

;;   (dashboard-setup-startup-hook)
;;   (setq dashboard-banner-logo-title nil)
;;   (setq dashboard-startup-banner 'ascii)
;;   (setq dashboard-center-content nil)
;;   (setq dashboard-show-shortcuts t)

;;   ;; Ensure agenda picks up the todo file
;;   (setq dashboard-match-agenda-entry "TODO=\"TODO\"|TODO=\"NEXT\"")
;;   ;; (setq org-agenda-files '("~/DCIM/content/aaa--todo.org")) ; Use global setting instead


;;   (setq dashboard-footer-messages '("Ready to hack." "The world is your terminal." "Emacs is the way."))

;;   (defun dashboard-insert-my-jumps (list-size)
;;     (dashboard-insert-section
;;      "QUICK JUMPS:"
;;      '("TODO List" "Content Hub" "Screenshots" "Camera")
;;      list-size
;;      'my-jumps
;;      "l"
;;      (lambda (w &rest _)
;;        (let ((el (widget-get w :tag)))
;;          (cond
;;           ((string= el "TODO List") (find-file "~/DCIM/content/aaa--todo.org"))
;;           ((string= el "Content Hub") (find-file "~/DCIM/content/"))
;;           ((string= el "Screenshots") (find-file "~/DCIM/Screenshots"))
;;           ((string= el "Camera") (find-file "~/DCIM/Camera")))))
;;      el))

;;   ;; Define custom sections for your tools
;;   (defun dashboard-insert-my-tools (list-size)
;;     (dashboard-insert-section
;;      "CUSTOM TOOLS:"
;;      '("Launch AI Agents" "Simply Annotate")
;;      list-size
;;      'my-tools
;;      "o"
;;      (lambda (w &rest _)
;;        (let ((el (widget-get w :tag)))
;;          (cond
;;           ((string= el "Launch AI Agents") (ollama-buddy-transient-menu))
;;           ((string= el "Simply Annotate") (simply-annotate-list-projects)))))
;;      el))

;;   (add-to-list 'dashboard-item-generators '(my-tools . dashboard-insert-my-tools))
;;   (add-to-list 'dashboard-item-generators '(my-jumps . dashboard-insert-my-jumps))

;;   ;; Advise dashboard--current-section to recognize custom headings
;;   (defun my/dashboard-current-section ()
;;     "Try to detect custom dashboard sections before falling back to original."
;;     (save-excursion
;;       (let ((sep (dashboard--separator)))
;;         (when (and (search-backward sep nil t)
;;                    (search-forward sep nil t))
;;           (let ((ln (thing-at-point 'line t)))
;;             (cond ((string-match-p "QUICK JUMPS:" ln) 'my-jumps)
;;                   ((string-match-p "CUSTOM TOOLS:" ln) 'my-tools)
;;                   (t nil)))))))

;;   (advice-add 'dashboard--current-section :before-until #'my/dashboard-current-section)

;;   (with-eval-after-load 'dashboard
;;     (define-key dashboard-mode-map (kbd "C-j") #'dashboard-return)
;;     (define-key dashboard-mode-map (kbd "f") #'dashboard-return))

;;   (setq dashboard-items '((my-jumps . 5)
;;                           (my-tools . 5)
;;                           (recents  . 10)
;;                           (bookmarks . 10)
;;                           (projects . 5)))

;;   (dashboard-refresh-buffer))

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
  ;; (setq simply-annotate-inline-default t)
  )

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
   '(agent-shell async csv-mode dape demap diff-hl dired-sidebar
                 doom-themes dumb-jump ef-themes elpa-mirror
                 gruvbox-theme htmlize i3wm-config-mode kotlin-mode
                 org-social ox-hugo package-lint protobuf-mode
                 timu-caribbean-theme timu-rouge-theme
                 timu-spacegrey-theme treemacs web-mode yaml-mode
                 ztree))
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

  ;; Make OpenCode the default agent
  (require 'agent-shell-opencode)
  (setq agent-shell-opencode-authentication
        (agent-shell-opencode-make-authentication :none t))
  (setq agent-shell-preferred-agent-config
        (agent-shell-opencode-make-agent-config))

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

(use-package dumb-jump
  :ensure t
  :after xref
  :custom
  (dumb-jump-force-searcher 'rg)
  (dumb-jump-prefer-searcher 'rg)
  (dumb-jump-selector 'completing-read)
  :config
  ;; Prepend globally so dumb-jump beats the default etags backend.
  ;; Eglot (when active) registers buffer-locally and still runs first,
  ;; since buffer-local hook functions precede global ones.
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; xref show-definitions-function is set in Emacs-vanilla/xref-core
;; (memoization + live-preview wrapper); applies to all backends.


;; Ada support for dumb-jump (not built-in).
;; Case-sensitive: works when the codebase is consistent about casing.
(with-eval-after-load 'dumb-jump
  (dolist (ext '("ads" "adb" "ada"))
    (add-to-list 'dumb-jump-language-file-exts
                 `(:language "ada" :ext ,ext :agtype nil :rgtype nil)))

  (add-to-list 'dumb-jump-find-rules
               '(:type "function" :supports ("rg" "ag" "grep" "git-grep")
                       :language "ada"
                       :regex "\\s*(procedure|function)\\s+JJJ\\b"))

  (add-to-list 'dumb-jump-find-rules
               '(:type "type" :supports ("rg" "ag" "grep" "git-grep")
                       :language "ada"
                       :regex "\\s*(type|subtype)\\s+JJJ\\b"))

  (add-to-list 'dumb-jump-find-rules
               '(:type "module" :supports ("rg" "ag" "grep" "git-grep")
                       :language "ada"
                       :regex "\\s*package(\\s+body)?\\s+JJJ\\b"))

  (add-to-list 'dumb-jump-find-rules
               '(:type "variable" :supports ("rg" "ag" "grep" "git-grep")
                       :language "ada"
                       :regex "\\s*JJJ\\s*:\\s*(constant\\s+)?[A-Za-z_][A-Za-z0-9_.]*")))

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

(load-theme 'doom-old-hope t)

(use-package dwell-map
  :load-path "/home/jdyer/.emacs.d/offline-packages/local-packages/dwell-map")

(dwell-map-mode 1)

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode 1)
  (unless (display-graphic-p)
    (diff-hl-margin-mode 1)))

(use-package protobuf-mode
  :mode "\\.proto\\'")

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(protobuf-mode . ("buf" "lsp" "serve"))))

(use-package eglot
  :ensure nil ; Built-in
  :config
  (add-to-list 'eglot-server-programs
               '(kotlin-mode . ("kotlin-language-server"))))

(use-package kotlin-mode
  :mode "\\.kts\\'")

(defun my/treemacs-toggle-current-project ()
  "Show treemacs with the current project only, or hide it if visible.
On open, keep focus in the original window."
  (interactive)
  (let ((win (treemacs-get-local-window)))
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
        treemacs-text-scale -0.5
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

(advice-add 'treemacs--follow :after #'my/treemacs-update-current-line-after-follow)

(add-hook 'treemacs-mode-hook
          (lambda ()
            (hl-line-mode -1)
            (add-hook 'post-command-hook
                      #'my/treemacs-update-current-line nil t)
            (my/treemacs-update-current-line)))

(load (expand-file-name "obp-config" user-emacs-directory))

(dolist (entry org-bootstrap-publish-sites)
  (let ((name (car entry)))
    (defalias (intern (format "my/obp-serve-%s" name))
      (lambda ()
        (interactive)
        (org-bootstrap-publish-serve-site name))
      (format "Stop any running obp server, switch to the `%s' site, then serve it."
              name))
    (defalias (intern (format "my/obp-preview-%s" name))
      (lambda ()
        (interactive)
        (org-bootstrap-publish-serve-site-preview name))
      (format "Stop any running obp server, switch to the `%s' site, then preview-serve it."
              name))))

(unless noninteractive
  (require 'transient)

  (transient-define-prefix my/obp-menu ()
    "org-bootstrap-publish — site selector, serve, deploy."
    [[:description (lambda () (format "Active site: %s"
                                      (or org-bootstrap-publish-cloudflare-project
                                          org-bootstrap-publish-site-title
                                          "none")))
                   ("d" "dyerdwelling" my/obp-serve-dyerdwelling)
                   ("a" "art"          my/obp-serve-art)
                   ("k" "katieboo85"   my/obp-serve-katieboo85)
                   ("e" "emacs"        my/obp-serve-emacs)
                   ("S" "stop server"  org-bootstrap-publish-stop)]
     [:description "Preview"
                   ("M-d" "dyerdwelling" my/obp-preview-dyerdwelling)
                   ("M-a" "art"          my/obp-preview-art)
                   ("M-k" "katieboo85"   my/obp-preview-katieboo85)
                   ("M-e" "emacs"        my/obp-preview-emacs)]
     [:description "Deploy"
                   ("P" "publish"      org-bootstrap-publish-publish)
                   ("A" "publish all"  org-bootstrap-publish-publish-all)
                   ("X" "abort"        org-bootstrap-publish-publish-abort)]
     [:description "Cloudflare"
                   ("c" "clean site…"  org-bootstrap-publish-clean-site)
                   ("f" "flush cache…" org-bootstrap-publish-flush-site)]])

  (global-set-key (kbd "C-c W") #'my/obp-menu))

(org-bootstrap-publish-use-site 'emacs)

;; -> obp-site-profiles-end

;; -> media-management

(defun my/media-run (command &optional dir)
  "Run a shell COMMAND asynchronously in DIR (defaulting to ~/nas)."
  (let* ((default-directory (expand-file-name (or dir "~/nas")))
         (buf-name (format "*media: %s*" command)))
    (async-shell-command command buf-name)
    (pop-to-buffer buf-name)))

(defun my/media-tag-images ()
  "Tag images in current directory using exiftool."
  (interactive)
  (my/media-run "tag_image_out.sh" default-directory))

(defun my/media-tag-videos ()
  "Tag videos in current directory using xmp sidecars."
  (interactive)
  (my/media-run "tag_video_out.sh" default-directory))

(defun my/media-process-images ()
  "Interactive image resizer (art, scans, photos)."
  (interactive)
  (my/media-run "images.sh"))

(defun my/media-process-videos ()
  "Resize videos from ~/nas/Photos to ~/DCIM/Videos."
  (interactive)
  (my/media-run "videos.sh"))

(defun my/media-sync-nas ()
  "Sync photos out to NAS."
  (interactive)
  (my/media-run "mysync --photos out"))

(defun my/media-regenerate-thumbnails ()
  "Regenerate missing tagged gallery thumbnails."
  (interactive)
  (my/media-run "tagged_thumbnail_update.sh" "~/DCIM/content/static/tagged"))

(defun my/media-clean-images ()
  "Remove resized jpg files from static image directories."
  (interactive)
  (my/media-run "rm -fr $(find ~/DCIM/content/static/art--gallery ~/DCIM/content/static/scans ~/DCIM/content/static/photos -mindepth 2 -name '*.jpg') 2>/dev/null; echo 'Image directories cleaned'"))

(defun my/media-clean-tagged ()
  "Remove resized jpg files from static/tagged directory."
  (interactive)
  (my/media-run "rm -fr $(find ~/DCIM/content/static/tagged -mindepth 2 -name '*.jpg') 2>/dev/null; echo 'Tagged directory cleaned'"))

(defun my/media-clean-all ()
  "Remove all resized images from static directories."
  (interactive)
  (my/media-run "rm -fr $(find ~/DCIM/content/static/art--gallery ~/DCIM/content/static/scans ~/DCIM/content/static/photos ~/DCIM/content/static/tagged -mindepth 2 -name '*.jpg') 2>/dev/null; echo 'All image directories cleaned'"))

(defvar my/nas-source-dir "~/nas/Photos"
  "Root directory for source photo/video collection.")

(unless noninteractive
  (require 'transient)

  (transient-define-prefix my/media-menu ()
    "Media — tagging, syncing, cleaning, and web publishing."
    [["Tag"
      ("ti" "tag images"    my/media-tag-images)
      ("tv" "tag videos"   my/media-tag-videos)]
     ["Process & Sync"
      ("p"  "process images"       my/media-process-images)
      ("v"  "process videos"       my/media-process-videos)
      ("s"  "sync to NAS"          my/media-sync-nas)]
     ["Clean & Maint"
      ("ci" "clean image dirs"     my/media-clean-images)
      ("ct" "clean tagged dir"     my/media-clean-tagged)
      ("ca" "clean all"            my/media-clean-all)
      ("r"  "regenerate"           my/media-regenerate-thumbnails)]
     ["Web Publishing"
      ("wu" "web update site…"     (lambda (site)
                                     (interactive (list (completing-read "Site: " '("dyerdwelling" "art" "katieboo85" "emacs") nil t)))
                                     (my/media-run (format "web update %s" site))))
      ("wp" "web publish site…"    (lambda (site)
                                     (interactive (list (completing-read "Site: " '("dyerdwelling" "art" "katieboo85" "emacs") nil t)))
                                     (my/media-run (format "web publish %s" site))))
      ("wa" "web publish all"      (lambda () (interactive) (my/media-run "web publish all")))]])

  (global-set-key (kbd "C-c M") #'my/media-menu))

;; -> media-management-end

(use-package htmlize
  :demand t)

(add-to-list 'load-path "~/.emacs.d/offline-packages/local-packages/transmute")
(require 'transmute)
(global-set-key (kbd "C-c I") #'transmute-menu)

(with-eval-after-load 'image-dired
  (require 'transmute)
  (transmute-setup-thumbnail-keys))

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
              (when (overlay-get ov 'diff-hl-hunk)
                (let* ((beg (overlay-start ov))
                       (end (overlay-end ov))
                       (type (overlay-get ov 'diff-hl-hunk-type))
                       (face (pcase type
                               ('insert 'my/demap-diff-added)
                               ('change 'my/demap-diff-modified)
                               ('delete 'my/demap-diff-removed)
                               (_ nil)))
                       (new-ov (make-overlay beg end minimap-buf)))
                  (when face
                    (overlay-put new-ov 'face face)
                    (overlay-put new-ov 'my/demap-diff t)
                    (overlay-put new-ov 'priority 100))))))))))

  (defun my/demap-diff-clear ()
    "Remove all diff overlays from the current minimap buffer."
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'my/demap-diff)
        (delete-overlay ov))))

  (defun my/demap-diff-update-current ()
    "Update diff overlays for any minimap showing the current buffer."
    (when-let* ((minimap-buf (get-buffer demap-minimap-default-name))
                (minimap (demap-buffer-minimap minimap-buf))
                (source-buf (demap-minimap-showing minimap))
                ((buffer-live-p source-buf)))
      (when (eq source-buf (current-buffer))
        (my/demap-diff-update minimap))))

  (defun my/demap-diff-schedule-update ()
    "Schedule a diff update for the minimap after diff-hl runs."
    (run-with-idle-timer 0.1 nil #'my/demap-diff-update-current))

  (defun my/demap-diff-after-save ()
    "Update demap diff overlays after saving a buffer with diff-hl active."
    (when (bound-and-true-p diff-hl-mode)
      (my/demap-diff-schedule-update)))

  (add-hook 'after-save-hook #'my/demap-diff-after-save)
  (add-hook 'window-state-change-hook
            (lambda ()
              (run-with-idle-timer 0.05 nil #'my/demap-diff-update-current)))

  (add-hook 'demap-minimap-construct-hook
            (lambda ()
              (my/demap-diff-update-current)))

  (defun my/demap-preserve-window (&rest _)
    "Set no-delete-other-windows on the demap side window."
    (when-let* ((buf (get-buffer demap-minimap-default-name))
                (win (get-buffer-window buf)))
      (set-window-parameter win 'no-delete-other-windows t)))
  (advice-add 'demap-open :after #'my/demap-preserve-window)
  (my/demap-preserve-window))

;;
;; -> visuals
;;
(set-frame-parameter nil 'alpha-background 90)
(add-to-list 'default-frame-alist '(alpha-background . 90))

(add-to-list 'load-path "~/.emacs.d/offline-packages/local-packages/diff-minimap")
(require 'diff-minimap)

;; (setq diff-minimap-side 'left)

;; (setq diff-minimap-viewport-style 'filled)
;; (setq diff-minimap-viewport-style 'edge-bar)
;; (setq diff-minimap-viewport-style 'stipple)
;; (setq diff-minimap-stipple-pattern 'dots-sparse)
;; (setq diff-minimap-colour-source 'diff-hl)
;; (setq diff-minimap-font-scale 0.2)

;;
;; -> bugs-overview — collect BUGS.org files across project areas
;;

(defvar my/bugs-search-roots
  (list (expand-file-name "~/.emacs.d")
        (expand-file-name "~/.emacs.d/Emacs-DIYer")
        (expand-file-name "~/.emacs.d/Emacs-vanilla")
        (expand-file-name "~/.emacs.d/offline-packages")
        (expand-file-name "~/.emacs.d/offline-packages/local-packages")
        (expand-file-name "~/source/repos")
        (expand-file-name "~/source")
        (expand-file-name "~/bin"))
  "Root directories to scan for BUGS.org files.")

(defun my/bugs-files ()
  "Return BUGS.org files found under `my/bugs-search-roots'.
Scans each root directly and any git-repo subdirectories one level deep."
  (let (files)
    (dolist (root my/bugs-search-roots)
      (when (file-directory-p root)
        (let ((bugs (expand-file-name "BUGS.org" root)))
          (when (file-exists-p bugs)
            (push bugs files)))
        (dolist (sub (directory-files root t "^[^.]"))
          (when (and (file-directory-p sub)
                     (file-exists-p (expand-file-name ".git" sub)))
            (let ((bugs (expand-file-name "BUGS.org" sub)))
              (when (file-exists-p bugs)
                (push bugs files)))))))
    (sort (delete-dups files) #'string<)))

(defun my/org-category-bugs-filename (orig-fun &optional pos force-refresh)
  "Override `org-get-category' for BUGS.org files.
Uses the parent directory name as the category instead of the
default \"BUGS\" so every entry shows which project it belongs to."
  (let ((cat (funcall orig-fun pos force-refresh)))
    (if (and (equal cat "BUGS")
             (buffer-file-name)
             (string-suffix-p "/BUGS.org" (buffer-file-name)))
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory (buffer-file-name))))
      cat)))

(advice-add 'org-get-category :around #'my/org-category-bugs-filename)

(defun my/bugs ()
  "Show all TODO entries across every BUGS.org file in project areas.
Scans roots defined by `my/bugs-search-roots'.
Each entry is prefixed with its project directory name thanks to
`my/org-category-bugs-filename'."
  (interactive)
  (let* ((fmt (if (listp org-agenda-prefix-format)
                  org-agenda-prefix-format
                (default-value 'org-agenda-prefix-format)))
         (org-agenda-files (my/bugs-files))
         (org-agenda-prefix-format
          (mapcar (lambda (item)
                    (if (eq (car item) 'todo)
                        (cons 'todo " %i %-20:c%?-12t% s")
                      item))
                  fmt)))
    (org-todo-list)))
