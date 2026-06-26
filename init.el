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
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/"))))

(setq url-queue-timeout 3)

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
;; (cl-loop for ext in '("\\.gpr$" "\\.ada$" "\\.ads$" "\\.adb$")
;; do (add-to-list 'auto-mode-alist (cons ext 'ada-light-mode)))

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

(tiny-diminish 'cursor-heatmap-mode)
;; (tiny-diminish 'simply-annotate-mode)
(tiny-diminish 'simple-autosuggest-mode)
(tiny-diminish 'org-indent-mode)

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

;; (my/sync-ui-accent-color "orange")

(defun insert-default-background-color ()
  "Insert the default background color at point."
  (interactive)
  (insert (downcase (face-attribute 'default :background))))

;; (use-package web-mode
;;   :mode "\\.cshtml?\\'"
;;   :hook (html-mode . web-mode)
;;   :bind (:map web-mode-map ("M-;" . nil)))

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

;;
;; -> mu4e
;;
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(when (locate-library "mu4e")
  (require 'mu4e)

  (setq mu4e-maildir "~/Maildir"
        mu4e-attachment-dir "~/Downloads"
        mu4e-change-filenames-when-moving t
        mu4e-update-interval 120
        mu4e-get-mail-command "mbsync -a"
        mu4e-headers-auto-update t
        mu4e-view-show-images t
        mu4e-view-show-addresses t
        mu4e-split-view 'vertical
        mu4e-context-policy 'pick-first
        mu4e-compose-context-policy 'ask
        mu4e-search-results-limit 2000
        message-send-mail-function 'smtpmail-send-it)

  ;; This format gives you "YYYY-MM-DD HH:MM" (e.g., 2026-05-21 08:53)
  (setq mu4e-headers-date-format "%Y-%m-%d %H:%M")

  (setq mu4e-headers-fields
        '((:date    . 17)    ; Date of the message
          ;; (:flags   .  6)    ; Message flags (U, R, F, etc.)
          (:from    . 22)    ; Sender name/email
          (:subject . nil))) ; Subject line (nil tells it to use all remaining space)


  (setq mu4e-contexts
        `(,(make-mu4e-context
            :name "james"
            :enter-func (lambda () (mu4e-message "Switch to james"))
            :leave-func (lambda () (mu4e-message "Leave james"))
            :match-func (lambda (msg)
                          (when msg
                            (string-prefix-p "/james" (mu4e-message-field msg :maildir))))
            :vars `((user-mail-address . "james@dyerdwelling.family")
                    (user-full-name . "james dyer")
                    (mu4e-sent-folder . "/james/Sent")
                    (mu4e-drafts-folder . "/james/Drafts")
                    (mu4e-trash-folder . "/james/Trash")
                    (mu4e-refile-folder . "/james/Archive")
                    (smtpmail-smtp-user . "james@dyerdwelling.family")
                    (smtpmail-smtp-server . "smtp.migadu.com")
                    (smtpmail-smtp-service . 465)
                    (smtpmail-stream-type . ssl)))
          ,(make-mu4e-context
            :name "bob"
            :enter-func (lambda () (mu4e-message "Switch to jimbob"))
            :leave-func (lambda () (mu4e-message "Leave jimbob"))
            :match-func (lambda (msg)
                          (when msg
                            (string-prefix-p "/jimbob" (mu4e-message-field msg :maildir))))
            :vars `((user-mail-address . "jimbob@dyerdwelling.family")
                    (user-full-name . "james dyer")
                    (mu4e-sent-folder . "/jimbob/Sent")
                    (mu4e-drafts-folder . "/jimbob/Drafts")
                    (mu4e-trash-folder . "/jimbob/Trash")
                    (mu4e-refile-folder . "/jimbob/Archive")
                    (smtpmail-smtp-user . "jimbob@dyerdwelling.family")
                    (smtpmail-smtp-server . "smtp.migadu.com")
                    (smtpmail-smtp-service . 465)
                    (smtpmail-stream-type . ssl)))
          ,(make-mu4e-context
            :name "captainflasmr"
            :enter-func (lambda () (mu4e-message "Switch to Gmail"))
            :leave-func (lambda () (mu4e-message "Leave Gmail"))
            :match-func (lambda (msg)
                          (when msg
                            (string-prefix-p "/captainflasmr" (mu4e-message-field msg :maildir))))
            :vars `((user-mail-address . "captainflasmr@gmail.com")
                    (user-full-name . "james dyer")
                    (mu4e-sent-folder . "/captainflasmr/[Gmail]/Sent Mail")
                    (mu4e-drafts-folder . "/captainflasmr/[Gmail]/Drafts")
                    (mu4e-trash-folder . "/captainflasmr/[Gmail]/Trash")
                    (mu4e-refile-folder . "/captainflasmr/[Gmail]/All Mail")
                    (smtpmail-smtp-user . "captainflasmr@gmail.com")
                    (smtpmail-smtp-server . "smtp.gmail.com")
                    (smtpmail-smtp-service . 587)
                    (smtpmail-stream-type . starttls)))))

  (setq mu4e-maildir-shortcuts
        '(("/jimbob/INBOX" . ?j)
          ("/james/INBOX" . ?m)
          ("/captainflasmr/INBOX" . ?g)))

  (setq mu4e-bookmarks
        '((:name "Unified Inbox"
                 :query "(maildir:/jimbob/INBOX OR maildir:/james/INBOX OR maildir:/captainflasmr/INBOX)"
                 :key ?b)
          (:name "Unified Archive"
                 :query "(maildir:/jimbob/Archive OR maildir:/james/Archive OR maildir:\"/captainflasmr/[Gmail]/All Mail\")"
                 :key ?a)
          (:name "Unified Sent"
                 :query "(maildir:/jimbob/Sent OR maildir:/james/Sent OR maildir:\"/captainflasmr/[Gmail]/Sent Mail\")"
                 :key ?s)
          (:name "Unified Trash"
                 :query "(maildir:/jimbob/Trash OR maildir:/james/Trash OR maildir:\"/captainflasmr/[Gmail]/Trash\")"
                 :key ?t)
          (:name "Unified Spam"
                 :query "(maildir:/jimbob/Junk OR maildir:/james/Junk OR maildir:\"/captainflasmr/[Gmail]/Spam\")"
                 :key ?p)
          (:name "Unread messages"
                 :query "flag:unread AND NOT flag:trashed"
                 :key ?u)
          (:name "Today's messages"
                 :query "date:today..now"
                 :key ?d)))

  (setq shr-use-colors nil
        shr-use-fonts nil
        mm-text-html-renderer 'shr)

  (remove-hook 'mu4e-view-rendered-hook 'mu4e-resize-linked-headers-window)
  (add-hook 'mu4e-view-rendered-hook
            (defun my/mu4e-balance-views ()
              (when (and (eq mu4e-split-view 'vertical)
                         (mu4e-current-buffer-type-p 'view))
                (when-let* ((win (get-buffer-window (current-buffer) t)))
                  (let ((target (floor (/ (frame-width) 2))))
                    (window-resize win (- target (window-total-width win)) t nil t))))))

  (defvar my/mu4e--focus-window nil)

  (advice-add 'mu4e-headers-view-message :before
              (defun my/mu4e-save-focus-window (&rest _)
                (setq my/mu4e--focus-window (selected-window))))

  (advice-add 'mu4e-view :after
              (defun my/mu4e-restore-focus-window (&rest _)
                (when (window-live-p my/mu4e--focus-window)
                  (select-window my/mu4e--focus-window 'norecord)
                  (setq my/mu4e--focus-window nil))))

  (global-set-key (kbd "C-c m") #'mu4e)

  (define-key mu4e-headers-mode-map (kbd "f") #'mu4e-headers-view-message)

  (advice-add 'mu4e-message :around
              (defun my/mu4e-suppress-indexing (orig-fn &rest args)
                "Suppress distracting indexing/retrieval progress messages from minibuffer."
                (unless (and (stringp (car args))
                             (string-match-p
                              "\\`\\(?:Indexing\\|Retrieving mail\\)" (car args)))
                  (apply orig-fn args))))

  ) ;; end (when (locate-library "mu4e"))

;;
;; -> magit
;;
(use-package magit
  :bind ("C-x g" . magit-status)
  :init
  ;; Repos for `magit-list-repositories' (each root, one level deep).
  ;; Set in :init so the variable exists even when that command loads
  ;; `magit-repos' without the full `magit' feature.
  (setq magit-repository-directories
        '(("~/.emacs.d" . 1)
          ("~/.emacs.d/offline-packages" . 1)
          ("~/.emacs.d/offline-packages/local-packages" . 1)
          ("~/source/repos" . 1)
          ("~/source" . 1)
          ("~/.config/sway" . 1)
          ("~/bin" . 1)))
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (setq magit-refresh-status-buffer t)
  (setq magit-section-initial-visibility-alist
        '((untracked . show)
          (stashes . hide)))
  (setq magit-diff-refine-hunk 'all)
  (define-key magit-status-mode-map (kbd "C-w") nil))

(use-package forge
  :after magit)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(misterioso))
 '(package-selected-packages
   '(agent-shell async csv-mode dape demap diff-hl dired-sidebar
                 doom-themes dumb-jump ef-themes elpa-mirror
                 forge gruvbox-theme htmlize i3wm-config-mode
                 kotlin-mode magit org-social ox-hugo package-lint
                 protobuf-mode timu-caribbean-theme timu-rouge-theme
                 timu-spacegrey-theme web-mode yaml-mode
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

(setq-default eglot-workspace-configuration
              '((java . (:import (:gradle (:offline (:enabled t)
                                                    :checksums (:verify :json-false)
                                                    :arguments "--offline")
                                          :autobuild (:enabled :json-false))))))
(setq jsonrpc-default-request-timeout 120)

(use-package dumb-jump
  :ensure t
  :after xref
  :custom
  (dumb-jump-force-searcher 'rg)
  (dumb-jump-prefer-searcher 'rg)
  (dumb-jump-selector 'completing-read)
  :config
  ;; Prepend globally so dumb-jump beats the default etags backend.
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))


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

;; (use-package diff-hl
;;   :ensure t
;;   :config
;;   (global-diff-hl-mode 1)
;;   (diff-hl-flydiff-mode 1)
;;   (unless (display-graphic-p)
;;     (diff-hl-margin-mode 1)))

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

(use-package powershell
  :mode (("\\.ps1\\'"  . powershell-mode)
         ("\\.psm1\\'" . powershell-mode)
         ("\\.psd1\\'" . powershell-mode)))

;; emeld-sidebar: a project file-tree sidebar that follows and highlights the
;; active file (current-line overlay + follow are built in).  `C-x m' toggles
;; it open/closed, rooted at the current file's project.
(use-package emeld
  :load-path "/home/jdyer/.emacs.d/offline-packages/local-packages/emeld"
  :bind ("C-x m" . emeld-sidebar))

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

  (defun my/obp-clear-cache ()
    "Delete the local build cache for the current site."
    (interactive)
    (when-let ((dir (and org-bootstrap-publish-cache-dir
                         (expand-file-name
                          (secure-hash 'sha1
                                       (expand-file-name org-bootstrap-publish-output-dir))
                          org-bootstrap-publish-cache-dir))))
      (delete-directory dir t)
      (message "org-bootstrap-publish: cache cleared (%s)" dir)))

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
     [:description "Cache"
                   ("C" "clear build cache" my/obp-clear-cache)]
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

(add-to-list 'load-path "~/.emacs.d/offline-packages/local-packages/diff-minimap")
(require 'diff-minimap)


;; (setq diff-minimap-viewport-style 'filled)
;; (setq diff-minimap-viewport-style 'edge-bar)
;; (setq diff-minimap-viewport-style 'stipple)
;; (setq diff-minimap-stipple-pattern 'dots-sparse)
;; (setq diff-minimap-colour-source 'diff-hl)
;; (setq diff-minimap-font-scale 0.2)
(setq diff-minimap-diff-backend 'vc)
;; (add-hook 'ediff-startup-hook #'diff-minimap-ediff-setup)


;;
;; -> project-overview — central table of git projects with status + actions
;;
;; `project-overview' (bound to M-l p) opens a tabulated-list dashboard with
;; one row per auto-discovered git project, showing the latest CHANGELOG.org
;; version/date, open/total BUGS.org count, and git branch/dirty/ahead-behind.
;; Single keys act on the project under point — see `project-overview-mode-map'.
;; `project-overview-bugs-agenda-all' (A in the dashboard) collects every
;; project's BUGS.org into one TODO agenda, replacing the old `my/bugs'.
;;

(use-package project-overview
  :ensure nil
  :load-path "~/.emacs.d/offline-packages/local-packages/project-overview"
  :commands (project-overview)
  :init
  (define-key my-jump-keymap (kbd "p") #'project-overview)
  ;; Show the dashboard instead of the splash screen at startup.
  ;; (setq initial-buffer-choice
  ;;       (lambda ()
  ;;         (project-overview)
  ;;         (get-buffer project-overview-buffer-name)))
  :config
  ;; Roots scanned for git projects (and their BUGS.org / CHANGELOG.org).
  (setq project-overview-search-roots
        (list (expand-file-name "~/.emacs.d")
              (expand-file-name "~/.emacs.d/Emacs-DIYer")
              (expand-file-name "~/.emacs.d/Emacs-vanilla")
              (expand-file-name "~/.emacs.d/offline-packages")
              (expand-file-name "~/.emacs.d/offline-packages/local-packages")
              (expand-file-name "~/source/repos")
              (expand-file-name "~/.config/sway")
              (expand-file-name "~/source")
              (expand-file-name "~/bin")))
  ;; Hide kernel trees and FedProClient-main_* checkouts.
  (setq project-overview-exclude-regexp "\\`\\(?:linux-\\|FedProClient-main_\\)")
  ;; My GitHub user (summarised in the header line, used by the "owned" filter).
  (setq project-overview-github-user "captainflasmr")
  ;; Open remotes in Firefox regardless of the system default browser.
  (setq project-overview-browse-url-function #'browse-url-firefox))

;;
;; -> old-ada-mode loaded from local-packages/ if present (see coding guide
;; Part 12). Silent no-op otherwise.
;;
(let ((old-ada (expand-file-name "offline-packages/local-packages/old-ada-mode" user-emacs-directory)))
  (when (file-directory-p old-ada)
    (add-to-list 'load-path old-ada)
    (use-package ada-mode
      :ensure nil
      :mode ("\\.gpr\\'" "\\.ada\\'" "\\.ads\\'" "\\.adb\\'"))))

;;
;; -> Ada project file helpers — ada_language_server needs a .gpr to work
;; properly; these functions find existing ones or create a basic template.
;;
(defun my/ada-find-gpr-file (&optional dir)
  "Find the nearest .gpr file upward from DIR (defaults to `default-directory')."
  (let ((dir (or dir default-directory)))
    (locate-dominating-file dir
                            (lambda (d) (car (directory-files d t "\\.gpr\\'"))))))

(defun my/ada-create-gpr (project-name &optional main-file)
  "Create PROJECT-NAME.gpr with an optional MAIN-FILE entry point."
  (interactive "sProject name: \nsMain Ada file (e.g. main.adb): ")
  (let ((gpr-file (concat project-name ".gpr")))
    (with-current-buffer (find-file gpr-file)
      (erase-buffer)
      (insert (format "project %s is\n" project-name)
              "\n"
              "   for Source_Dirs use (\"**\");\n"
              "   for Object_Dir use \"build\";\n"
              "   for Exec_Dir use \".\";\n"
              (when (and main-file (not (string-empty-p main-file)))
                (format "   for Main use (\"%s\");\n" main-file))
              "\n"
              (format "end %s;\n" project-name))
      (goto-char (point-min))
      (when (called-interactively-p 'interactive)
        (message "Created %s" gpr-file))
      gpr-file)))

(defun my/ada-ensure-gpr (&optional dir)
  "Find a .gpr file in DIR or interactively create one.
Returns the absolute path to the .gpr file, or nil if cancelled."
  (let ((dir (or dir default-directory)))
    (or (my/ada-find-gpr-file dir)
        (when (y-or-n-p "No .gpr file found.  Create one? ")
          (call-interactively #'my/ada-create-gpr)))))

(defun my/ada-setup-project (&optional dir)
  "Set up ada_language_server project for the current Ada buffer.
Finds or creates a .gpr file and restarts eglot so ALS picks it up."
  (interactive)
  (unless (derived-mode-p 'ada-mode)
    (user-error "Not in an Ada buffer"))
  (let* ((dir (or dir default-directory))
         (gpr (my/ada-ensure-gpr dir)))
    (if gpr
        (let* ((gpr-dir (file-name-directory gpr))
               (gpr-file (file-name-nondirectory gpr)))
          ;; Tell ALS which project file to use via eglot workspace config
          (setq-local eglot-workspace-configuration
                      `((ada . ((projectFile . ,gpr)))))
          ;; If eglot is already running, restart it so ALS re-reads config
          (when (eglot-current-server)
            (eglot-shutdown (eglot-current-server))
            ;; Re-open the project root so eglot picks the right root
            (let ((default-directory gpr-dir))
              (eglot-ensure))))
      (user-error "No .gpr file; ada_language_server needs a project file"))))

(use-package markdown-mode)

(setq ztree-indent-step 2)

(add-to-list 'load-path "~/.emacs.d/offline-packages/local-packages/emeld")
(require 'emeld)

(global-set-key (kbd "C-c z d") 'emeld-diff-dwim)
(global-set-key (kbd "C-c z j") 'emeld-load-preset)
(global-set-key (kbd "C-c z l") 'emeld-load-preset)
(global-set-key (kbd "C-c z s") 'emeld-save-preset)
(global-set-key (kbd "C-c z x") 'emeld-delete-preset)
;; (setq emeld-benchmark t)
(setq emeld-dir-prefix "📁 ")

;; 1. Classic solid triangles — medium (U+25BA / U+25BC)
;; (setq emeld-fold-collapsed-indicator "► "
;; emeld-fold-expanded-indicator  "▼ ")

;; 2. "Media" triangles — noticeably larger/bolder (U+23F5 / U+23F7)
;; (setq emeld-fold-collapsed-indicator "⏵ "
;; emeld-fold-expanded-indicator  "⏷ ")

;; 3. Centred medium triangles (U+2BC8 / U+2BC6)
;; (setq emeld-fold-collapsed-indicator "⯈ "
;; emeld-fold-expanded-indicator  "⯆ ")

;;
;; -> visuals
;;
(set-frame-parameter nil 'alpha-background 90)
(add-to-list 'default-frame-alist '(alpha-background . 90))

;; $ emacs --batch --eval '(progn (find-file "/home/jdyer/.emacs.d/offline-packages/local-packages/emeld/emeld.el") (goto-char (point-min)) (condition-case nil (while (not (eobp)) (forward-sexp)) (error (message "Unbalanced at pos %d, line %d, col %d" (point) (line-number-at-pos) (current-column)))))' 2>&1
;; Unbalanced at pos 31818, line 693, col 62

(use-package simply-kanban
  :demand t
  :load-path "~/.emacs.d/offline-packages/local-packages/simply-kanban"
  :bind
  ("C-c K" . simply-kanban)
  ("C-c A" . simply-kanban-agenda))

(define-key org-mode-map (kbd "C-c ;") #'simply-kanban-show-card)

(load-theme 'doom-city-lights t)

(defun pwsh-here ()
  "Open a native Windows PowerShell console in the current directory."
  (interactive)
  (start-process "powershell" nil "powershell.exe"
                 "-NoExit" "-Command"
                 (format "Set-Location -LiteralPath '%s'"
                         (expand-file-name default-directory))))

(use-package outline-indent
  :ensure t
  :commands outline-indent-minor-mode
  :custom
  (outline-indent-ellipsis " ▼")
  :hook
  ((nxml-mode
    mhtml-mode
    prog-mode
    web-mode
    yaml-mode
    yaml-ts-mode
    json-mode
    json-ts-mode
    js-json-mode
    dockerfile-mode
    python-mode
    python-ts-mode
    conf-mode) . outline-indent-minor-mode))

;; (add-hook 'outline-indent-minor-mode-hook
;;           (lambda ()
;;             (when (derived-mode-p 'nxml-mode)
;;               (outline-indent-close-level 2))))

(require 'transient)

(transient-define-prefix outline-indent-transient ()
  "Outline / outline-indent folding, navigation, and structure commands."
  :transient-non-suffix #'transient--do-stay
  [["Fold at point"
    ("TAB" "Toggle level"        outline-indent-toggle-level-at-point)
    ("o"   "Open fold"           outline-indent-open-fold)
    ("c"   "Close fold"          outline-indent-close-fold)
    ("O"   "Open recursively"    outline-indent-open-fold-rec)
    ("s"   "Show subtree"        outline-show-subtree)
    ("h"   "Hide subtree"        outline-hide-subtree)]
   ["Whole buffer"
    ("a"   "Show all"            outline-show-all)
    ("t"   "Hide body"           outline-hide-body)
    ("r"   "Open all folds"      outline-indent-open-folds)
    ("m"   "Close all folds"     outline-indent-close-folds)
    ("q"   "Hide to N levels"    outline-hide-sublevels)
    ("k"   "Isolate (hide other)" outline-hide-other)]
   ["Navigate"
    ("n"   "Next heading"        outline-next-visible-heading)
    ("p"   "Prev heading"        outline-previous-visible-heading)
    ("f"   "Fwd same level"      outline-indent-forward-same-level)
    ("b"   "Back same level"     outline-indent-backward-same-level)
    ("u"   "Up heading"          outline-up-heading)]
   ["Structure / Isolate"
    (">"   "Shift right"         outline-indent-shift-right)
    ("<"   "Shift left"          outline-indent-shift-left)
    ("U"   "Move subtree up"     outline-indent-move-subtree-up)
    ("D"   "Move subtree down"   outline-indent-move-subtree-down)
    ("v"   "Select block"        outline-indent-select)
    ("N"   "Narrow to block"     outline-indent-narrow)
    ("w"   "Widen"               widen)]
   ])

(with-eval-after-load 'outline-indent
  (define-key outline-indent-minor-mode-map (kbd "C-c o") #'outline-indent-transient)
  ;; Global fold/unfold across every outline-indent buffer (web-mode muscle memory).
  (define-key outline-indent-minor-mode-map (kbd "C-c C-f") #'outline-indent-toggle-fold))

(use-package simply-annotate
  :demand t
  :load-path "~/source/repos/simply-annotate"
  :hook (find-file-hook . simply-annotate-mode)
  :config
  (global-set-key (kbd "M-s") simply-annotate-command-map)
  (setq simply-annotate-inline-position 'above)
  (setq simply-annotate-tint-amount 20)
  (setq simply-annotate-hide-done-statuses '("resolved" "closed"))
  ;; (setq simply-annotate-hide-done-style 'full)
  (setq simply-annotate-hide-done-style 'indicator)

  ;; (setq simply-annotate-thread-statuses '("inbox" "doing" "done" "closed"))
  
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

(load-theme 'deeper-blue t)

(custom-set-faces
 '(mode-line-buffer-id ((t (:foreground "#EEEEFF" :weight bold)))))
