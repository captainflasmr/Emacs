;; -*- lexical-binding: t; -*-

;;
;; -> core-configuration
;;
(load-file "~/.emacs.d/Emacs-vanilla/init.el")

;;
;; -> fonts
;;
;; Font heights and `my/apply-main-font-sizes' moved to
;; Emacs-vanilla/init.el (`font-size-core').  C-z z resets the display
;; scale after buffer or global zooming.

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

;; (define-key my-overrides-mode-map (kbd "C-c w") selected-window-accent-map)

;;
;; -> org-table-fit
;;
;; org 9.7 removed the org-table-map prefix keymap (C-c | is now a
;; direct binding), so these live on the free C-c t prefix.

(use-package org-table-fit
  :load-path "~/.emacs.d/offline-packages/local-packages/org-table-fit"
  :hook (org-mode . org-table-fit-overlay-mode)
  :bind (:map org-mode-map
              ("C-c t i" . org-table-fit-window)
              ("C-c t t" . org-table-fit-overlay-mode)
              ("C-c t u" . org-table-fit-unwrap)
              ("C-c t r" . org-table-fit-toggle-reveal-on-point))
  :config
  (setq org-table-fit-overlay-reveal-on-point nil))

;;
;; -> org-ibullets
;;
;; Display Org heading stars as UTF-8 bullets.  Installed from source with
;; the built-in `:vc' backend; `:commands' keeps it lazy until an Org buffer
;; is opened.  Disabling font-cache compaction keeps the composed bullets
;; fast (see the package README).

(use-package org-ibullets
  :vc (:url "https://github.com/jamescherti/org-ibullets.el"
            :rev :newest)
  :commands org-ibullets-mode
  :init
  (setq my/org-ibullets-configured t)
  :hook (org-mode . org-ibullets-mode))

(setq inhibit-compacting-font-caches t)

;;
;; -> org-slides
;;
;; Present an Org file directly from the buffer: each top-level heading is
;; one slide.  `C-c P' starts the presentation; `n'/`SPC'/right move
;; forward, `p'/`S-SPC'/left move back, and `q' quits.  The package handles
;; the text scale, inline images, read-only mode, and cursor hiding itself.
;; `C-c s' is the global spelling-menu prefix, so use capital `P' for slides.
;; The source is vendored in offline-packages/local-packages/org-slides, so
;; this needs no package archive; `:ensure nil' keeps startup offline-safe.
;; Experimental: set `org-slides-page-style' to `split' to narrow each page
;; to only that page's content and show "SLIDE.PAGE / TOTALS" in the mode line.
;; SPC/f reveals the next Org list item; S-SPC (or b/S-TAB/Backspace)
;; re-shadows the previous one.
;; The indicator is shown on every slide by default; set
;; `org-slides-page-indicator' to `split' for only multi-page slides, or
;; nil to disable it.  The title page is slide 0 and top-level headings are
;; numbered 1, 2, ...
;;

(use-package org-slides
  :ensure nil
  :load-path ("offline-packages/local-packages/org-slides"
              "local-packages/org-slides")
  :commands (org-slides-start org-slides-timed-macro-toggle)
  :bind (("C-c T" . org-slides-timed-macro-toggle)
         :map org-mode-map
         ("C-c P" . org-slides-start))
  :init
  ;; (setq my/org-slides-configured t)
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-c P") #'org-slides-start))
  :config
  (setq org-slides-page-style 'split)
  (setq org-slides-text-scale 3)
  (setq org-slides-hide-stars-in-headings nil)
  ;; (setq org-slides-hide-cursor-on-start nil)
  ;; Refit displayed tables after org-slides narrows to a slide.  The
  ;; text-scale refresh can run while the title page is current, and slide
  ;; navigation does not otherwise retrigger it, so wide tables can keep
  ;; their old layout until the window is resized.
  (add-hook 'org-slides-after-navigate-functions
            (lambda (&rest _)
              (when (and (bound-and-true-p org-table-fit-overlay-mode)
                         (fboundp 'org-table-fit-refresh-overlays))
                (org-table-fit-refresh-overlays)))))

;;
;; -> use-package
;;
(use-package async
  :defer t)
(use-package i3wm-config-mode
  :defer t)
(use-package yaml-mode
  :mode ("\\.yml\\'" "\\.yaml\\'"))

;;
;; -> plantuml
;;
(use-package plantuml-mode
  :mode ("\\.puml\\'" "\\.plantuml\\'" "\\.iuml\\'")
  :custom
  (plantuml-jar-path "~/.emacs.d/plantuml.jar")
  (plantuml-default-exec-mode 'jar)
  (plantuml-output-type "png"))

(use-package ox-plantuml-gantt
  :load-path "~/.emacs.d/offline-packages/local-packages/ox-plantuml-gantt"
  :after org)

;;
;; -> keys-navigation
;;

;;
;; -> themes
;;
(use-package doom-themes)
(use-package ef-themes
  :defer t)
(use-package gruvbox-theme
  :defer t)
(use-package timu-caribbean-theme
  :defer t)

(use-package timu-spacegrey-theme
  :defer t
  :custom
  ;; (setq timu-spacegrey-flavour "light")
  (timu-spacegrey-scale-org-document-title 1.8)
  (timu-spacegrey-scale-org-document-info 1.4)
  (timu-spacegrey-scale-org-level-1 1.8)
  (timu-spacegrey-scale-org-level-2 1.4)
  (timu-spacegrey-scale-org-level-3 1.2))

(use-package timu-rouge-theme
  :defer t
  :custom
  ;; (setq timu-rouge-org-intense-colors t)
  (timu-rouge-mode-line-border t)
  (timu-rouge-scale-org-document-title 1.8)
  (timu-rouge-scale-org-document-info 1.4)
  (timu-rouge-scale-org-level-1 1.8)
  (timu-rouge-scale-org-level-2 1.4)
  (timu-rouge-scale-org-level-3 1.2))

;;
;; -> auto-mode-alist
;;
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
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

(use-package csv-mode
  :mode "\\.csv\\'")
(use-package package-lint
  :commands (package-lint-current-buffer
             package-lint-batch-and-exit))

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
  :bind
  ("C-c O" . ollama-buddy-transient-menu)
  :config
  ;; overall default model
  ;; (setq ollama-buddy-default-model "u:gemma4:31b-cloud")
  (setq ollama-buddy-default-model "n:deepseek-v4.1-flash")

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
  (ollama-buddy-update-menu-entry 'refactor-code     :model "n:deepseek-v4.1-flash")
  (ollama-buddy-update-menu-entry 'git-commit        :model "n:deepseek-v4.1-flash")
  (ollama-buddy-update-menu-entry 'describe-code     :model "n:deepseek-v4.1-flash")
  (ollama-buddy-update-menu-entry 'dictionary-lookup :model "n:deepseek-v4.1-flash")
  (ollama-buddy-update-menu-entry 'synonym           :model "n:deepseek-v4.1-flash")
  (ollama-buddy-update-menu-entry 'proofread         :model "n:deepseek-v4.1-flash")

  ;; (ollama-buddy-update-menu-entry 'refactor-code     :model "u:gemma4:31b-cloud")
  ;; (ollama-buddy-update-menu-entry 'git-commit        :model "u:gemma4:31b-cloud")
  ;; (ollama-buddy-update-menu-entry 'describe-code     :model "u:gemma4:31b-cloud")
  ;; (ollama-buddy-update-menu-entry 'dictionary-lookup :model "u:gemma4:31b-cloud")
  ;; (ollama-buddy-update-menu-entry 'synonym           :model "u:gemma4:31b-cloud")
  ;; (ollama-buddy-update-menu-entry 'proofread         :model "u:gemma4:31b-cloud")

  ;; dired integration
  (with-eval-after-load 'dired
    (define-key dired-mode-map (kbd "C-c C-a") #'ollama-buddy-dired-attach-marked-files)))

;;
;; -> other
;;
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

;; (use-package web-mode
;;   :mode "\\.cshtml?\\'"
;;   :hook (html-mode . web-mode)
;;   :bind (:map web-mode-map ("M-;" . nil)))

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
  :config
  (setq dired-image-thumbnail-auto-accept t)
  (setq dired-image-thumbnail-sort-by 'date)
  (setq dired-image-thumbnail-sort-order 'descending)
  :bind
  (:map dired-mode-map
        ("r" . dired-image-thumbnail)  ; r for render/reveal thumbnails
        ("C-t s" . dired-image-thumbnail-insert-image-subdirs)  ; s for smart subdirs
        ("C-t z" . dired-image-thumbnail-insert-subdir-recursive)  ; z for all (last letter)
        ("C-t k" . dired-image-thumbnail-kill-all-subdirs)))  ; k for kill

;;
;; -> mu4e
;;
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(when (locate-library "mu4e")
  ;; mu4e pulls in gnus/message and friends (~0.5s): load it on first
  ;; use via C-c m or M-x mu4e rather than at startup.
  (autoload 'mu4e "mu4e" "Start mu4e." t)
  (define-key my-overrides-mode-map (kbd "C-c m") #'mu4e)

  (with-eval-after-load 'mu4e
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
                      (smtpmail-smtp-service . 587)
                      (smtpmail-stream-type . starttls)))
            ,(make-mu4e-context
              :name "jimbob"
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
                      (smtpmail-smtp-service . 587)
                      (smtpmail-stream-type . starttls)))
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

    (define-key mu4e-headers-mode-map (kbd "f") #'mu4e-headers-view-message)

    (advice-add 'mu4e-message :around
		        (defun my/mu4e-suppress-indexing (orig-fn &rest args)
                  "Suppress distracting indexing/retrieval progress messages from minibuffer."
                  (unless (and (stringp (car args))
                               (string-match-p
				                "\\`\\(?:Indexing\\|Retrieving mail\\)" (car args)))
                    (apply orig-fn args))))

    ;;
    ;; -> mu4e-update-indicator
    ;;
    ;; Show retrieval/indexing progress in the header-line of mu4e buffers.
    ;; mu4e's own progress messages are suppressed (see
    ;; `my/mu4e-suppress-indexing' above), so this is the main feedback
    ;; during mail updates: retrieve progress comes from mbsync's status
    ;; line, indexing progress from `mu4e-index-message' calls.
    (defvar my/mu4e--updating nil
      "Non-nil while mu4e is retrieving or indexing mail.")
    (defvar my/mu4e-update-timer nil
      "Timer for refreshing the mu4e update header-line.")
    (defvar my/mu4e--index-progress nil
      "Latest indexing progress, e.g. \"checked 2588 · updated 3\".")
    (defvar-local my/mu4e--saved-header-line-format nil
      "Value of `header-line-format' to restore after an update.")

    (defun my/mu4e--update-buffer-p ()
      "Return non-nil if the mu4e retrieval process is alive."
      (and (boundp 'mu4e--update-buffer)
           (buffer-live-p mu4e--update-buffer)
           (process-live-p (get-buffer-process mu4e--update-buffer))))

    (defun my/mu4e--buffer-is-mu4e-p ()
      "Return non-nil if current buffer is in a mu4e mode."
      (memq major-mode
            '(mu4e-main-mode mu4e-headers-mode
                             mu4e-view-mode mu4e-raw-view-mode)))

    (defun my/mu4e--indicator-p (value)
      "Return non-nil if VALUE is one of our update indicator strings."
      (and (stringp value)
           (get-text-property 0 'my/mu4e-indicator value)))

    (defun my/mu4e--format-mbsync-status (status)
      "Render an mbsync status line STATUS compactly; nil if unrecognized.
STATUS looks like this, with the counts changing as mail is moved:
\"C: 1/3  B: 11/12  F: +0/0 *0/0 #0/0 -0/0  N: +210/337 *0/0 #0/0 -0/0\"."
      (when (and status
                 (string-match
                  (concat "\\`C: \\([0-9]+\\)/\\([0-9]+\\)"
                          " +B: \\([0-9]+\\)/\\([0-9]+\\)"
                          ".* N: \\([+*#-]\\)\\([0-9]+\\)/\\([0-9]+\\)")
                  status))
        (let ((parts (list (format "%s/%s accounts"
                                   (match-string 1 status)
                                   (match-string 2 status)))))
          ;; Box count is 0/0 until the first mailbox of a channel is known.
          (unless (string= (match-string 4 status) "0")
            (setq parts (append parts (list (format "%s/%s mailboxes"
                                                    (match-string 3 status)
                                                    (match-string 4 status))))))
          ;; Only count transferred ("+") messages, when there are any.
          (when (and (string= (match-string 5 status) "+")
                     (not (string= (match-string 7 status) "0")))
            (setq parts (append parts (list (format "%s/%s new"
                                                    (match-string 6 status)
                                                    (match-string 7 status))))))
          (mapconcat #'identity parts " · "))))

    (defun my/mu4e--retrieval-progress ()
      "Describe the current mail retrieval progress.
Prefer the latest mbsync status line from `mu4e--update-buffer'; fall
back to its last non-empty line.  Return nil when nothing is known."
      (when (and (boundp 'mu4e--update-buffer)
                 (buffer-live-p mu4e--update-buffer))
        (with-current-buffer mu4e--update-buffer
          (let* ((tail (buffer-substring-no-properties
                        (max (point-min) (- (point-max) 2000))
                        (point-max)))
                 ;; Foreground updates rewrite the status line in place,
                 ;; background ones accumulate them separated by CRs.
                 (lines (split-string tail "[\r\n]+" t))
                 (status (car (last (seq-filter
                                     (lambda (line) (string-match-p "\\`C: " line))
                                     lines)))))
            (or (my/mu4e--format-mbsync-status status)
                (when-let* ((line (car (last (seq-filter
                                              (lambda (l)
                                                (not (string-empty-p (string-trim l))))
                                              lines)))))
                  (truncate-string-to-width
                   (replace-regexp-in-string
                    "%" "%%" ;; escape for use in `header-line-format'
                    (string-trim (replace-regexp-in-string "[ \t]+" " " line)))
                   60 nil nil "…")))))))

    (defun my/mu4e--record-index-progress (frm &rest args)
      "Record indexing progress from a `mu4e-index-message' FRM call.
Also starts showing the indicator for indexing started outside of a
mail retrieval (e.g. \\[mu4e-update-index])."
      (let ((msg (ignore-errors (apply #'format frm args))))
        (cond
         ((and (stringp msg)
               (string-match
                (concat "\\`Indexing\\.\\.\\. checked \\([0-9]+\\), "
                        "updated \\([0-9]+\\)")
                msg))
          (unless my/mu4e-update-timer
            (my/mu4e-update-header-start))
          (setq my/mu4e--index-progress
                (format "checked %s · updated %s"
                        (match-string 1 msg) (match-string 2 msg))))
         ((and (stringp msg)
               (string-match "\\`\\(?:Lazy indexing\\|Indexing\\) completed" msg))
          (setq my/mu4e--index-progress nil)))))

    (defun my/mu4e-update-header-refresh ()
      "Refresh the header-line in mu4e buffers to show update status.
Shows the current mail retrieval or indexing progress and restores
mu4e's own header-line content once the update is done."
      (let* ((retrieving (my/mu4e--update-buffer-p))
             (indicator
              (cond
               (retrieving
                (propertize (format " ⟳ mu4e retrieving: %s "
                                    (or (my/mu4e--retrieval-progress) "connecting…"))
                            'face 'mode-line-highlight
                            'my/mu4e-indicator t))
               (my/mu4e--updating
                (propertize (if my/mu4e--index-progress
                                (format " ⟳ mu4e indexing: %s " my/mu4e--index-progress)
                              " ⟳ mu4e updating… ")
                            'face 'mode-line-highlight
                            'my/mu4e-indicator t)))))
        (dolist (buf (buffer-list))
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (when (my/mu4e--buffer-is-mu4e-p)
                (if indicator
                    (progn ;; remember mu4e's own header-line for restoration
                      (unless (my/mu4e--indicator-p header-line-format)
                        (setq-local my/mu4e--saved-header-line-format
                                    header-line-format))
                      (setq-local header-line-format indicator))
                  (when (my/mu4e--indicator-p header-line-format)
                    (setq-local header-line-format
                                my/mu4e--saved-header-line-format)))))))
        (force-mode-line-update t)
        (unless indicator
          (setq my/mu4e--updating nil
                my/mu4e--index-progress nil)
          (when my/mu4e-update-timer
            (cancel-timer my/mu4e-update-timer)
            (setq my/mu4e-update-timer nil)))))

    (defun my/mu4e-update-header-start ()
      "Start the mu4e update header-line timer."
      (setq my/mu4e--updating t
            my/mu4e--index-progress nil)
      (my/mu4e-update-header-refresh)
      (unless my/mu4e-update-timer
        (setq my/mu4e-update-timer
              (run-at-time 0.5 0.5 #'my/mu4e-update-header-refresh))))

    (defun my/mu4e-update-header-stop ()
      "Mark mu4e update as done and clear header-lines."
      (setq my/mu4e--updating nil)
      (my/mu4e-update-header-refresh))

    (advice-add 'mu4e-index-message :before #'my/mu4e--record-index-progress)
    (add-hook 'mu4e-update-pre-hook #'my/mu4e-update-header-start)
    (add-hook 'mu4e-index-updated-hook #'my/mu4e-update-header-stop)
    )) ;; end (with-eval-after-load 'mu4e) and (when (locate-library "mu4e"))

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(agent-shell async csv-mode dape diff-hl dired-sidebar
                 doom-themes dumb-jump ef-themes elpa-mirror
                 forge gruvbox-theme htmlize i3wm-config-mode
                 kotlin-mode magit org-social ox-hugo package-lint
                 protobuf-mode timu-caribbean-theme timu-rouge-theme
                 timu-spacegrey-theme web-mode yaml-mode)))

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
  :hook (nxml-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               `(java-mode . ("/home/jdyer/.emacs.d/bin/jdtls/bin/jdtls"
                              :initializationOptions
                              (:bundles ["/home/jdyer/.emacs.d/bin/jdtls/com.microsoft.java.debug.plugin-0.53.2.jar"]))))

  (add-to-list 'eglot-server-programs
               '(nxml-mode . ("java" "-jar"
                              "/home/jdyer/Downloads/org.eclipse.lemminx-uber.jar")))

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
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate 100))


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

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(kotlin-mode . ("kotlin-language-server"))))

(use-package kotlin-mode
  :mode "\\.kts\\'")

(use-package powershell
  :mode (("\\.ps1\\'"  . powershell-mode)
         ("\\.psm1\\'" . powershell-mode)
         ("\\.psd1\\'" . powershell-mode)))

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
                   ("X" "abort"        org-bootstrap-publish-publish-abort)
                   ("m" "menu"         org-bootstrap-publish-menu)]
     [:description "Cache"
                   ("C" "clear build cache" my/obp-clear-cache)]
     [:description "Cloudflare"
                   ("c" "clean site…"  org-bootstrap-publish-clean-site)
                   ("f" "flush cache…" org-bootstrap-publish-flush-site)]])

  (define-key my-overrides-mode-map (kbd "C-c W") #'my/obp-menu))

(org-bootstrap-publish-use-site 'emacs)

;; -> obp-site-profiles-end

(use-package htmlize
  :demand t)

(use-package transmute
  :load-path "~/.emacs.d/offline-packages/local-packages/transmute"
  :bind (:map my-overrides-mode-map
              ("C-c I" . transmute-menu))
  :config
  (with-eval-after-load 'image-dired
    (transmute-setup-thumbnail-keys)))

;;
;; -> diff-minimap — minimap sidebar showing diff regions
;;
;; Toggle with `C-z m' (see `my-win-keymap' below), not `C-x v m':
;; that is stock `vc-merge' and prompts for a branch merge in VC buffers.
;; With the minimap open, every hunk is shown inline; edits update the diff
;; live (reverting text to HEAD clears it) and `M-n'/`M-p' (also `M-F5'/
;; `M-S-F5', `C-x v ]'/`C-x v [') step between hunks.  `C-c r' reverts the
;; hunk at point in place (no vc-diff needed), and `C-x u' undoes it.  Old
;; lines are virtual, new lines are real text; use `C-x o' to switch windows
;; (it skips the minimap).  `C-z m' closes the shared sidebar but clears
;; inlines only in the current buffer; other buffers keep theirs.  `C-c e'/
;; `C-c E' toggle one/all inline hunks, leaving `C-c p'/`C-c P' free for
;; major modes (Org uses `C-c P' for org-slides-start).  The sidebar only
;; appears for files with local changes.
;;
(add-to-list 'load-path "~/.emacs.d/offline-packages/local-packages/diff-minimap")
(autoload 'diff-minimap-toggle "diff-minimap" nil t)


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

(use-package markdown-mode
  :mode ("\\.md\\'" "\\.markdown\\'"))

;; emeld-sidebar: a project file-tree sidebar that follows and highlights the
;; active file (current-line overlay + follow are built in).  `C-x m' toggles
;; it open/closed, rooted at the current file's project.
;;
;; `C-c z' is the emeld compare namespace: d diff-dwim, j reopen a recent
;; comparison; a marks the current dired directory as comparison side A,
;; c compares it with the directory being browsed.
(use-package emeld
  :load-path "/home/jdyer/.emacs.d/offline-packages/local-packages/emeld"
  :commands (emeld-diff-dwim emeld-recent)
  :bind (("C-x m"   . emeld-sidebar)
         ("C-c z d" . emeld-diff-dwim)
         ("C-c z l" . emeld-recent)
         ("C-c z a" . emeld-mark-dir-a)
         ("C-c z c" . emeld-compare-marked-dirs))
  ;; M-n / M-p keep emeld's own change navigation (next/previous changed
  ;; file) and M-N / M-P step through its hunks: `emeld-mode' is in
  ;; `my/window-nav-passthrough-modes', so the global window bindings
  ;; stand aside there.
  :config
  ;; (setq emeld-benchmark t)
  ;; (setq emeld-dir-prefix "📁 ")
  )

;; 1. Classic solid triangles — medium (U+25BA / U+25BC)
;; (setq emeld-fold-collapsed-indicator "► "
;; emeld-fold-expanded-indicator  "▼ ")

;; 2. "Media" triangles — noticeably larger/bolder (U+23F5 / U+23F7)
;; (setq emeld-fold-collapsed-indicator "⏵ "
;; emeld-fold-expanded-indicator  "⏷ ")

;; 3. Centred medium triangles (U+2BC8 / U+2BC6)
;; (setq emeld-fold-collapsed-indicator "⯈ "
;; emeld-fold-expanded-indicator  "⯆ ")

;; $ emacs --batch --eval '(progn (find-file "/home/jdyer/.emacs.d/offline-packages/local-packages/emeld/emeld.el") (goto-char (point-min)) (condition-case nil (while (not (eobp)) (forward-sexp)) (error (message "Unbalanced at pos %d, line %d, col %d" (point) (line-number-at-pos) (current-column)))))' 2>&1
;; Unbalanced at pos 31818, line 693, col 62

(use-package simply-kanban
  :load-path "~/.emacs.d/offline-packages/local-packages/simply-kanban"
  :commands (simply-kanban-show-card)
  :bind
  ("C-c K" . simply-kanban)
  ("C-c A" . simply-kanban-agenda))

(define-key org-mode-map (kbd "C-c ;") #'simply-kanban-show-card)

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

(defun my/outline-hide-sublevels-prompt ()
  "Hide buffer to a given level, prompting for the level number."
  (interactive)
  (outline-hide-sublevels (read-number "Levels to hide: ")))

(defun my/outline-toggle-recursive ()
  "Toggle fold: close if open, open recursively if closed."
  (interactive)
  (if (outline-indent-folded-p)
      (outline-indent-open-fold-rec)
    (outline-indent-close-fold)))

;; Outline-aware imenu: XML buffers index elements instead of every
;; physical line; `C-c o i' uses outline headings in any other buffer.
(defvar-local my/outline-imenu-heading-function nil
  "Function used to label outline headings in the imenu index.
Called with point on a heading; return a string, or nil to omit
the heading.  When nil, use the trimmed heading line.")

(defun my/outline-imenu-default-heading ()
  "Return the trimmed text of the heading line at point."
  (string-trim (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position))))

(defun my/nxml-outline-imenu-tag-text ()
  "Return the complete XML start tag at point, possibly spanning lines."
  (save-excursion
    (skip-chars-forward " \t")
    (when (eq (char-after) ?<)
      (let ((start (point))
            (limit (min (point-max) (+ (point) 4096)))
            end)
        (while (and (not end) (< (point) limit))
          (if (search-forward ">" (line-end-position) t)
              (setq end (point))
            (forward-line 1)))
        (when end
          (buffer-substring-no-properties start end))))))

(defun my/nxml-outline-imenu-heading ()
  "Return an imenu label for the XML element at point.
Return nil for closing tags, comments, declarations, attribute
continuation lines, and text-only lines, so the index follows the
element structure instead of every physical line."
  (let ((line (string-trim (buffer-substring-no-properties
                            (line-beginning-position)
                            (line-end-position)))))
    (when (and (not (nth 4 (syntax-ppss)))
               (string-match "\\`<\\([[:alpha:]_][^[:space:]/>]*\\)" line))
      (let ((tag (match-string 1 line)))
        (cond
         ;; Complete element on one line (text or self-closing): keep
         ;; the whole line so attribute values and text stay visible.
         ((or (string-match "/>[[:space:]]*\\'" line)
              (string-match ">[^<]*</[^>]+>[[:space:]]*\\'" line))
          line)
         ;; First line of a multi-line start tag: include an
         ;; identifying attribute even when it is on a later line.
         (t
          (let ((tag-text (if (string-match ">" line)
                              line
                            (or (my/nxml-outline-imenu-tag-text) line))))
            (if (string-match "[[:space:]]\\(?:id\\|name\\)=\"[^\"]*\""
                              tag-text)
                (format "<%s%s>" tag (match-string 0 tag-text))
              (format "<%s>" tag)))))))))

(defun my/outline-imenu-create-index ()
  "Create a flat imenu index from the buffer's outline headings.
Names include ancestor headings so every heading, including
parents, is directly jumpable."
  (let ((heading-function (or my/outline-imenu-heading-function
                              #'my/outline-imenu-default-heading))
        (index nil)
        (stack nil))
    (save-excursion
      (goto-char (point-min))
      (let ((collect
             (lambda ()
               (let ((level (funcall outline-level))
                     (marker (point-marker))
                     (label (funcall heading-function)))
                 (when label
                   (while (and stack (>= (caar stack) level))
                     (pop stack))
                   (push (cons (if stack
                                   (concat (mapconcat #'cdr (reverse stack)
                                                      imenu-level-separator)
                                           imenu-level-separator
                                           label)
                                 label)
                               marker)
                         index)
                   (push (cons level label) stack))))))
        (when (outline-on-heading-p) (funcall collect))
        (while (outline-next-heading) (funcall collect))))
    (nreverse index)))

(defcustom my/outline-imenu-preview-delay 0.1
  "Idle seconds before previewing the highlighted outline section.
Set to 0 to preview on every minibuffer command."
  :type 'number
  :group 'imenu)

;; Live preview state, dynamically bound around `completing-read' so the
;; minibuffer post-command hook can see it.
(defvar my/outline-imenu--preview-items nil
  "Alist of (display-name . marker) for the active preview session.")
(defvar my/outline-imenu--preview-origin nil
  "List (WINDOW BUFFER POINT WINDOW-START) captured before the prompt.")
(defvar my/outline-imenu--preview-last nil
  "Last marker previewed; used to skip redundant redraws.")
(defvar my/outline-imenu--preview-timer nil
  "Pending idle timer for the next preview update.")
(defvar my/outline-imenu--preview-folds nil
  "Outline overlays captured before previewing, as (START . END) pairs.")

(defun my/outline-imenu--snapshot-folds (buffer)
  "Return a list of (START . END) outline overlays in BUFFER."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (delq nil
            (mapcar (lambda (overlay)
                      (when (eq (overlay-get overlay 'invisible) 'outline)
                        (cons (overlay-start overlay) (overlay-end overlay))))
                    (overlays-in (point-min) (point-max)))))))

(defun my/outline-imenu--restore-folds (buffer folds)
  "Restore outline FOLDS in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (remove-overlays (point-min) (point-max) 'invisible 'outline)
        (dolist (range folds)
          (let ((overlay (make-overlay (car range) (cdr range)
                                       nil 'front-advance)))
            (overlay-put overlay 'evaporate t)
            (overlay-put overlay 'invisible 'outline)
            (overlay-put overlay 'isearch-open-invisible
                         (or (bound-and-true-p outline-isearch-open-invisible-function)
                             #'outline-isearch-open-invisible))))))))

(defun my/outline-imenu--preview-candidate ()
  "Return the completion candidate currently highlighted in the minibuffer."
  (or (and (boundp 'completion-all-sorted-completions)
           (bound-and-true-p icomplete-mode)
           completion-all-sorted-completions
           (car completion-all-sorted-completions))
      (and (minibufferp) (minibuffer-contents-no-properties))))

(defun my/outline-imenu--preview-update ()
  "Show the highlighted outline section in the origin window."
  (when (and (minibufferp)
             my/outline-imenu--preview-items
             my/outline-imenu--preview-origin)
    (let* ((candidate (ignore-errors (my/outline-imenu--preview-candidate)))
           (marker (and candidate
                        (cdr (assoc candidate my/outline-imenu--preview-items)))))
      (when (and (markerp marker)
                 (not (equal marker my/outline-imenu--preview-last))
                 (window-live-p (car my/outline-imenu--preview-origin)))
        (setq my/outline-imenu--preview-last marker)
        (with-selected-window (car my/outline-imenu--preview-origin)
          (let ((buffer (marker-buffer marker)))
            (when (buffer-live-p buffer)
              (switch-to-buffer buffer t t)
              (goto-char marker)
              (when (outline-on-heading-p t)
                (outline-show-entry))
              (recenter))))))))

(defun my/outline-imenu--preview-schedule ()
  "Schedule a preview after `my/outline-imenu-preview-delay' idle seconds."
  (when (timerp my/outline-imenu--preview-timer)
    (cancel-timer my/outline-imenu--preview-timer))
  (setq my/outline-imenu--preview-timer
        (run-with-idle-timer my/outline-imenu-preview-delay nil
                             #'my/outline-imenu--preview-update)))

(defun my/outline-imenu--preview-cleanup ()
  "Cancel any pending preview and restore the origin window state."
  (when (timerp my/outline-imenu--preview-timer)
    (cancel-timer my/outline-imenu--preview-timer)
    (setq my/outline-imenu--preview-timer nil))
  (pcase my/outline-imenu--preview-origin
    (`(,win ,buf ,pt ,start)
     (when (and (window-live-p win) (buffer-live-p buf))
       (my/outline-imenu--restore-folds buf my/outline-imenu--preview-folds)
       (with-selected-window win
         (switch-to-buffer buf t t)
         (goto-char pt)
         (set-window-start win start))))))

(defun my/outline-imenu--choose (index-alist &optional prompt)
  "Read an outline index entry from INDEX-ALIST with live preview.
PROMPT is the minibuffer prompt.  Return the chosen cons cell, or nil."
  (let* ((win (selected-window))
         (my/outline-imenu--preview-items index-alist)
         (my/outline-imenu--preview-origin
          (list win (window-buffer win) (window-point win) (window-start win)))
         (my/outline-imenu--preview-last nil)
         (my/outline-imenu--preview-folds
          (my/outline-imenu--snapshot-folds (window-buffer win)))
         (imenu-space-replacement nil)
         (choice nil))
    (minibuffer-with-setup-hook
        (lambda ()
          (add-hook 'post-command-hook #'my/outline-imenu--preview-schedule nil t)
          (add-hook 'minibuffer-exit-hook #'my/outline-imenu--preview-cleanup nil t)
          (my/outline-imenu--preview-schedule))
      (setq choice
            (completing-read
             (or prompt "Outline section: ")
             (lambda (string pred action)
               (if (eq action 'metadata)
                   '(metadata (category . imenu))
                 (complete-with-action action index-alist string pred)))
             nil t nil 'imenu--history-list)))
    (if (stringp choice)
        (assoc choice index-alist)
      choice)))

(defun my/outline-imenu--completion-buffer (orig index-alist &optional prompt)
  "Use `my/outline-imenu--choose' for outline-based imenu indexes."
  (if (eq imenu-create-index-function #'my/outline-imenu-create-index)
      (my/outline-imenu--choose index-alist prompt)
    (funcall orig index-alist prompt)))

(require 'imenu)
(unless (advice-member-p #'my/outline-imenu--completion-buffer
                         'imenu--completion-buffer)
  (advice-add 'imenu--completion-buffer :around
              #'my/outline-imenu--completion-buffer))

(defun my/outline-imenu ()
  "Jump to a heading using the current outline structure.
The highlighted candidate is previewed in the source window
before the jump."
  (interactive)
  (let ((imenu-create-index-function #'my/outline-imenu-create-index)
        (imenu-generic-expression nil)
        (imenu--index-alist nil)
        (imenu-auto-rescan t)
        (imenu-max-item-length nil)
        (imenu-space-replacement nil))
    (call-interactively #'imenu)))

(defun my/outline-imenu-setup ()
  "Use outline headings for `imenu' in XML buffers.
Other modes keep their native imenu index; `my/outline-imenu' is
available everywhere for an outline-based index."
  (if outline-indent-minor-mode
      (when (derived-mode-p 'nxml-mode)
        (setq-local my/outline-imenu-heading-function
                    #'my/nxml-outline-imenu-heading)
        (setq-local imenu-create-index-function
                    #'my/outline-imenu-create-index)
        (setq-local imenu-auto-rescan t)
        (setq-local imenu-max-item-length nil)
        (setq-local imenu-space-replacement nil))
    (when (and (boundp 'imenu-create-index-function)
               (eq imenu-create-index-function #'my/outline-imenu-create-index))
      (kill-local-variable 'imenu-create-index-function)
      (kill-local-variable 'imenu-auto-rescan)
      (kill-local-variable 'imenu-max-item-length)
      (kill-local-variable 'imenu-space-replacement)
      (kill-local-variable 'my/outline-imenu-heading-function))))

(defun my/outline-imenu-reveal-heading ()
  "Reveal the outline heading at point after an imenu jump."
  (when (and (bound-and-true-p outline-indent-minor-mode)
             (outline-on-heading-p t))
    (outline-show-entry)
    (recenter)))

(add-hook 'imenu-after-jump-hook #'my/outline-imenu-reveal-heading)
(add-hook 'outline-indent-minor-mode-hook #'my/outline-imenu-setup)

(require 'transient)

(transient-define-prefix outline-indent-transient ()
  "Outline / outline-indent folding, navigation, and structure commands."
  :transient-non-suffix #'transient--do-stay
  [["Fold at point"
    ("TAB" "Toggle level"         outline-indent-toggle-level-at-point)
    ("o"   "Toggle recursive"     my/outline-toggle-recursive)
    ("h"   "Close fold"           outline-indent-close-fold)
    ("s"   "Show subtree"         outline-show-subtree)]
   ["Whole buffer"
    ("a"   "Open all folds"       outline-indent-open-folds)
    ("m"   "Close all folds"      outline-indent-close-folds)
    ("l"   "Hide to N levels"     my/outline-hide-sublevels-prompt)
    ("k"   "Isolate (hide other)" outline-hide-other)]
   ["Navigate"
    ("n"   "Next heading"         outline-next-visible-heading)
    ("p"   "Prev heading"         outline-previous-visible-heading)
    ("f"   "Fwd same level"       outline-indent-forward-same-level)
    ("b"   "Back same level"      outline-indent-backward-same-level)
    ("u"   "Up heading"           outline-up-heading)
    ("i"   "Jump to section"      my/outline-imenu)]
   ["Structure / Isolate"
    (">"   "Shift right"          outline-indent-shift-right)
    ("<"   "Shift left"           outline-indent-shift-left)
    ("v"   "Select block"         outline-indent-select)
    ("N"   "Narrow to block"      outline-indent-narrow)
    ("w"   "Widen"                widen)]
   ])

(with-eval-after-load 'outline-indent
  (define-key outline-indent-minor-mode-map (kbd "C-c o") #'outline-indent-transient)
  ;; Global fold/unfold across every outline-indent buffer (web-mode muscle memory).
  ;; TAB keeps its standard indentation command in those buffers; folding is
  ;; available here and on `C-c o'.
  (define-key outline-indent-minor-mode-map (kbd "C-c C-f") #'outline-cycle))

(defun my/elisp-outline-level ()
  "Outline level of the heading at point.
Section headers (\";; -> ...\" and \";;; ...\") are level 1,
definitions and top-level forms are level 2."
  (if (looking-at "^(def\\|^(cl-def\\|^(use-package\\|^(with-eval-after-load")
      2
    1))

(defun my/elisp-outline-fold-setup ()
  "Make section headers and top-level forms foldable in elisp buffers.
`outline-indent' derives heading levels from indentation alone, so in a
flat elisp file every non-blank line is a level-1 heading: the subtree
of a comment header ends at the very next line and has nothing to hide.
Fall back to regexp-driven headings instead: \";; -> ...\" and \";;;\"
comment headers fold their section, definitions fold to the next one."
  (setq-local outline-regexp
              "^;;;\\|^;; +-> \\|^(def\\|^(cl-def\\|^(use-package\\|^(with-eval-after-load")
  (kill-local-variable 'outline-search-function)
  (setq-local outline-level #'my/elisp-outline-level))

(add-hook 'emacs-lisp-mode-hook #'my/elisp-outline-fold-setup)
(add-hook 'outline-indent-minor-mode-hook
          (lambda ()
            (when (and outline-indent-minor-mode
                       (derived-mode-p 'emacs-lisp-mode))
              (my/elisp-outline-fold-setup))))

(use-package simply-annotate
  :load-path "~/source/repos/simply-annotate"
  :hook (find-file-hook . simply-annotate-mode)
  :config
  (let ((my-ms-map (make-sparse-keymap)))
    (set-keymap-parent my-ms-map simply-annotate-command-map)
    (define-key my-ms-map (kbd "u")
                (lambda () (interactive) (my/tab-bar-push-window -1)))
    (define-key my-ms-map (kbd "i")
                (lambda () (interactive) (my/tab-bar-push-window 1)))
    (define-key my-overrides-mode-map (kbd "M-s") my-ms-map))
  (require 'posframe)
  (setq simply-annotate-inline-position 'margin-right)
  (setq simply-annotate-tint-amount 50)
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
  (setq simply-annotate-display-style '(bracket))
  ;; (setq simply-annotate-inline-default t)
  )

(with-eval-after-load 'simply-annotate
  (add-hook 'dired-mode-hook #'simply-annotate-dired-mode))

(use-package chess
  :defer t)

(setq chess-images-separate-frame nil)
(setq chess-images-default-size 64)

(use-package gnuplot
  :defer t)

(use-package zig-mode
  :mode "\\.zig\\'")

(load-theme 'doom-material-dark t)
;; (load-theme 'doom-outrun-electric t)
;; (load-theme 'doom-manegarm t)
;; (load-theme 'ef-trio-dark t)

(define-key my-win-keymap (kbd "m") #'diff-minimap-toggle)

;;
;; -> vlf — view very large files (e.g. SVN dumps) in batches
;;
;; Usage:
;;   M-x vlf <file>  — open any file in ~1 MiB chunks (works on multi-GB files)
;;   V in dired      — open the file at point with VLF (wired up by vlf-setup)
;;   C-c V           — open with VLF from anywhere (binding below)
;; In a VLF buffer: n/p next/prev chunk, [/] first/last chunk, +/- resize
;; chunk, s/r search forward/back over the whole file, o occur, l go to
;; line, C-c C-v prefix for all VLF keys. Scrolling past the buffer limits
;; auto-advances to the next/previous chunk.
;;
(use-package vlf
  :ensure t
  :custom
  ;; Offer VLF when a file exceeds `large-file-warning-threshold'
  ;; (default 10 MiB). Change to 'dont-ask to jump straight into VLF.
  (vlf-application 'ask)
  ;; Chunk size for local files. Auto-tuned at runtime (vlf-tune-enabled).
  ;; 1 MiB is a good balance for line-oriented ASCII like SVN dumps.
  (vlf-batch-size 1048576)
  (vlf-tune-enabled t)
  :config
  ;; Hooks VLF into `abort-if-file-too-large' and defines `dired-vlf'.
  (require 'vlf-setup)
  ;; SVN dumps / generic dumps: keep them in fundamental-mode so no slow
  ;; major-mode or font-lock runs over the chunk.
  (add-to-list 'auto-mode-alist '("\\.svndump\\'" . fundamental-mode))
  (add-to-list 'auto-mode-alist '("\\.dump\\'" . fundamental-mode))
  ;; Safety net for very long single lines inside a chunk.
  (when (fboundp 'global-so-long-mode)
    (global-so-long-mode 1))
  (define-key my-overrides-mode-map (kbd "C-c V") #'vlf)
  (with-eval-after-load 'dired
    (define-key dired-mode-map (kbd "V") #'dired-vlf)))

;; (use-package completion-preview
;;   :ensure nil
;;   :demand t
;;   :bind
;;   (:map completion-preview-active-mode-map
;;         ("M-i" . completion-preview-insert-word)
;;         ("M-n" . completion-preview-next-candidate)
;;         ("M-p" . completion-preview-prev-candidate)
;;         ("M-<return>" . completion-preview-insert)
;;         ;; With TAB we effectively defer to the *Completions* buffer to
;;         ;; show more completion candidates at once.
;;         ("<tab>" . completion-preview-complete))
;;   :config
;;   (setq completion-preview-minimum-symbol-length 2)
;;   (global-completion-preview-mode 1))
