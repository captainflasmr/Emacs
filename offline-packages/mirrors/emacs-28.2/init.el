;; -*- lexical-binding: t; -*-
;;
;; Rendered from init.el.in by create-install.sh for Emacs 28.2.
;; Do not edit the rendered init.el by hand — edit the template or the
;; packages/emacs-<VER>.el list and re-render.

;;
;; -> core-configuration
;;
(let ((f (expand-file-name "~/.emacs.d/Emacs-vanilla/init.el")))
  (when (file-exists-p f) (load-file f)))

;;
;; -> package-archives
;;
(require 'package)

;; Presence of ~/elpa-mirror-emacs-*/ selects the local mirror; absence
;; falls through to online archives. Force-disable by setting this to nil.
(setq my/offline-packages t)

(setq my/offline-packages-dir
      (car (sort (file-expand-wildcards
                  (expand-file-name "~/elpa-mirror-emacs-*"))
                 #'string>)))

(message "my/offline-packages=%s  dir=%s"
         my/offline-packages
         (or my/offline-packages-dir "<none found>"))

(cond
 ((and my/offline-packages my/offline-packages-dir)
  (setq package-archives
        `(("local" . ,(file-name-as-directory my/offline-packages-dir)))))
 ((eq system-type 'gnu/linux)
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("elpa"  . "https://elpa.gnu.org/packages/")
                           ("org"   . "https://orgmode.org/elpa/")))))

(package-initialize)
(setq load-prefer-newer t)

(defun my/ensure-package (pkg)
  "Install PKG from configured archives if it is not already present.
Failures (e.g. package requires a newer Emacs) are logged, not fatal."
  (unless (package-installed-p pkg)
    (unless package-archive-contents
      (package-refresh-contents))
    (condition-case err
        (package-install pkg)
      (error (message "my/ensure-package: skipping %s (%s)" pkg err)))))

;;
;; -> package-list (injected)
;;
(dolist (pkg '(async use-package i3wm-config-mode yaml-mode doom-themes gruvbox-theme timu-caribbean-theme timu-spacegrey-theme timu-rouge-theme csv-mode selected-window-accent-mode simply-annotate ztree web-mode))
  (my/ensure-package pkg))

;;
;; -> custom-faces
;;
(condition-case err
    (custom-set-faces
     '(default ((t (:family "Monospace" :foundry "ADBO" :slant normal :weight regular :height 100 :width normal))))
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
  (error (message "custom-set-faces skipped: %s" err)))
