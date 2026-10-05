;;; early-init.el --- Pre-startup frame settings -*- lexical-binding: t; -*-
(add-to-list 'default-frame-alist '(background-color . "#1e1e1e"))

;;
;; -> frame-chrome
;;
;; Disable menu/tool/scroll bars before the first frame is created
;; (startup.el creates it after this file loads), otherwise they flash
;; on screen during startup.  The mode calls in Emacs-vanilla/init.el
;; stay for manually-loaded/batch use.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;;
;; -> startup-performance
;;
;; Startup loads hundreds of libraries, so the default GC threshold
;; (800k) forces ~70 collections before Emacs is up.  Raise it while
;; loading and restore sane values once startup finishes.  Likewise,
;; skip `file-name-handler-alist' lookups (TRAMP/EPA/etc.) during
;; startup; handlers registered while loading are merged back in.
;;
;; This is for a normal in-process startup -- no daemon/server involved.
(defvar my--file-name-handler-alist file-name-handler-alist
  "Value of `file-name-handler-alist' before startup optimisation.")

(setq file-name-handler-alist nil
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Keep any handler that was registered during startup too.
            (dolist (handler file-name-handler-alist)
              (unless (member handler my--file-name-handler-alist)
                (push handler my--file-name-handler-alist)))
            (setq file-name-handler-alist my--file-name-handler-alist
                  gc-cons-threshold (* 64 1024 1024)
                  gc-cons-percentage 0.1)))
