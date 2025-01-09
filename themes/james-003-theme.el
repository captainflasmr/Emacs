;;; james-003-theme.el --- james-003
;;; Version: 1.0
;;; Commentary:
;;; A theme called james-003
;;; Code:

(deftheme james-003 "DOCSTRING for james-003")
  (custom-theme-set-faces 'james-003
   '(default ((t (:foreground "#242121" :background "#ffffff" ))))
   '(cursor ((t (:background "#000000" ))))
   '(fringe ((t (:background "#eef0f0" ))))
   '(mode-line ((t (:foreground "#ffffff" :background "#6f8784" ))))
   '(region ((t (:background "#cccccc" ))))
   '(secondary-selection ((t (:background "#cddbec" ))))
   '(font-lock-builtin-face ((t (:foreground "#738aa1" ))))
   '(font-lock-comment-face ((t (:foreground "#7d827d" ))))
   '(font-lock-function-name-face ((t (:foreground "#375a0d" ))))
   '(font-lock-keyword-face ((t (:foreground "#105163" ))))
   '(font-lock-string-face ((t (:foreground "#4c7685" ))))
   '(font-lock-type-face ((t (:foreground "#659915" ))))
   '(font-lock-constant-face ((t (:foreground "#456b48" ))))
   '(font-lock-variable-name-face ((t (:foreground "#ac8d4b" ))))
   '(minibuffer-prompt ((t (:foreground "#7299ff" :bold t ))))
   '(font-lock-warning-face ((t (:foreground "red" :bold t ))))
   )

;;;###autoload
(and load-file-name
    (boundp 'custom-theme-load-path)
    (add-to-list 'custom-theme-load-path
                 (file-name-as-directory
                  (file-name-directory load-file-name))))
;; Automatically add this theme to the load path

(provide-theme 'james-003)

;;; james-003-theme.el ends here
