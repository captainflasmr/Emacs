;;; james-004-theme.el --- james-004
;;; Version: 1.0
;;; Commentary:
;;; A theme called james-004
;;; Code:

(deftheme james-004 "DOCSTRING for james-004")
  (custom-theme-set-faces 'james-004
   '(default ((t (:foreground "#d9d521" :background "#080851" ))))
   '(cursor ((t (:background "#fdf4c1" ))))
   '(fringe ((t (:background "#282828" ))))
   '(mode-line ((t (:foreground "#ece09f" :background "#1e1c1a" ))))
   '(region ((t (:background "#003172" ))))
   '(secondary-selection ((t (:background "#4c7588" ))))
   '(font-lock-builtin-face ((t (:foreground "#fe8019" ))))
   '(font-lock-comment-face ((t (:foreground "#7c6f64" ))))
   '(font-lock-function-name-face ((t (:foreground "#a99865" ))))
   '(font-lock-keyword-face ((t (:foreground "#dd6f48" ))))
   '(font-lock-string-face ((t (:foreground "#429489" ))))
   '(font-lock-type-face ((t (:foreground "#66999d" ))))
   '(font-lock-constant-face ((t (:foreground "#bbaa97" ))))
   '(font-lock-variable-name-face ((t (:foreground "#83a598" ))))
   '(minibuffer-prompt ((t (:foreground "#61acbb" :bold t ))))
   '(font-lock-warning-face ((t (:foreground "red" :bold t ))))
   )

;;;###autoload
(and load-file-name
    (boundp 'custom-theme-load-path)
    (add-to-list 'custom-theme-load-path
                 (file-name-as-directory
                  (file-name-directory load-file-name))))
;; Automatically add this theme to the load path

(provide-theme 'james-004)

;;; james-004-theme.el ends here
