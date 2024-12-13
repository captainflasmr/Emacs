;;
;; -> package-archives
;;

(require 'package)

(when (eq system-type 'gnu/linux)
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("elpa" . "https://elpa.gnu.org/packages/")
                           ("org" . "https://orgmode.org/elpa/"))))

(when (eq system-type 'windows-nt)
  (setq package-archives '(("melpa" . "~/emacs-pkgs/melpa")
                           ("elpa" . "~/emacs-pkgs/elpa")
                           ("org" . "~/emacs-pkgs/org-mode/lisp"))))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(require 'use-package)
(setq load-prefer-newer t)

;; (when init-file-debug
(setq use-package-verbose t
      use-package-expand-minimally nil
      use-package-compute-statistics t
      debug-on-error nil)

;;
;; -> requires-core
;;
(require 'org)
(require 'grep)
(require 'bookmark)
(require 'dired)

;;
;; -> completion-core
;;
(setq-default abbrev-mode t)
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs try-expand-dabbrev
        try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially try-complete-lisp-symbol))

;;
;; -> modeline-completion-core
;;
(fido-mode 1)
(defun my-icomplete-exit-minibuffer-with-input ()
  "Exit the minibuffer with the current input, without forcing completion."
  (interactive)
  (exit-minibuffer))
(define-key icomplete-minibuffer-map (kbd "M-RET") 'my-icomplete-exit-minibuffer-with-input)
(setq icomplete-compute-delay 0)
(setq icomplete-show-matches-on-no-input t)

;;
;; -> keys-navigation-core
;;
(defvar my-jump-keymap (make-sparse-keymap))
(global-set-key (kbd "M-o") my-jump-keymap)
(define-key my-jump-keymap (kbd "=") #'tab-bar-new-tab)
(define-key my-jump-keymap (kbd "b") (lambda () (interactive) (find-file "~/bin")))
(define-key my-jump-keymap (kbd "e")
            (lambda ()
              (interactive)
              (find-file (expand-file-name "init.el" user-emacs-directory))))
(define-key my-jump-keymap (kbd "f") #'find-name-dired)
(define-key my-jump-keymap (kbd "g") (lambda () (interactive) (find-file "~/.config")))
(define-key my-jump-keymap (kbd "h") (lambda () (interactive) (find-file "~")))
(define-key my-jump-keymap (kbd "j") (lambda () (interactive) (find-file "~/DCIM/content/aaa--todo.org")))
(define-key my-jump-keymap (kbd "k")
            (lambda () (interactive)
              (find-file (concat user-emacs-directory "emacs--core.org"))))
(define-key my-jump-keymap (kbd "l") #'my/recentf-open)
(define-key my-jump-keymap (kbd "m") #'customize-themes)
(define-key my-jump-keymap (kbd "n") (lambda () (interactive) (find-file "~/nas")))
(define-key my-jump-keymap (kbd "o") #'bookmark-jump)
(define-key my-jump-keymap (kbd "r") (lambda () (interactive) (switch-to-buffer "*scratch*")))
(define-key my-jump-keymap (kbd "w") (lambda () (interactive) (find-file "~/DCIM/content/")))
(define-key my-jump-keymap (kbd "-") #'tab-close)
;;
(defun my/quick-window-jump ()
  "Jump to a window by typing its assigned character label.
If there are only two windows, jump directly to the other window."
  (interactive)
  (let* ((window-list (window-list nil 'no-mini)))
    (if (= (length window-list) 2)
        ;; If there are only two windows, switch to the other one directly.
        (select-window (other-window-for-scrolling))
      ;; Otherwise, show the key selection interface.
      (let* ((my/quick-window-overlays nil)
             (sorted-windows (sort window-list
                                   (lambda (w1 w2)
                                     (let ((edges1 (window-edges w1))
                                           (edges2 (window-edges w2)))
                                       (or (< (car edges1) (car edges2))
                                           (and (= (car edges1) (car edges2))
                                                (< (cadr edges1) (cadr edges2))))))))
             (window-keys (seq-take '("j" "k" "l" ";" "a" "s" "d" "f")
                                    (length sorted-windows)))
             (window-map (cl-pairlis window-keys sorted-windows)))
        (setq my/quick-window-overlays
              (mapcar (lambda (entry)
                        (let* ((key (car entry))
                               (window (cdr entry))
                               (start (window-start window))
                               (overlay (make-overlay start start (window-buffer window))))
                          (overlay-put overlay 'after-string 
                                       (propertize (format "[%s]" key)
                                                   'face '(:foreground "white" :background "blue" :weight bold)))
                          (overlay-put overlay 'window window)
                          overlay))
                      window-map))
        (let ((key (read-key (format "Select window [%s]: " (string-join window-keys ", ")))))
          (mapc #'delete-overlay my/quick-window-overlays)
          (setq my/quick-window-overlays nil)
          (when-let ((selected-window (cdr (assoc (char-to-string key) window-map))))
            (select-window selected-window)))))))
;;
(global-set-key (kbd "M-a") #'my/quick-window-jump)

;;
;; -> keys-visual-core
;;
(add-hook 'text-mode-hook 'visual-line-mode)

;;
;; -> keys-visual-core
;;
(defvar my-win-keymap (make-sparse-keymap))
(global-set-key (kbd "C-q") my-win-keymap)
(define-key my-win-keymap (kbd "c") #'display-fill-column-indicator-mode)
(define-key my-win-keymap (kbd "d") #'window-divider-mode)
(define-key my-win-keymap (kbd "e") #'whitespace-mode)
(define-key my-win-keymap (kbd "f") #'font-lock-mode)
(define-key my-win-keymap (kbd "h") #'global-hl-line-mode)
(define-key my-win-keymap (kbd "k") #'my/toggle-mode-line)
(define-key my-win-keymap (kbd "l") #'my/sync-tab-bar-to-theme)
(define-key my-win-keymap (kbd "m") #'my/load-theme)
(define-key my-win-keymap (kbd "n") #'display-line-numbers-mode)
(define-key my-win-keymap (kbd "o") #'toggle-centered-buffer)
(define-key my-win-keymap (kbd "p") #'variable-pitch-mode)
(define-key my-win-keymap (kbd "q") #'toggle-menu-bar-mode-from-frame)
(define-key my-win-keymap (kbd "s") #'my/toggle-internal-border-width)
(define-key my-win-keymap (kbd "u") #'set-cursor-color)
(define-key my-win-keymap (kbd "v") #'visual-line-mode)
(define-key my-win-keymap (kbd "b") #'(lambda () (interactive)(tab-bar-mode 'toggle)))

;;
;; -> keys-other-core
;;
(global-set-key (kbd "M-s ,") #'my/mark-line)
(global-set-key (kbd "M-s g") #'my/grep)
(global-set-key (kbd "M-s h") #'my/mark-block)
(global-set-key (kbd "M-s j") #'eval-defun)
(global-set-key (kbd "M-s l") #'eval-expression)
(global-set-key (kbd "M-s =") #'ediff-buffers)
(global-set-key (kbd "M-s w") #'(lambda ()(interactive)
                                  (org-html-export-to-html)
                                  (my/html-promote-headers)
                                  (my/html-org-table-highlight)))
(global-set-key (kbd "M-s e") #'(lambda ()(interactive)
                                  (org-odt-export-to-odt)
                                  (async-shell-command
                                   "libreoffice --headless --convert-to docx confluence--setup-sles.odt" "*create-docs*")))
(global-set-key (kbd "M-s ;") #'my/copy-buffer-to-kill-ring)

;;
;; -> keybinding-core
;;
(global-set-key (kbd "C-=") (lambda ()(interactive)(text-scale-adjust 1)))
(global-set-key (kbd "C--") (lambda ()(interactive)(text-scale-adjust -1)))
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c h") #'my/shell-create)
(global-set-key (kbd "C-c j") #'my/repeat-window-size)
(global-set-key (kbd "C-c o h") #'outline-hide-sublevels)
(global-set-key (kbd "C-c o s") #'outline-show-all)
(global-set-key (kbd "C-x ;") #'my/switch-to-thing)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x [") #'beginning-of-buffer)
(global-set-key (kbd "C-x ]") #'end-of-buffer)
(global-set-key (kbd "C-x j") #'(lambda() (interactive)(tab-bar-history-back)(my/repeat-history)))
(global-set-key (kbd "C-x k") #'(lambda() (interactive)(tab-bar-history-forward)(my/repeat-history)))
(global-set-key (kbd "C-x l") #'scroll-lock-mode)
(global-set-key (kbd "C-x m") #'my/switch-to-thing)
(global-set-key (kbd "C-x s") #'save-buffer)
(global-set-key (kbd "C-x v e") 'vc-ediff)
(global-set-key (kbd "C-x x g") #'revert-buffer)
(global-set-key (kbd "C-x x t") #'toggle-truncate-lines)
(global-set-key (kbd "M-z") #'my/comment-or-uncomment)
(global-set-key (kbd "C-z") #'my/comment-or-uncomment)
(global-set-key (kbd "M-c") #'delete-other-windows)
(global-set-key (kbd "M-'") #'set-mark-command)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") #'delete-other-windows)
(global-set-key (kbd "M-2") #'split-window-vertically)
(global-set-key (kbd "M-3") #'split-window-horizontally)
(global-set-key (kbd "M-9") #'hippie-expand)
(global-set-key (kbd "M-;") 'delete-other-windows)
(global-set-key (kbd "M-[") #'yank)
(global-set-key (kbd "M-]") #'yank-pop)
(global-set-key (kbd "M-e") #'dired-jump)
(global-set-key (kbd "M-g i") 'imenu)
(global-set-key (kbd "M-i") #'tab-bar-switch-to-next-tab)
(global-set-key (kbd "M-j") #'(lambda ()(interactive)(scroll-up (/ (window-height) 4))))
(global-set-key (kbd "M-k") #'(lambda ()(interactive)(scroll-down (/ (window-height) 4))))
(global-set-key (kbd "M-l") #'split-window-horizontally)
(global-set-key (kbd "M-m") #'split-window-vertically)
(global-set-key (kbd "M-u") #'tab-bar-switch-to-prev-tab)
(global-unset-key (kbd "C-h h"))
(global-unset-key (kbd "C-t"))
(with-eval-after-load 'vc-dir
  (define-key vc-dir-mode-map (kbd "e") #'vc-ediff))

;;
;; -> modes-core
;;
(column-number-mode 1)
(desktop-save-mode -1)
(display-time-mode -1)
(global-auto-revert-mode t)
(savehist-mode 1)
(show-paren-mode t)
(tab-bar-history-mode 1)
(global-font-lock-mode t)
(server-mode 1)

;;
;; -> bell-core
;;
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;;
;; -> setqs-core
;;
(setq completion-styles '(basic partial-completion emacs22))
(setq custom-safe-themes t)
(setq delete-selection-mode nil)
(setq enable-local-variables :all)
(setq frame-title-format "%f")
(setq kill-whole-line t)
(setq-default truncate-lines t)
(setq frame-inhibit-implied-resize t)
(setq native-comp-async-report-warnings-errors nil)

;;
;; -> confirm-core
;;
(defalias 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'y-or-n-p)
(setq confirm-kill-processes nil)
(setq confirm-nonexistent-file-or-buffer nil)
(set-buffer-modified-p nil)

;;
;; -> backups-core
;;
(setq make-backup-files 1)
(setq backup-directory-alist '(("." . "~/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 10   ; how many of the newest versions to keep
      kept-old-versions 5)   ; and how many of the old

;;
;; -> custom-settings-core
;;
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit default :weight regular :height 1.0))))
 '(org-level-2 ((t (:inherit default :weight light :height 1.0))))
 '(org-level-3 ((t (:inherit default :weight light :height 1.0))))
 '(org-level-4 ((t (:inherit default :weight light :height 1.0))))
 '(org-level-5 ((t (:inherit default :weight light :height 1.0))))
 '(org-level-6 ((t (:inherit default :weight light :height 1.0))))
 '(ediff-current-diff-A ((t (:extend t :background "#b5daeb" :foreground "#000000"))))
 '(ediff-even-diff-A ((t (:background "#bafbba" :foreground "#000000" :extend t))))
 '(ediff-fine-diff-A ((t (:background "#f4bd92" :foreground "#000000" :extend t))))
 '(ediff-odd-diff-A ((t (:background "#b8fbb8" :foreground "#000000" :extend t))))
 '(font-lock-warning-face ((t (:foreground "#930000" :inverse-video nil))))
 '(org-link ((t (:underline nil))))
 '(indent-guide-face ((t (:background "#282828" :foreground "#666666"))))
 '(widget-button ((t (:inherit fixed-pitch :weight regular))))
 '(window-divider ((t (:foreground "black"))))
 '(org-tag ((t (:height 0.99))))
 '(vertical-border ((t (:foreground "#000000")))))
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(wombat))
 '(warning-suppress-log-types '((frameset)))
 '(warning-suppress-types '((frameset))))

;;
;; -> defun-core
;;
(defun save-macro (name)
  "Save a macro by NAME."
  (interactive "SName of the macro: ")
  (kmacro-name-last-macro name)
  (find-file user-init-file)
  (goto-char (point-max))
  (newline)
  (insert-kbd-macro name)
  (newline))
;;
(defun my/comment-or-uncomment ()
  "Comments or uncomments the current line or region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region
       (region-beginning)(region-end))
    (comment-or-uncomment-region
     (line-beginning-position)(line-end-position))))
;;
(defun my/dired-duplicate-file (arg)
  "Duplicate a file from DIRED with an incremented number.
                                If ARG is provided, it sets the counter."
  (interactive "p")
  (let* ((file (dired-get-file-for-visit))
         (dir (file-name-directory file))
         (name (file-name-nondirectory file))
         (base-name (file-name-sans-extension name))
         (extension (file-name-extension name t))
         (counter (if arg (prefix-numeric-value arg) 1))
         (new-file))
    (while (and (setq new-file
                      (format "%s%s_%03d%s" dir base-name counter extension))
                (file-exists-p new-file))
      (setq counter (1+ counter)))
    (if (file-directory-p file)
        (copy-directory file new-file)
      (copy-file file new-file))
    (dired-revert)))
;;
(defun my/mark-line ()
  "Mark whole line."
  (interactive)
  (beginning-of-line)
  (push-mark (point) nil t)
  (end-of-line))
;;
(defun my/mark-block ()
  "Marking a block of text surrounded by a newline."
  (interactive)
  (when (not (region-active-p))
    (backward-char))
  (skip-chars-forward " \n\t")
  (re-search-backward "^[ \t]*\n" nil 1)
  (skip-chars-forward " \n\t")
  (when (not (region-active-p))
    (push-mark))
  (re-search-forward "^[ \t]*\n" nil 1)
  (skip-chars-backward " \n\t")
  (setq mark-active t))
;;
(defun my/repeat-history ()
  ""
  (interactive)
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "j") (lambda () (interactive)
                                (tab-bar-history-back)))
    (define-key map (kbd "k") (lambda () (interactive)
                                (tab-bar-history-forward)))
    (set-transient-map map t)))
;;
(defun my/get-window-position ()
  "Return the position of the current window as 'left', 'right', 'top', or 'bottom'."
  (let* ((edges (window-edges))
         (min-x (nth 0 edges))
         (min-y (nth 1 edges))
         (max-x (nth 2 edges))
         (max-y (nth 3 edges))
         (frame-width (frame-width))
         (frame-height (frame-height)))
    (cond
     ((<= min-x 0) 'left)
     ((>= max-x frame-width) 'right)
     ((= min-y 0) 'top)
     ((= max-y frame-height) 'bottom)
     (t 'center))))
;;
(defun my/adaptive-resize (horizontal delta)
  "Resize the current window adaptively based on its position.
HORIZONTAL is non-nil for horizontal resizing (left/right).
DELTA is the amount to resize (positive to grow, negative to shrink)."
  (let ((pos (my/get-window-position)))
    (cond
     ((and horizontal (eq pos 'left)) (enlarge-window (- delta) t))
     ((and horizontal (eq pos 'right)) (enlarge-window delta t))
     ((and (not horizontal) (eq pos 'top)) (enlarge-window delta nil))
     ((and (not horizontal) (eq pos 'bottom)) (enlarge-window (- delta) nil))
     (t (enlarge-window delta horizontal)))))
;;
(defun my/repeat-window-size ()
  "Set up a sparse keymap for repeating window actions with adaptive resizing."
  (interactive)
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") (lambda () (interactive)
                                (window-swap-states)))
    (define-key map (kbd "h") (lambda () (interactive)
                                (my/adaptive-resize t 2)))
    (define-key map (kbd "l") (lambda () (interactive)
                                (my/adaptive-resize t -2)))
    (define-key map (kbd "j") (lambda () (interactive)
                                (my/adaptive-resize nil 1)))
    (define-key map (kbd "k") (lambda () (interactive)
                                (my/adaptive-resize nil -1)))
    (set-transient-map map t)))
;;
(defun my/dired-du ()
  "Run 'du -hc' and count the total number of files in the directory under
  the cursor in Dired, then display the output in a buffer named *dired-du*."
  (interactive)
  (let ((current-dir (dired-get-file-for-visit)))
    (if (file-directory-p current-dir)
        (let ((output-buffer-name "*dired-du*"))
          (with-current-buffer (get-buffer-create output-buffer-name)
            (erase-buffer)) ; Clear the buffer before running the command
          (async-shell-command
           (format "du -hc --max-depth=1 %s && echo && echo 'File counts per subdirectory:' && find %s -maxdepth 2 -type d -exec sh -c 'echo -n \"{}: \"; find \"{}\" -type f | wc -l' \\;"
                   (shell-quote-argument current-dir)
                   (shell-quote-argument current-dir))
           output-buffer-name))
      (message "The current point is not a directory."))))
;;
(defun darken-color (color percent)
  "Return a darker shade of COLOR by reducing its brightness by PERCENT."
  (let* ((rgb (color-values color))
         (factor (/ (- 100 percent) 100.0))
         (darker-rgb (mapcar (lambda (x) (max 0 (round (* x factor)))) rgb)))
    (apply 'format "#%02x%02x%02x" (mapcar (lambda (x) (/ x 256)) darker-rgb))))
;;
(defun set-hl-line-darker-background ()
  "Set the hl-line background to a slightly darker shade of the default background,
                                            preserving the original foreground colors of the current line."
  (interactive)
  (require 'hl-line)
  (unless global-hl-line-mode
    (global-hl-line-mode 1))
  (when (facep 'hl-line)
    (let* ((bg (face-background 'default))
           (darker-bg (darken-color bg 15)))
      (custom-set-faces
       `(hl-line ((t (:background ,darker-bg))))))))
;;
(defun my/load-theme ()
  "Prompt to select a theme from available themes and load the selected theme."
  (interactive)
  (let ((theme (completing-read "Choose theme: " (mapcar 'symbol-name (custom-available-themes)))))
    (dolist (item custom-enabled-themes)
      (disable-theme item))
    (load-theme (intern theme) t)))
;;
(defun my/switch-to-thing ()
  "Switch to a buffer, open a recent file, jump to a bookmark,
                                        or change the theme from a unified interface."
  (interactive)
  (let* ((buffers (mapcar #'buffer-name (buffer-list)))
         (recent-files recentf-list)
         (bookmarks (bookmark-all-names))
         (all-options (append buffers recent-files bookmarks))
         (selection (completing-read "Switch to: " all-options)))
    (pcase selection
      ((pred (lambda (sel) (member sel buffers))) (switch-to-buffer selection))
      ((pred (lambda (sel) (member sel bookmarks))) (bookmark-jump selection))
      (_ (find-file selection)))))
;;
(defvar highlight-rules
  '((th . (("TODO" . "#999")))
    (td . (("\\&gt" . "#bbb")
           ("-\\&gt" . "#ccc")
           ("- " . "#ddd")
           ("- - - - " . "#eee")
           ("- - - - - - - - " . "#fff")
           ("TODO" . "#fdd")
           ("DOING" . "#ddf")
           ("DONE" . "#dfd"))))
  "Alist of elements ('th or 'td) and associated keywords/colors for row highlighting.")
;;
(defun apply-row-style (row-start row-attributes color)
  "Apply a background COLOR to the row starting at ROW-START with ROW-ATTRIBUTES."
  (goto-char row-start)
  (kill-line)
  (insert (format "<tr%s style=\"background: %s\">\n" row-attributes color)))
;;
(defun highlight-row-by-rules (row-start row-end row-attributes element)
  "Highlight a row based on ELEMENT ('th or 'td) keyword rules within ROW-START to ROW-END."
  (let ((rules (cdr (assoc element highlight-rules))))
    (dolist (rule rules)
      (let ((keyword (car rule))
            (color (cdr rule)))
        (when (save-excursion
                (and (re-search-forward (format "<%s.*>%s.*</%s>" element keyword element) row-end t)
                     (goto-char row-start)))
          (apply-row-style row-start row-attributes color))))))
;;
(defun my/html-org-table-highlight ()
  "Open the exported HTML file, find tables with specific classes,
                                                        and add background styles to rows containing keywords in <td> or <th> elements."
  (interactive)
  (let* ((org-file (buffer-file-name))
         (html-file (concat (file-name-sans-extension org-file) ".html")))
    (with-temp-buffer
      (insert-file-contents html-file)
      (goto-char (point-min))
      (while (re-search-forward "<table.*>" nil t)
        (let ((table-start (point))
              (table-end (save-excursion
                           (when (re-search-forward "</table>" nil t)
                             (point)))))
          (when table-end
            (save-restriction
              (narrow-to-region table-start table-end)
              (goto-char (point-min))
              (while (re-search-forward "<tr\\(.*\\)>" nil t)
                (let ((row-start (match-beginning 0))
                      (row-attributes (match-string 1))
                      (row-end (save-excursion (search-forward "</tr>"))))
                  (highlight-row-by-rules row-start row-end row-attributes 'th)
                  (highlight-row-by-rules row-start row-end row-attributes 'td)))))))
      (write-region (point-min) (point-max) html-file))))
;;
(defun my/format-to-table (&optional match properties-to-display)
  "Format Org headings into a structured alist, optionally filtered by MATCH
  and displaying only specified PROPERTIES-TO-DISPLAY (e.g., '(\"ID\" \"PRIORITY\"))."
  (interactive)
  (let ((rows '())
        (header '("TODO" "Tags" "Title" "Comments")) ;; Start without "Properties"
        (max-level 0))
    (save-excursion
      (goto-char (point-min))
      (when match (re-search-forward (format "\\*%s\\*$" (regexp-quote match)) nil t))
      ;; Add property names to the header dynamically
      (setq header (append header properties-to-display))
      (org-map-entries
       (lambda ()
         (let* ((entry (org-element-at-point))
                (heading (org-get-heading t t t t))
                (level (org-outline-level))
                (tags (remove "noexport" (org-get-tags)))
                (todo (org-get-todo-state))
                (vis-indent "- ")
                (contents "")
                (all-properties (org-entry-properties))
                (filtered-properties
                 (mapcar (lambda (prop)
                           (if (cdr (assoc prop all-properties))
                               (cdr (assoc prop all-properties))
                             ""))
                         properties-to-display)))
           (prin1 properties-to-display)
           (prin1 all-properties)
           (prin1 filtered-properties)
           (org-end-of-meta-data nil)
           (skip-chars-forward " \n\t")
           (when (eq (org-element-type (org-element-at-point)) 'paragraph)
             (let ((start (point)))
               (org-next-visible-heading 1)
               (setq contents (buffer-substring-no-properties start (point)))
               (dolist (pattern '("^#\\+begin.*" "^#\\+end.*" "\n+"))
                 (setq contents (replace-regexp-in-string pattern
                                                          (if (string= pattern "\n+") " " "")
                                                          (string-trim contents))))))
           (setq max-level (max max-level level))
           (push (append
                  (list
                   (or todo "")
                   (string-join tags ":")
                   (cond ((= level 1)
                          (concat "> " heading))
                         ((= level 2)
                          (concat "> " heading))
                         ((= level 3)
                          (concat "*> " heading "*"))
                         ((= level 4)
                          (concat "*" heading "*"))
                         (t
                          (concat "/"
                                  (mapconcat (lambda (_) vis-indent)
                                             (make-list (* (- level 4) 1) "") "") heading "/")))
                   (or contents ""))
                  filtered-properties)
                 rows)))
       nil (when match 'tree)))
    (setq rows (reverse rows))
    (push 'hline rows)
    (cons header rows)))
;;
(defun my/html-promote-headers ()
  "Promote all headers in the HTML file by one level (e.g., h2 -> h1, h3 -> h2, etc.), accounting for attributes."
  (interactive)
  (let* ((org-file (buffer-file-name))
         (html-file (concat (file-name-sans-extension org-file) ".html")))
    (with-temp-buffer
      (insert-file-contents html-file)
      (goto-char (point-min))
      (let ((header-levels '("h1" "h2" "h3" "h4" "h5" "h6")))
        (dolist (level header-levels)
          (let* ((current-level (string-to-number (substring level 1)))
                 (new-level (max 1 (1- current-level)))  ;; Promote but don't go below h1
                 (open-tag-regex (format "<%s\\([^>]*\\)>" level))  ;; Regex for opening tag with attributes
                 (close-tag-regex (format "</%s>" level))  ;; Regex for closing tag
                 (new-open-tag (format "<h%d\\1>" new-level))  ;; Replacement for opening tag, preserving attributes
                 (new-close-tag (format "</h%d>" new-level)))  ;; Replacement for closing tag
            ;; Replace opening tags
            (goto-char (point-min))
            (while (re-search-forward open-tag-regex nil t)
              (replace-match new-open-tag))
            ;; Replace closing tags
            (goto-char (point-min))
            (while (re-search-forward close-tag-regex nil t)
              (replace-match new-close-tag)))))
      (write-region (point-min) (point-max) html-file))))
;;
(defun toggle-centered-buffer ()
  "Toggle center alignment of the buffer by adjusting window margins based on the fill-column."
  (interactive)
  (let* ((current-margins (window-margins))
         (margin (if (or (equal current-margins '(0 . 0))
                         (null (car (window-margins))))
                     (/ (- (window-total-width) fill-column) 2)
                   0)))
    (visual-line-mode 1)
    (set-window-margins nil margin margin)))
;;
(defun my/copy-buffer-to-kill-ring ()
  "Copy the entire buffer to the kill ring without changing the point."
  (interactive)
  (kill-ring-save (point-min) (point-max))
  (message (concat (buffer-file-name) " Copied")))
;;
(defun my/sync-tab-bar-to-theme ()
  "Synchronize tab-bar faces with the current theme, and set
  mode-line background color interactively using `read-color`."
  (interactive)
  ;; Use `read-color` to get the mode-line background color from the user
  (let ((selected-color (read-color)))
    (set-hl-line-darker-background)
    (set-face-attribute 'mode-line nil :height 120 :underline nil :overline nil :box nil
                        :background selected-color :foreground "#000000")
    (set-face-attribute 'mode-line-inactive nil :height 120 :underline nil :overline nil
                        :background "#000000" :foreground "#aaaaaa")
    (let ((default-bg (face-background 'default))
          (default-fg (face-foreground 'default))
          (default-hl (face-background 'highlight))
          (inactive-fg (face-foreground 'mode-line-inactive)))
      (custom-set-faces
       `(vertical-border ((t (:foreground ,(darken-color default-fg 60)))))
       `(window-divider ((t (:foreground ,(darken-color default-fg 60)))))
       `(fringe ((t (:foreground ,default-bg :background ,default-bg))))
       `(tab-bar ((t (:inherit default :background ,default-bg :foreground ,default-fg))))
       `(tab-bar-tab ((t (:inherit 'highlight :background ,selected-color :foreground "#000000"))))
       `(tab-bar-tab-inactive ((t (:inherit default :background ,default-bg :foreground ,inactive-fg
                                            :box (:line-width 2 :color ,default-bg :style released-button)))))))))
;;
(defun my/recentf-open (file)
  "Prompt for FILE in `recentf-list' and visit it.
Enable `recentf-mode' if it isn't already."
  (interactive
   (list
    (progn (unless recentf-mode (recentf-mode 1))
           (completing-read (format-prompt "Open recent file" nil)
                            recentf-list nil t))))
  (when file
    (funcall recentf-menu-action file)))

;;
;; -> window-positioning-core
;;
(add-to-list 'display-buffer-alist
             '("\\*.*shell"
               (display-buffer-reuse-window display-buffer-in-direction)
               (direction . bottommost)
               (dedicated . t)
               (window-height . 0.2)
               (inhibit-same-window . t)))
(add-to-list 'display-buffer-alist
             '("\\*Messages" display-buffer-same-window))

;;
;; -> org-core
;;
(setq org-startup-indented t)
(setq org-use-speed-commands t)
(setq org-hide-leading-stars t)
(setq org-todo-keywords
      '((sequence "TODO" "DOING" "|" "DONE" "CANCELLED"))
      org-todo-keyword-faces
      '(("TODO" . "#ee5566")
        ("DOING" . "#5577aa")
        ("DONE" . "#77aa66")
        ("CANCELLED" . "#426b3e")))

;;
;; -> scroll-core
;;
(setq scroll-margin 10)
(setq scroll-conservatively 10)
(setq scroll-preserve-screen-position t)

;;
;; -> dired-core
;;
(setq dired-dwim-target t)
(setq dired-listing-switches "-alGgh")
(setq dired-auto-revert-buffer t)
(setq dired-confirm-shell-command nil)
(setq dired-no-confirm t)
(setq dired-deletion-confirmer '(lambda (x) t))
(setq dired-recursive-deletes 'always)
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c d") 'my/dired-duplicate-file)
  (define-key dired-mode-map (kbd "C-c u") 'my/dired-du)
  (define-key dired-mode-map (kbd "_") #'dired-create-empty-file))

;;
;; -> visuals-core
;;
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-screen t)
(setq use-dialog-box nil)
(setq window-divider-default-bottom-width 2)
(setq window-divider-default-right-width 2)
(setq window-divider-default-places t)
(window-divider-mode -1)
(defvar my/internal-border-width 0 "Default internal border width for toggling.")
(modify-all-frames-parameters `((internal-border-width . ,my/internal-border-width)))
(set-fringe-mode '(20 . 20))
;;
(defun my/rainbow-mode ()
  "Overlay colors represented as hex values in the current buffer."
  (interactive)
  (remove-overlays (point-min) (point-max))
  (let ((hex-color-regex "#[0-9a-fA-F]\\{3,6\\}"))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward hex-color-regex nil t)
        (let* ((color (match-string 0))
               (overlay (make-overlay (match-beginning 0) (match-end 0))))
          (if (string-greaterp color "#888888")
              (overlay-put overlay 'face `(:background ,color :foreground "black"))
            (overlay-put overlay 'face `(:background ,color :foreground "white"))))))))
;;
(defun my/rainbow-mode-clear ()
  "Remove all hex color overlays in the current buffer."
  (interactive)
  (remove-overlays (point-min) (point-max)))
;;
(add-hook 'prog-mode-hook #'my/rainbow-mode)
(add-hook 'org-mode-hook #'my/rainbow-mode)
(add-hook 'conf-space-mode-hook #'my/rainbow-mode)

;;
;; -> imenu-core
;;
(defun my-imenu-create-index ()
  "Create an index using definitions starting with ';; ->'."
  (let ((index-alist '())
        (regex "^;;[[:space:]]->\\(.+\\)$"))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regex nil t)
        (let ((name (s-trim (match-string 1)))
              (pos (match-beginning 0)))
          (push (cons name (set-marker (make-marker) pos)) index-alist))))
    (setq imenu--index-alist (sort
                              index-alist
                              (lambda (a b)
                                (string< (car a) (car b)))))))
;;
;; (setq imenu-create-index-function #'my-imenu-create-index)
;;
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq truncate-lines t)
            (setq imenu-sort-function 'imenu--sort-by-name)
            (setq imenu-generic-expression
                  '((nil "^;;[[:space:]]+-> \\(.*\\)$" 1)))
            (imenu-add-menubar-index)))
;;
(add-hook 'conf-space-mode-hook
          (lambda ()
            (setq imenu-sort-function 'imenu--sort-by-name)
            (setq imenu-generic-expression
                  '((nil "^#[[:space:]]+-> \\(.*\\)$" 1)))
            (imenu-add-menubar-index)))

;;
;; -> recentf-core
;;
(recentf-mode 1)
(setq recentf-max-menu-items 200)
(setq recentf-max-saved-items 200)

;;
;; -> modeline-core
;;
(setq my/mode-line-format
      (list
       '(:eval (if (and (buffer-file-name) (buffer-modified-p))
                   (propertize " * " 'face
                               '(:background "#ff0000" :foreground "#ffffff" :inherit bold)) ""))
       '(:eval
         (propertize (format "%s" (abbreviate-file-name default-directory)) 'face '(:inherit bold)))
       '(:eval
         (if (not (equal major-mode 'dired-mode))
             (propertize (format "%s " (buffer-name)))
           " "))
       'mode-line-position
       'mode-line-modes
       'mode-line-misc-info
       '(:eval (format " | Point: %d" (point)))))
;;
(setq-default mode-line-format my/mode-line-format)
;;
(defun my/toggle-mode-line ()
  "Toggle the visibility of the mode-line by checking its current state."
  (interactive)
  (if (eq mode-line-format nil)
      (progn
        (setq-default mode-line-format my/mode-line-format)
        (setq frame-title-format "%f"))
    (progn
      (setq-default mode-line-format nil)
      (setq frame-title-format mode-line-format)))
  (force-mode-line-update t))

;;
;; -> find-core
;;
(defun my/find-file ()
  "Find file from current directory in many different ways."
  (interactive)
  (let* ((find-options '(("find -type f -printf \"$PWD/%p\\0\"" . :string)
                         ("fd --absolute-path --type f -0" . :string)
                         ("rg --follow --files --null" . :string)
                         ("find-name-dired" . :command)))
         (selection (completing-read "Select : " find-options))
         (file-list)
         (file))
    (pcase (alist-get selection find-options nil nil #'string=)
      (:command
       (call-interactively (intern selection)))
      (:string
       (setq file-list (split-string (shell-command-to-string selection) "\0" t))
       (setq file (completing-read
                   (format "Find file in %s: "
                           (abbreviate-file-name default-directory))
                   file-list))))
    (when file (find-file (expand-file-name file)))))

;;
;; -> grep-core
;;
(eval-after-load 'grep
  '(progn
     (dolist (dir '("nas" ".cache" "cache" "elpa" "chromium" ".local/share" "syncthing" ".mozilla" ".local/lib" "Games"))
       (push dir grep-find-ignored-directories))
     (dolist (file '(".cache" "*cache*" "*.iso" "*.xmp" "*.jpg" "*.mp4"))
       (push file grep-find-ignored-files))
     ))

;;
;; -> spelling-core
;;
(setq ispell-local-dictionary "en_GB")
(setq ispell-program-name "hunspell")
(global-set-key (kbd "C-c s l") #'(lambda()(interactive)
                                    (flyspell-buffer)
                                    (flyspell-mode)))
(global-set-key (kbd "C-c s s") #'ispell-word)
(global-set-key (kbd "C-c s j") #'ispell-word)

;;
;; -> gdb-core
;;
(setq gdb-display-io-nopopup 1)
(setq gdb-many-windows t)
(global-set-key (kbd "<f9>") 'gud-break)
(global-set-key (kbd "<f10>") 'gud-next)
(global-set-key (kbd "<f11>") 'gud-step)

;;
;; -> compilation-core
;;
(setq compilation-always-kill t)
(setq compilation-context-lines 3)
(setq compilation-scroll-output t)
;; ignore warnings
(setq compilation-skip-threshold 2)
(global-set-key (kbd "<f5>") 'my/project-compile)

;;
;; -> diff-core
;;
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-highlight-all-diffs t)
(setq ediff-split-window-function 'split-window-horizontally)
(add-hook 'ediff-prepare-buffer-hook #'outline-show-all)
(add-hook 'ediff-prepare-buffer-hook (lambda () (visual-line-mode -1)))

;;
;; -> project-core
;;
(defun my/project-create-compilation-search-path ()
  "Populate the 'compilation-search-path' variable.
With directories under project root using find."
  (interactive)
  (let ((find-command
         (concat "find " (project-root (project-current t))
                 " \\( -path \\*/.local -o -path \\*/.config -o
 -path \\*/.svn -o -path \\*/.git -o -path \\*/nas \\) -prune -o
 -type d -print")))
    (setq compilation-search-path
          (split-string
           (shell-command-to-string find-command)
           "\n" t))))
;;
(setq project-vc-extra-root-markers '(".project"))

;;
;; -> indentation-core
;;
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;;
;; -> etags-core
;;
;;
(defun my/etags-load ()
  "Load TAGS file from the first it can find up the directory stack."
  (interactive)
  (let ((my-tags-file (locate-dominating-file default-directory "TAGS")))
    (when my-tags-file
      (message "Loading tags file: %s" my-tags-file)
      (visit-tags-table my-tags-file))))
;;
(when (executable-find "my-generate-etags.sh")
  (defun my/etags-update ()
    "Call external bash script to generate new etags for all languages it can find."
    (interactive)
    (async-shell-command "my-generate-etags.sh" "*etags*")))
;;
(defun predicate-exclusion-p (dir)
  "exclusion of directories"
  (not
   (or
    (string-match "/home/jdyer/examples/CPPrograms/nil" dir)
    )))
;;
(defun my/generate-etags ()
  "Generate TAGS file for various source files in `default-directory` and its subdirectories."
  (interactive)
  (message "Getting file list...")
  (let ((all-files
         (append
          (directory-files-recursively default-directory "\\(?:\\.cpp$\\|\\.c$\\|\\.h$\\)" nil 'predicate-exclusion-p)
          (directory-files-recursively default-directory "\\(?:\\.cs$\\|\\.cs$\\)" nil 'predicate-exclusion-p)
          (directory-files-recursively default-directory "\\(?:\\.ads$\\|\\.adb$\\)" nil 'predicate-exclusion-p)))
        (tags-file-path (expand-file-name (concat default-directory "TAGS"))))
    (unless (file-directory-p default-directory)
      (error "Default directory does not exist: %s" default-directory))
    ;; Generate TAGS file
    (dolist (file all-files)
      (message file)
      (shell-command (format "etags --append \%s -o %s" file tags-file-path)))))
;; (global-set-key (kbd "C-x p l") 'my/etags-load)
;; (global-set-key (kbd "C-x p u") 'my/etags-update)

;;
;; -> shell-core
;;
(defun my/shell-create (name)
  "Create a custom-named eshell buffer with NAME."
  (interactive "sName: ")
  (eshell 'new)
  (let ((new-buffer-name (concat "*eshell-" name "*")))
    (rename-buffer new-buffer-name t)))

;;
;; -> tab-bar-core
;;
(setq tab-bar-close-button-show nil)
(setq tab-bar-new-button-show nil)
(setq tab-bar-new-tab-to 'rightmost)
(setq tab-bar-close-button-show nil)

;;
;; -> windows-specific-core
;;
(when (eq system-type 'windows-nt)
  (setq home-dir "c:/users/jimbo")
  (let ((xPaths
         `(,(expand-file-name "~/bin")
           ,(expand-file-name "~/bin/PortableGit/bin")
           ,(expand-file-name "~/bin/PortableGit/usr/bin")
           ,(expand-file-name "~/bin/Apache-Subversion/bin/")
           ,(expand-file-name "~/bin/svn2git-2.4.0/bin")
           ,(expand-file-name "~/bin/clang/bin")
           ,(expand-file-name "~/bin/find")
           ,(expand-file-name "~/bin/omnisharp-win-x64")
           "c:/GnuWin32/bin"
           "c:/GNAT/2021/bin")))
    (setenv "PATH" (mapconcat 'identity xPaths ";"))
    (setq exec-path (append xPaths (list "." exec-directory))))
  ;;
  (custom-theme-set-faces
   'user
   '(variable-pitch ((t (:family "Consolas" :height 110 :weight normal))))
   '(fixed-pitch ((t ( :family "Consolas" :height 110)))))

  (setq font-general "Consolas 11")
  (set-frame-font font-general nil t)
  (add-to-list 'default-frame-alist `(font . ,font-general)))
;;
(setq tab-bar-show 1)

;;
;; -> linux-specific-core
;;
(when (eq system-type 'gnu/linux)
  (custom-theme-set-faces
   'user
   '(variable-pitch ((t (:family "DejaVu Sans" :height 120 :weight normal))))
   '(fixed-pitch ((t ( :family "Source Code Pro" :height 110)))))
  (setq font-general "Source Code Pro 12")
  (set-frame-font font-general nil t)
  (add-to-list 'default-frame-alist `(font . ,font-general)))

;;
;; -> LLM-core
;;
;;
(defun safe-add-to-load-path (dir)
  "Add DIR to `load-path` if it exists."
  (when (file-directory-p dir)
    (add-to-list 'load-path dir)))
;; Add directories to load-path only if they exist
(safe-add-to-load-path (expand-file-name "lisp/shell-maker" user-emacs-directory))
(safe-add-to-load-path (expand-file-name "lisp/chatgpt-shell" user-emacs-directory))
(safe-add-to-load-path (expand-file-name "lisp/gptel" user-emacs-directory))
;; Conditionally require and configure packages if their files exist
(when (locate-library "gptel")
  (require 'gptel)
  (require 'gptel-ollama)
  (require 'gptel-curl)
  (gptel-make-ollama "llama3_2"
    :host "localhost:11434"
    :stream t
    :models '(llama3_2:latest))
  (setq gptel-model 'qwen2.5-coder-7b-instruct-q5_k_m:latest
        gptel-backend (gptel-make-ollama "llama3_2"
                        :host "localhost:11434"
                        :stream t
                        :models '(llama3_2:latest))))
;;
(when (locate-library "shell-maker")
  (require 'shell-maker))
;;
(when (locate-library "chatgpt-shell")
  (require 'chatgpt-shell)
  (setq chatgpt-shell-models
        '(((:provider . "Ollama")
           (:label . "Ollama-llama")
           (:version . "llama3_2")
           (:short-version)
           (:token-width . 4)
           (:context-window . 8192)
           (:handler . chatgpt-shell-ollama--handle-ollama-command)
           (:filter . chatgpt-shell-ollama--extract-ollama-response)
           (:payload . chatgpt-shell-ollama-make-payload)
           (:url . chatgpt-shell-ollama--make-url))))
  (with-eval-after-load 'chatgpt-shell
    (defun chatgpt-shell-menu ()
      "Menu for ChatGPT Shell commands."
      (interactive)
      (let ((key (read-key
                  (propertize
                   "ChatGPT Shell Commands:\n
      e: Explain Code      d: Describe Code           l: Start Shell
      p: Proofread Region  r: Refactor Code           t: Save Session Transcript
      g: Write Git Commit  u: Generate Unit Test      o: Summarize Last Command Output
      s: Send Region       a: Send and Review Region  m: Swap Model\n
        q: Quit\n\nPress a key: " 'face 'minibuffer-prompt))))
        (pcase key
          (?e (call-interactively 'chatgpt-shell-explain-code))
          (?p (call-interactively 'chatgpt-shell-proofread-region))
          (?g (call-interactively 'chatgpt-shell-write-git-commit))
          (?s (call-interactively 'chatgpt-shell-send-region))
          (?d (call-interactively 'chatgpt-shell-describe-code))
          (?r (call-interactively 'chatgpt-shell-refactor-code))
          (?u (call-interactively 'chatgpt-shell-generate-unit-test))
          (?a (call-interactively 'chatgpt-shell-send-and-review-region))
          (?l (call-interactively 'chatgpt-shell))
          (?t (call-interactively 'chatgpt-shell-save-session-transcript))
          (?o (call-interactively 'chatgpt-shell-eshell-summarize-last-command-output))
          (?w (call-interactively 'chatgpt-shell-eshell-whats-wrong-with-last-command))
          (?i (call-interactively 'chatgpt-shell-describe-image))
          (?m (call-interactively 'chatgpt-shell-swap-model))
          (?q (message "Quit ChatGPT Shell menu."))
          (?\C-g (message "Quit ChatGPT Shell menu."))
          (_ (message "Invalid key: %c" key))))))
  (global-set-key (kbd "C-c g") 'chatgpt-shell-menu))

;;
;; -> programming-core
;;
;;
(defun my/eglot-dir-locals ()
  "Create .dir-locals.el file for eglot ada-mode using the selected DIRED path."
  (interactive)
  (add-dir-local-variable
   'ada-mode
   'eglot-workspace-configuration
   `((ada . (:projectFile ,(dired-get-filename))))))
;;
(setq vc-handled-backends '(SVN Git))

;;
;; -> ada-core
;;
(defvar ada-light-mode-keywords
  ;; https://www.adaic.org/resources/add_content/standards/05rm/html/RM-2-9.html
  '("abort" "else" "new" "return" "abs" "elsif" "not" "reverse" "abstract" "end"
    "null" "accept" "entry" "select" "access" "exception" "of" "separate"
    "aliased" "exit" "or" "subtype" "all" "others" "synchronized" "and" "for"
    "out" "array" "function" "overriding" "tagged" "at" "task" "generic"
    "package" "terminate" "begin" "goto" "pragma" "then" "body" "private" "type"
    "if" "procedure" "case" "in" "protected" "until" "constant" "interface"
    "use" "is" "raise" "declare" "range" "when" "delay" "limited" "record"
    "while" "delta" "loop" "rem" "with" "digits" "renames" "do" "mod" "requeue"
    "xor")
  "Keywords of the Ada 2012 language.")
;;
(defvar ada-light-mode--font-lock-rules
  (list (regexp-opt ada-light-mode-keywords 'symbols))
  "Rules for search-based fontification in `ada-light-mode'.
The format is appropriate for `font-lock-keywords'.")
;;
(defvar ada-light-mode-syntax-table     ; used automatically by define-derived-mode
  (let ((table (make-syntax-table)))
    ;; Comments start with "--".
    (modify-syntax-entry ?- ". 12" table)
    ;; Newlines end comments.
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\r ">" table)
    ;; Backslash is a regular symbol, not an escape character.
    (modify-syntax-entry ?\\ "_" table)
    table)
  "Syntax table used in `ada-light-mode'.")
;;
(defvar ada-light-mode-other-file-alist
  '(("\\.ads\\'" (".adb"))
    ("\\.adb\\'" (".ads")))
  "Value for `ff-other-file-alist' in `ada-light-mode'.")
;;
(defun ada-light-mode--syntax-propertize (start end)
  "Apply syntax properties to the region from START to END."
  ;; Ada delimits character literals with single quotes, but also uses the
  ;; single quote for other purposes. Since character literals are always
  ;; exactly one character long (i.e., there are no escape sequences), we can
  ;; easily find them with a regular expression and change the syntax class of
  ;; the enclosing single quotes to "generic string". This also nicely handles
  ;; the case of '"': generic string delimiters only match other generic string
  ;; delimiters, but not ordinary quote characters (i.e., the double quote).
  (goto-char start)
  (while-let ((pos (re-search-forward "'.'" end t)))
    (put-text-property (- pos 3) (- pos 2) 'syntax-table '(15))
    (put-text-property (- pos 1) pos 'syntax-table '(15))))
;;
(defvar ada-light-mode--imenu-rules
  `(("Functions"
     ,(rx bol
          (* space)
          (? (? "not" (* space)) "overriding" (* space))
          "function"
          (+ space)
          (group (+ (or word (syntax symbol)))))
     1)
    ("Procedures"
     ,(rx bol
          (* space)
          (? (? "not" (* space)) "overriding" (* space))
          "procedure"
          (+ space)
          (group (+ (or word (syntax symbol)))))
     1)
    ("Types"
     ,(rx bol
          (* space)
          (? "sub")
          "type"
          (+ space)
          (group (+ (or word (syntax symbol)))))
     1)
    ("Packages"
     ,(rx bol
          (* space)
          "package"
          (+ space)
          (group (+ (or word (syntax symbol))))
          (+ space)
          "is")
     1))
  "Imenu configuration for `ada-light-mode'.
The format is appropriate for `imenu-generic-expression'.")
;;
(defun ada-light-mode--indent-line ()
  "Indent a single line of Ada code."
  ;; This is a really dumb implementation which just indents to the most recent
  ;; non-empty line's indentation. It's better than the default though because
  ;; it stops there, so that users who want completion on TAB can get it after
  ;; indenting. (The default behavior is to insert TAB characters indefinitely.)
  (let ((indent (save-excursion
                  (beginning-of-line)
                  (if (re-search-backward "^[^\n]" nil t) ; non-empty line
                      (current-indentation)
                    0))))
    (if (<= (current-column) (current-indentation))
        (indent-line-to indent)
      (when (< (current-indentation) indent)
        (save-excursion (indent-line-to indent))))))
;;
;;;###autoload
(define-derived-mode ada-light-base-mode prog-mode "AdaLBase"
  "Base mode for `ada-light-mode' and `gpr-light-mode'."
  ;; Set up commenting; Ada uses "--" followed by two spaces.
  (setq-local comment-use-syntax t
              comment-start "--"
              comment-padding 2)
  ;; Set up fontification.
  (setq-local font-lock-defaults '(ada-light-mode--font-lock-rules nil t)
              syntax-propertize-function #'ada-light-mode--syntax-propertize)
  ;; And finally, configure indentation. Since our indentation function isn't
  ;; particularly good, don't force it upon the user.
  (setq-local standard-indent 3
              tab-width 3               ; used by eglot for range formatting
              indent-line-function 'ada-light-mode--indent-line
              electric-indent-inhibit t))
;;
;;;###autoload
(define-derived-mode ada-light-mode ada-light-base-mode "AdaL"
  "Major mode for the Ada programming language.
It doesn't define any keybindings. In comparison with `ada-mode',
`ada-light-mode' is faster but less accurate."
  (setq-local ff-other-file-alist ada-light-mode-other-file-alist
              imenu-generic-expression ada-light-mode--imenu-rules))
;;
;;;###autoload
(define-derived-mode gpr-light-mode ada-light-base-mode "GPRL"
  "Major mode for GPR project files."
  :syntax-table ada-light-mode-syntax-table)
;;
;; Register the mode for Ada code following GNAT naming conventions.
;;;###autoload
(progn (add-to-list 'auto-mode-alist '("\\.ad[abcs]\\'" . ada-light-mode))
       (add-to-list 'auto-mode-alist '("\\.gpr\\'" . gpr-light-mode)))
;;
;; Configure eglot if available.
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '((ada-light-mode :language-id "ada")
                                        "ada_language_server"))
  ;; The Ada Language Server doesn't support formatting .gpr files, but it
  ;; provides completion and detects syntax errors.
  (add-to-list 'eglot-server-programs '((gpr-light-mode :language-id "ada")
                                        "ada_language_server" "--language-gpr"))
  (defun ada-light-other-file ()
    "Jump from spec to body or vice versa using the Ada Language Server."
    (interactive)
    (if-let ((server (eglot-current-server)))
        (eglot-execute-command server
                               "als-other-file"
                               (vector (eglot--TextDocumentIdentifier)))
      (message "%s" "Not connected to the Ada Language Server")))
  ;; The "als-other-file" command used by `ada-light-other-file' requires
  ;; support for the "window/showDocument" server request in eglot; add it if
  ;; necessary.
  (unless (cl-find-method 'eglot-handle-request nil '(t (eql window/showDocument)))
    (cl-defmethod eglot-handle-request
      (_server (_method (eql window/showDocument)) &key uri &allow-other-keys)
      (find-file (eglot--uri-to-path uri))
      (list :success t)))
  ;;
  (defun ada-light-mode--current-line-empty-p ()
    (save-excursion
      (beginning-of-line)
      (looking-at-p (rx (* space) eol))))
  ;;
  (defun ada-light-mode--indent-line-eglot ()
    "Indent the current line using the Ada Language Server."
    (interactive)
    (if (ada-light-mode--current-line-empty-p)
        ;; Let's not "indent" empty lines with the language server, it would
        ;; just delete them. Instead, take a guess at the required indentation
        ;; based on the most recent non-empty line.
        (indent-relative t t)
      (condition-case err
          (eglot-format (line-beginning-position) (line-end-position))
        ;; When `eglot-format' fails due to a server issue it signals the
        ;; underlying `jsonrpc-error'. In this case, let's return normally to
        ;; give completion a chance.
        (jsonrpc-error
         (when-let ((msg (alist-get 'jsonrpc-error-message (cdr err))))
           (message "Language server error: %s" msg))
         nil))))
  ;;
  (defun ada-light-mode--eglot-setup ()
    "Set up `eglot' integration for `ada-light-mode'."
    (when (eq major-mode 'ada-light-mode)
      (if (eglot-managed-p)
          (setq-local indent-line-function 'ada-light-mode--indent-line-eglot
                      electric-indent-inhibit nil)
        (setq-local indent-line-function 'ada-light-mode--indent-line
                    electric-indent-inhibit t))))
  ;;
  (add-hook 'eglot-managed-mode-hook #'ada-light-mode--eglot-setup))
;;
(provide 'ada-light-mode)

;;
;; -> development-core
;;
(global-set-key (kbd "C-c t") 'toggle-centered-buffer)
;;
(defun my/md-to-org-convert-buffer ()
  "Convert the current buffer from Markdown to Org-mode format"
  (interactive)
  (save-excursion
    ;; Lists: Translate `-`, `*`, or `+` lists to Org-mode lists
    (goto-char (point-min))
    (while (re-search-forward "^\\([ \t]*\\)[*-+] \\(.*\\)$" nil t)
      (replace-match (concat (match-string 1) "- \\2")))
    ;; Bold: `**bold**` -> `*bold*` only if directly adjacent
    (goto-char (point-min))
    (while (re-search-forward "\\*\\*\\([^ ]\\(.*?\\)[^ ]\\)\\*\\*" nil t)
      (replace-match "*\\1*"))
    ;; Italics: `_italic_` -> `/italic/`
    (goto-char (point-min))
    (while (re-search-forward "\\b_\\([^ ]\\(.*?\\)[^ ]\\)_\\b" nil t)
      (replace-match "/\\1/"))
    ;; Links: `[text](url)` -> `[[url][text]]`
    (goto-char (point-min))
    (while (re-search-forward "\\[\\(.*?\\)\\](\\(.*?\\))" nil t)
      (replace-match "[[\\2][\\1]]"))
    ;; Code blocks: Markdown ```lang ... ``` to Org #+begin_src ... #+end_src
    (goto-char (point-min))
    (while (re-search-forward "```\\(.*?\\)\\(?:\n\\|\\s-\\)\\(\\(?:.\\|\n\\)*?\\)```" nil t)
      (replace-match "#+begin_src \\1\n\\2#+end_src"))
    ;; Inline code: `code` -> =code=
    (goto-char (point-min))
    (while (re-search-forward "`\\(.*?\\)`" nil t)
      (replace-match "=\\1="))
    ;; Horizontal rules: `---` or `***` -> `-----`
    (goto-char (point-min))
    (while (re-search-forward "^\\(-{3,}\\|\\*{3,}\\)$" nil t)
      (replace-match "-----"))
    ;; Images: `![alt text](url)` -> `[[url]]`
    (goto-char (point-min))
    (while (re-search-forward "!\\[.*?\\](\\(.*?\\))" nil t)
      (replace-match "[[\\1]]"))
    (goto-char (point-min))
    ;; Headers: Adjust '#'
    (while (re-search-forward "^\\(#+\\) " nil t)
      (replace-match (make-string (length (match-string 1)) ?*) nil nil nil 1))))
;;
(defun my/md-to-org-convert-file (input-file output-file)
  "Convert a Markdown file INPUT-FILE to an Org-mode file OUTPUT-FILE."
  (with-temp-buffer
    (insert-file-contents input-file)
    (md-to-org-convert-buffer)
    (write-file output-file)))
;;
(defun my/convert-markdown-clipboard-to-org ()
  "Convert Markdown content from clipboard to Org format and insert it at point."
  (interactive)
  (let ((markdown-content (current-kill 0))
        (original-buffer (current-buffer)))
    (with-temp-buffer
      (insert markdown-content)
      (my/md-to-org-convert-buffer)
      (let ((org-content (buffer-string)))
        (with-current-buffer original-buffer
          (insert org-content))))))
;;
(defun my/org-promote-all-headings (&optional arg)
  "Promote all headings in the current Org buffer along with their subheadings."
  (interactive "p")
  (org-map-entries
   (lambda () 
     (dotimes (_ arg) (org-promote)))))
;;
(global-set-key (kbd "M-s i") #'my/convert-markdown-clipboard-to-org)
(global-set-key (kbd "M-s u") #'my/org-promote-all-headings)
;;
(defun my-icomplete-copy-candidate ()
  "Copy the current Icomplete candidate to the kill ring."
  (interactive)
  (let ((candidate (car completion-all-sorted-completions)))
    (when candidate
      (kill-new (substring-no-properties candidate))
      (abort-recursive-edit))))
;;
(define-key minibuffer-local-completion-map (kbd "C-c ,") 'my-icomplete-copy-candidate)
;;
(defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.
    The generic `keyboard-quit' does not do the expected thing when
    the minibuffer is open.  Whereas we want it to close the
    minibuffer, even without explicitly focusing it.
    The DWIM behaviour of this command is as follows:
    - When the region is active, disable it.
    - When a minibuffer is open, but not focused, close the minibuffer.
    - When the Completions buffer is selected, close it.
    - In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))
;;
(define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim)
;;
(defun my/grep (search-term directory glob)
  "Run ripgrep (rg) with SEARCH-TERM in DIRECTORY and GLOB if available,
otherwise fall back to Emacs's rgrep command. Highlights SEARCH-TERM in results."
  (interactive
   (list
    (read-string "Search for: ")
    (read-directory-name "Directory: ")
    (read-string "File pattern (glob, default: *): " nil nil "*")))
  (let ((directory (expand-file-name directory))) ;; Expand directory to absolute path
    (if (executable-find "rg")
        ;; Use ripgrep if available
        (let* ((buffer-name "*my-rg-results*")
               (home-dir (expand-file-name "~"))
               (rg-command (format "rg --color=never --column --line-number --no-heading --smart-case -e %s --glob %s %s"
                                   (shell-quote-argument search-term)
                                   (shell-quote-argument glob)
                                   directory))
               (raw-output (shell-command-to-string rg-command))
               (formatted-output
                (if (not (string-empty-p raw-output))
                    ;; Replace absolute path with relative path or "./"
                    (replace-regexp-in-string (concat "\\(^" (regexp-quote directory) "\\)") "./" raw-output)
                  nil)))
          ;; Kill existing buffer if it exists
          (when (get-buffer buffer-name)
            (kill-buffer buffer-name))
          ;; Create the results buffer
          (with-current-buffer (get-buffer-create buffer-name)
            (read-only-mode -1)
            (erase-buffer)
            (if (not formatted-output)
                (progn
                  (message "Ripgrep finished with errors or no results.")
                  (insert "No results found."))
              (insert formatted-output)
              ;; Highlight the search term
              (let ((case-fold-search t)) ;; Make the highlighting case insensitive
                (goto-char (point-min))
                (while (search-forward search-term nil t)
                  (let ((start (match-beginning 0))
                        (end (match-end 0)))
                    ;; Add an overlay to highlight the match
                    (let ((overlay (make-overlay start end)))
                      (overlay-put overlay 'face '(:background "yellow" :foreground "black"))))))
              (grep-mode)
              (pop-to-buffer buffer-name)
              (goto-char (point-min)))))
      ;; Fall back to rgrep if ripgrep is not available
      (let ((default-directory directory))
        (rgrep search-term glob directory)))))
;;
(add-to-list 'display-buffer-alist
             '("\\*my-rg-results"
               (display-buffer-reuse-window display-buffer-in-direction)
               (direction . leftmost)
               (dedicated . t)
               (window-width . 0.33)
               (inhibit-same-window . t)))

;;
;; -> selected-window-accent-mode
;;

(use-package selected-window-accent-mode
  :config (selected-window-accent-mode 1)
  :custom
  (selected-window-accent-fringe-thickness 10)
  (selected-window-accent-percentage-darken 0)
  (selected-window-accent-percentage-desaturate 0)
  (selected-window-accent-smart-borders t)
  (selected-window-accent-use-blend-background nil)
  (selected-window-accent-use-blend-alpha 0)
  (selected-window-accent-tab-accent t)
  (selected-window-accent-use-pywal t)
  (selected-window-accent-custom-color "cyan4")
  (selected-window-accent-mode-style 'default))

(eval-after-load 'selected-window-accent-mode
  '(progn
     (define-key global-map (kbd "C-c w") 'selected-window-accent-transient)))

;;
;; -> transients
;;

(defvar cmake-preset
  "build/linux/debug"
  "cmake-preset")

(defun change-directory-and-run (dir command bufname)
  "Change to DIR and run the COMMAND."
  (let ((default-directory dir))
    (async-shell-command command bufname)
    (message "Running command: %s:%s" dir command)))

(defun run-exe-command (dir exe bufname)
  "Run EXE from a specified DIR."
  (message "run-exe-command: %s:%s:%s" dir exe bufname)
  (change-directory-and-run dir exe bufname))

(defun run-cmake-command (command)
  "Run COMMAND from the top level of the project."
  (message command)
  (change-directory-and-run (project-root (project-current t)) command "*cmake*"))

(defun run-cmake-compile-command (command)
  "Run compile COMMAND from the top level of the project."
  (message command)
  (let ((default-directory (project-root (project-current t))))
    (compile command)
    (message "Running command: %s:%s" dir command)))

(defun kill-async-buffer (buffer-name)
  "Kill the async buffer with BUFFER-NAME."
  (let ((buffer (get-buffer buffer-name)))
    (when buffer
      (kill-buffer buffer)
      (message "Killed buffer: %s" buffer-name))))

(defun list-cmake-presets ()
  "List available CMake presets using `cmake --list-presets=configure`."
  (let ((output (shell-command-to-string "cmake --list-presets=configure")))
    (delq nil
          (mapcar (lambda (line)
                    (if (string-match "^\\s-+\"\\([^\"]+\\)\"\\s-*$" line)
                        (match-string 1 line)))
                  (split-string output "\n")))))

(defun transient-select-cmake-preset ()
  "Function to select a CMake preset."
  (interactive)
  (let* ((presets (list-cmake-presets))
         (preset (completing-read "Select CMake preset: " presets nil t)))
    (setq cmake-preset preset)
    (message "Selected CMake preset: %s" preset)))

(transient-define-prefix build-transient ()
  "Build and Diagnostic transient commands."
  [:description (lambda () (project-root (project-current t)))
                ["CMake"
                 ("p" "Set Preset" transient-select-cmake-preset)
                 ("c" "Configure"
                  (lambda () (interactive)
                    (run-cmake-command (format "cmake --preset %s" cmake-preset))))
                 ("RET" "Build"
                  (lambda () (interactive)
                    (run-cmake-compile-command (format "cmake --build --preset %s" cmake-preset))))
                 ("i" "Install"
                  (lambda () (interactive)
                    (run-cmake-command (format "cmake --install %s" cmake-preset))))
                 ("g" "Refresh"
                  (lambda () (interactive)
                    (run-cmake-command (format "cmake --preset %s --fresh" cmake-preset))))
                 ("x" "Clean"
                  (lambda () (interactive)
                    (if (y-or-n-p "Are you sure you want to proceed? ")
                        (run-cmake-command "rm -rf build"))))
                 ;; ("m" "Toggle compilation"
                 ;;   (lambda () (interactive)
                 ;;     (let ((buffer (get-buffer "*compilation*")))
                 ;;       (if buffer
                 ;;         (if (get-buffer-window buffer 'visible)
                 ;;           (delete-windows-on buffer)
                 ;;           (display-buffer buffer))
                 ;;         (message "No *compilation* buffer found.")))))
                 ("s" "List Presets"
                  (lambda () (interactive)
                    (run-cmake-command "cmake --list-presets=configure")))]
                ["Actions"
                 ("SPC" "File Backup" my/dired-duplicate-backup-file)
                 ("f" "Toggle Flycheck" flymake-mode)
                 ("d" "Show Flycheck Diagnostics" flymake-show-buffer-diagnostics)]
                ["Coding"
                 ("e" "Fancy Stuff"
                  (lambda () (interactive)
                    (call-interactively 'eglot)
                    (company-mode 1)
                    (flymake-mode 1)))
                 ("u" "Undo Fancy Stuff"
                  (lambda () (interactive)
                    (eglot-shutdown-all)
                    (company-mode -1)
                    (flymake-mode -1)))
                 ("h" "Stop eglot"
                  (lambda () (interactive)
                    (eglot-shutdown-all)))]
                ["Run"
                 ("r" "All"
                  (lambda () (interactive)
                    (run-exe-command
                     (concat (project-root (project-current t))
                             "build/windows/debug/bin/Debug")
                     "CigiDummyIG.exe" "*Running CigiDummyIG.exe*")
                    (run-exe-command
                     (concat (project-root (project-current t))
                             "build/windows/debug/bin/Debug")
                     "CigiMiniHostCSharp.exe" "*Running CigiMiniHostCSharp.exe*")))
                 ("1" "CigiDummyIG"
                  (lambda () (interactive)
                    (run-exe-command
                     (concat (project-root (project-current t))
                             "build/windows/debug/bin/Debug")
                     "CigiDummyIG.exe"
                     "*Running CigiDummyIG.exe*")))
                 ("2" "CigiMiniHost"
                  (lambda () (interactive)
                    (run-exe-command
                     (concat (project-root (project-current t))
                             "build/windows/debug/bin/Debug")
                     "CigiMiniHost.exe"
                     "*Running CigiMiniHost.exe*")))
                 ("3" "CigiMiniHostCSharp"
                  (lambda () (interactive)
                    (run-exe-command
                     (concat (project-root (project-current t))
                             "build/windows/debug/bin/Debug")
                     "CigiMiniHostCSharp.exe"
                     "*Running CigiMiniHostCSharp.exe*")))]
                ["Kill"
                 ("5" "CigiDummyIG (k)"
                  (lambda () (interactive)
                    (kill-async-buffer "*Running CigiDummyIG.exe*")))
                 ("6" "CigiMiniHost (k)"
                  (lambda () (interactive)
                    (kill-async-buffer "*Running CigiMiniHost.exe*")))
                 ("7" "CigiMiniHostCSharp (k)"
                  (lambda () (interactive)
                    (kill-async-buffer "*Running CigiMiniHostCSharp.exe*")))
                 ("k" "All (k)"
                  (lambda () (interactive)
                    (kill-async-buffer "*Running CigiDummyIG.exe*")
                    (kill-async-buffer "*Running CigiMiniHost.exe*")
                    (kill-async-buffer "*Running CigiMiniHostCSharp.exe*")))]
                ])

(global-set-key (kbd "M-RET") #'build-transient)

(transient-define-prefix my/transient-outlining-and-folding ()
  "Transient menu for outline-mode."
  ["Outline Mode Commands"
   ["Cycle / Folding"
    ("g" "Cycle" outline-cycle)
    ("O" "Cycle Buffer" outline-cycle-buffer)
    ("F" "Global Folding at Point"
     (lambda () (interactive)
       (if (eq selective-display (1+ (current-column)))
           (set-selective-display 0)
         (set-selective-display (1+ (current-column))))))]
   ["Visibility"
    ("o" "Toggle Children" outline-toggle-children)
    ("h" "Hide Sublevels" outline-hide-sublevels)
    ("s" "Show All" outline-show-all)
    ("i" "Hide Body" outline-hide-body)
    ("e" "Show Entry" outline-show-entry)
    ("H" "Hide Entry" outline-hide-entry)
    ("c" "Hide Leaves" outline-hide-leaves)
    ("k" "Show Branches" outline-show-branches)
    ("t" "Hide Subtree" outline-hide-subtree)
    ("S" "Show Subtree" outline-show-subtree)]
   ["Motion"
    ("n" "Next Visible Heading" outline-next-visible-heading)
    ("p" "Previous Visible Heading" outline-previous-visible-heading)
    ("u" "Up Heading" outline-up-heading)
    ("f" "Forward Same Level" outline-forward-same-level)
    ("b" "Backward Same Level" outline-backward-same-level)]
   ["Structure"
    ("t" "Promote Heading" outline-promote)
    ("d" "Demote Heading" outline-demote)
    ("P" "Move Subtree Up" outline-move-subtree-up)
    ("N" "Move Subtree Down" outline-move-subtree-down)]
   ["Edit"
    ("a" "Add Heading" outline-insert-heading)
    ("r" "Rename Heading" outline-insert-heading)
    ("m" "Mark Subtree" outline-mark-subtree)]])

(bind-key* (kbd "C-c o") 'my/transient-outlining-and-folding)

(defun my/prog-folding ()
  "Enable and configure outline minor mode for code folding.
This function sets up the outline minor mode tailored for
programming modes based on basic space / tab indentation."
  (interactive)
  (setq-local outline-minor-mode-use-buttons nil)
  (setq-local outline-regexp (rx bol
                                 (zero-or-more (any " \t"))
                                 (not (any " \t\n"))))
  (outline-minor-mode 1))

(add-hook 'prog-mode-hook 'my/prog-folding)

(with-eval-after-load 'chatgpt-shell
  (transient-define-prefix chatgpt-shell-transient ()
    "Transient for ChatGPT Shell commands."
    ["ChatGPT Shell Commands"
     ["Code and Text"
      ("e" "Explain Code" chatgpt-shell-explain-code)
      ("p" "Proofread Region" chatgpt-shell-proofread-region)
      ("g" "Write Git Commit" chatgpt-shell-write-git-commit)
      ("s" "Send Region" chatgpt-shell-send-region)
      ("d" "Describe Code" chatgpt-shell-describe-code)
      ("r" "Refactor Code" chatgpt-shell-refactor-code)
      ("u" "Generate Unit Test" chatgpt-shell-generate-unit-test)
      ("a" "Send and Review Region" chatgpt-shell-send-and-review-region)]
     ["Shell Operations"
      ("l" "Start Shell" chatgpt-shell)
      ;;    ("m" "Swap Model Version" chatgpt-shell-swap-model-version)
      ("t" "Save Session Transcript" chatgpt-shell-save-session-transcript)]
     ["Eshell Integrations"
      ("o" "Summarize Last Command Output" chatgpt-shell-eshell-summarize-last-command-output)
      ("w" "What's Wrong With Last Command" chatgpt-shell-eshell-whats-wrong-with-last-command)]
     ["Miscellaneous"
      ("i" "Describe Image" chatgpt-shell-describe-image)
      ("m" "Swap Model" chatgpt-shell-swap-model)]
     ])

  (global-set-key (kbd "C-c g") 'chatgpt-shell-transient))

;;
;; -> kurecolor
;;

(use-package kurecolor
  :ensure t ; Ensure the package is installed (optional)
  :bind (("M-<up>" . (lambda () (interactive) (kurecolor-increase-brightness-by-step 0.2)))
         ("M-<down>" . (lambda () (interactive) (kurecolor-decrease-brightness-by-step 0.2)))
         ("M-<prior>" . (lambda () (interactive) (kurecolor-increase-saturation-by-step 0.2)))
         ("M-<next>" . (lambda () (interactive) (kurecolor-decrease-saturation-by-step 0.2)))
         ("M-<left>" . (lambda () (interactive) (kurecolor-decrease-hue-by-step 0.2)))
         ("M-<right>" . (lambda () (interactive) (kurecolor-increase-hue-by-step 0.2))))
  :config
  (global-set-key (kbd "M-<home>") 'my/insert-random-color-at-point))

(defun my/insert-random-color-at-point ()
  "Generate random color and insert at current hex color under cursor."
  (interactive)
  (let* ((color (format "#%06x" (random (expt 16 6))))
         (bounds (bounds-of-thing-at-point 'sexp))
         (start (car bounds))
         (end (cdr bounds)))
    (if (and bounds (> end start))
        (progn
          (goto-char start)
          (unless (looking-at "#[0-9a-fA-F]\\{6\\}")
            (error "Not on a hex color code"))
          (delete-region start end)
          (insert color))
      (error "No hex color code at point"))))

;;
;; -> calendar
;;

(use-package calfw)
(use-package calfw-org)
(use-package calfw-cal)

(setq calendar-holidays nil)
(setq calendar-week-start-day 1)

(setq cfw:org-capture-template
      '("c" "Calendar" plain
        (file+function
         "~/DCIM/content/aaa--calendar.org"
         my-capture-top-level)
        "* TODO %?\n SCHEDULED: %(cfw:org-capture-day)\n"
        :prepend t :jump-to-captured t))

;;
;; -> plantuml
;;

(use-package plantuml-mode
  :custom
  (plantuml-default-exec-mode 'jar)
  (plantuml-jar-path (concat user-emacs-directory "plantuml.jar"))
  (org-plantuml-jar-path (concat user-emacs-directory "plantuml.jar")))

(add-to-list
 'org-src-lang-modes '("plantuml" . plantuml))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((plantuml . t)))

(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))

(add-hook 'plantuml-mode-hook (lambda ()
                                (setq tab-width 0)
                                (setq indent-tabs-mode nil)))


(defun my-org-confirm-babel-evaluate (lang body)
  (not (or (string= lang "plantuml")
           (string= lang "emacs-lisp"))))

(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

;;
;; -> org-agenda
;;

(use-package org
  :custom
  (org-agenda-include-diary nil)
  (org-agenda-show-all-dates t)
  (org-refile-targets '((org-agenda-files :maxlevel . 1)))
  (org-agenda-files '("~/DCIM/content/aaa--aaa.org"
                      "~/DCIM/content/aaa--todo.org"
                      "~/DCIM/content/aab--house.org"
                      "~/DCIM/content/aaa--calendar.org"
                      "~/DCIM/content/aac--baby.org"
                      "~/DCIM/content/aaf--kate.org"
                      "~/DCIM/content/aag--emacs-todo.org"
                      ))
  :config
  (with-eval-after-load 'org-agenda
    (unbind-key "M-m" org-agenda-mode-map)
    (setq org-agenda-custom-commands
          '(("m" "Month View" agenda ""
             ((org-agenda-start-day "today")
              (org-agenda-span 30)
              (org-agenda-time-grid nil)))
            ("0" "Year View (2020)" agenda ""
             ((org-agenda-start-day "2020-01-01")
              (org-agenda-span 'year)
              (org-agenda-time-grid nil)))
            ("1" "Year View (2021)" agenda ""
             ((org-agenda-start-day "2021-01-01")
              (org-agenda-span 'year)
              (org-agenda-time-grid nil)))
            ("2" "Year View (2022)" agenda ""
             ((org-agenda-start-day "2022-01-01")
              (org-agenda-span 'year)
              (org-agenda-time-grid nil)))
            ("3" "Year View (2023)" agenda ""
             ((org-agenda-start-day "2023-01-01")
              (org-agenda-span 'year)
              (org-agenda-time-grid nil)))
            ("4" "Year View (2024)" agenda ""
             ((org-agenda-start-day "2024-01-01")
              (org-agenda-span 'year)
              (org-agenda-time-grid nil)))))))

;;
;; -> dwim
;;

(when (file-exists-p "/home/jdyer/bin/category-list-uniq.txt")
  (progn
    (defvar my/dwim-convert-commands
      '("ConvertNoSpace" "AudioConvert" "AudioInfo" "AudioNormalise"
        "AudioTrimSilence" "PictureAutoColour" "PictureConvert"
        "PictureCrush" "PictureFrompdf" "PictureInfo" "PictureMontage"
        "PictureOrganise" "PictureCrop" "PictureRotateFlip" "PictureEmail"
        "PictureUpdateFromCreateDate"
        "PictureRotateLeft" "PictureRotateRight" "PictureScale"
        "PictureUpscale" "PictureGetText" "PictureOrientation"
        "PictureUpdateToCreateDate" "VideoConcat" "VideoConvert" "VideoConvertToGif"
        "VideoCut" "VideoDouble" "VideoExtractAudio" "VideoExtractFrames"
        "VideoFilter" "VideoFromFrames" "VideoInfo" "VideoRemoveAudio"
        "VideoReplaceVideoAudio" "VideoRescale" "VideoReverse"
        "VideoRotate" "VideoRotateLeft" "VideoRotateRight" "VideoShrink"
        "VideoSlowDown" "VideoSpeedUp" "VideoZoom" "WhatsAppConvert"
        "PictureCorrect" "Picture2pdf" "PictureTag" "PictureTagRename"
        "OtherTagDate" "VideoRemoveFlips")
      "List of commands for dwim-convert.")

    (defun my/read-lines (file-path)
      "Return a list of lines of a file at FILE-PATH."
      (with-temp-buffer
        (insert-file-contents file-path)
        (split-string (buffer-string) "\n" t)))

    (defun my/dwim-convert-generic (command)
      "Execute a dwim-shell-command-on-marked-files with the given COMMAND."
      (let* ((unique-text-file "~/bin/category-list-uniq.txt")
             (user-selection nil)
             (files (dired-get-marked-files nil current-prefix-arg))
             (command-and-files (concat command " " (mapconcat 'identity files " "))))
        (when (string= command "PictureTag")
          (setq user-selection (completing-read "Choose an option: "
                                                (my/read-lines unique-text-file)
                                                nil t)))
        (async-shell-command (if user-selection
                                 (concat command " " user-selection " " (mapconcat 'identity files " "))
                               (concat command " " (mapconcat 'identity files " ")))
                             "*convert*")))

    (defun my/dwim-convert-with-selection ()
      "Prompt user to choose command and execute dwim-shell-command-on-marked-files."
      (interactive)
      (let ((chosen-command (completing-read "Choose command: "
                                             my/dwim-convert-commands)))
        (my/dwim-convert-generic chosen-command)))

    (global-set-key (kbd "C-c v") 'my/dwim-convert-with-selection)))

;;
;; -> org-capture
;;

(setq bookmark-fringe-mark nil)

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

(defun my/test-finalize ()
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

(add-hook 'org-capture-after-finalize-hook 'my/test-finalize)

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
;; -> magit
;;

(when (executable-find "git")
  (use-package magit
    :defer 5
    :config
    (magit-add-section-hook
     'magit-status-sections-hook 'magit-insert-tracked-files nil 'append)
    :custom
    (magit-section-initial-visibility-alist (quote ((untracked . hide))))
    (magit-repolist-column-flag-alist
     '((magit-untracked-files . "N")
       (magit-unstaged-files . "U")
       (magit-staged-files . "S")))
    (magit-repolist-columns
     '(("Name" 25 magit-repolist-column-ident nil)
       ("" 3 magit-repolist-column-flag)
       ("Version" 25 magit-repolist-column-version
        ((:sort magit-repolist-version<)))
       ("B<U" 3 magit-repolist-column-unpulled-from-upstream
        ((:right-align t)
         (:sort <)))
       ("B>U" 3 magit-repolist-column-unpushed-to-upstream
        ((:right-align t)
         (:sort <)))
       ("Path" 99 magit-repolist-column-path nil)))
    (magit-repository-directories
     '(("~/.config" . 0)
       ("~/source/repos" . 2)
       ("~/bin" . 1)
       ("~/.emacs.d" . 1)
       ("~/.emacs.d.core" . 1)
       ("~/DCIM/Art/Content" . 2)
       ("~/DCIM/themes" . 2)))))

;;
;; -> use-package
;;
(use-package doom-themes)
(use-package ef-themes)
(use-package gruvbox-theme)
(use-package htmlize)
(use-package org-kanban)
(use-package org-ql)
(use-package embark)
(use-package embark-consult)

(use-package ox-hugo
  :defer t
  :config
  (setq org-hugo-front-matter-format "yaml"))

(use-package deadgrep
  :config
  (setq-default deadgrep--search-case 'ignore)
  :custom
  (deadgrep-max-buffers 1)
  (deadgrep-extra-arguments '("--no-config")))
;; (deadgrep-extra-arguments '("--no-config" "--no-ignore" "--no-ignore-vcs")))

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
;; -> image-dired
;;

(require 'image-mode)
(require 'image-dired)

(add-to-list 'display-buffer-alist
             '("\\*image-dired\\*"
               display-buffer-in-direction
               (direction . left)
               (window . root)
               (window-width . 0.5)))

(add-to-list 'display-buffer-alist
             '("\\*image-dired-display-image\\*"
               display-buffer-in-direction
               (direction . right)
               (window . root)
               (window-width . 0.5)))

(defun my/image-dired-sort (arg)
  "Sort images in various ways given ARG."
  (interactive "P")
  ;; Use `let` to temporarily set `dired-actual-switches`
  (let ((dired-actual-switches
         (cond
          ((equal arg nil)            ; no C-u
           "-lGghat --ignore=*.xmp")
          ((equal arg '(4))           ; C-u
           "-lGgha --ignore=*.xmp")
          ((equal arg 1)              ; C-u 1
           "-lGgha --ignore=*.xmp"))))
    (let ((w (selected-window)))
      (delete-other-windows)
      (revert-buffer)
      (image-dired ".")
      (let ((idw (selected-window)))
        (select-window w)
        (dired-unmark-all-marks)
        (select-window idw)
        (image-dired-display-this)
        (image-dired-line-up-dynamic)))))

(setq image-use-external-converter t)
(setq image-dired-external-viewer "/usr/bin/gthumb")
(setq image-dired-show-all-from-dir-max-files 999)
(setq image-dired-thumbs-per-row 999)
(setq image-dired-thumb-relief 0)
(setq image-dired-thumb-margin 5)
(setq image-dired-thumb-size 120)

(defun my/image-save-as ()
  "Save the current image buffer as a new file."
  (interactive)
  (let* ((file (buffer-file-name))
         (dir (file-name-directory file))
         (name (file-name-nondirectory file))
         (base-name (file-name-sans-extension name))
         (extension (file-name-extension name t))
         (initial_mode major-mode)
         (counter 1)
         (new-file))
    (while (and (setq new-file
                      (format "%s%s_%03d%s" dir base-name counter extension))
                (file-exists-p new-file))
      (setq counter (1+ counter)))
    (write-region (point-min) (point-max) new-file nil 'no-message)
    (revert-buffer nil t nil)
    ;; (delete-file file t)
    (if (equal initial_mode 'image-dired-image-mode)
        (progn
          (image-dired ".")
          (image-dired-display-this))
      (find-file new-file t))))

(defun my/delete-current-image-and-move-to-next ()
  "Delete the current image file and move to the next image in the directory."
  (interactive)
  (let ((current-file (buffer-file-name)))
    (when current-file
      (image-next-file 1)
      (delete-file current-file)
      (message "Deleted %s" current-file))))

(defun my/delete-current-image-thumbnails ()
  "Delete the current image file and move to the next image in the directory."
  (interactive)
  (let ((file-name (image-dired-original-file-name)))
    (delete-file file-name)
    (image-dired-delete-char)
    (image-dired-display-this)))

(eval-after-load 'image-mode
  '(progn
     (define-key image-mode-map (kbd "C-d") 'my/delete-current-image-and-move-to-next)
     (define-key image-mode-map (kbd "C-x C-s") 'my/image-save-as)))

(eval-after-load 'image-dired
  '(progn
     (define-key image-dired-thumbnail-mode-map (kbd "C-d") 'my/delete-current-image-thumbnails)
     (define-key image-dired-thumbnail-mode-map (kbd "n")
                 (lambda ()(interactive)(image-dired-forward-image)(image-dired-display-this)))
     (define-key image-dired-thumbnail-mode-map (kbd "p")
                 (lambda ()(interactive)(image-dired-backward-image)(image-dired-display-this)))
     ))

;;
;; -> keys-navigation
;;

(define-key my-jump-keymap (kbd "f") #'my/find-file)
(define-key my-jump-keymap (kbd "k")
            (lambda () (interactive)
              (find-file (concat user-emacs-directory "emacs--init.org"))))
(define-key my-jump-keymap (kbd "l") #'recentf-open)

;;
;; -> completion
;;

(use-package capf-autosuggest)

(use-package eglot
  :hook
  (eglot-managed-mode
   . (lambda ()
       (setq-local completion-at-point-functions
                   (list (cape-capf-super
                          #'cape-dabbrev
                          #'eglot-completion-at-point)))))
  :custom
  (eglot-ignored-server-capabilities
   '(
     ;; :hoverProvider                    ; Provides information when you hover over code elements.
     ;; :completionProvider               ; Provides code completion suggestions.
     ;; :signatureHelpProvider            ; Offers signature information for functions/methods.
     ;; :definitionProvider               ; Finds the definition of variables/functions.
     ;; :typeDefinitionProvider           ; Finds the type definition of variables/functions.
     ;; :implementationProvider           ; Finds the implementation of types/functions.
     ;; :declarationProvider              ; Finds the declaration of variables/types.
     ;; :referencesProvider               ; Finds all references to the symbol at the caret.
     ;; :documentHighlightProvider        ; Highlights references to the symbol at the caret.
     ;; :documentSymbolProvider           ; Lists all symbols in a document.
     ;; :workspaceSymbolProvider          ; Lists symbols across workspace/project.
     ;; :codeActionProvider               ; Suggests code actions (like quick fixes).
     ;; :codeLensProvider                 ; Displays inline code actions or information.
     ;; :documentFormattingProvider       ; Formats an entire document.
     ;; :documentRangeFormattingProvider  ; Formats a specified range in a document.
     ;; :documentOnTypeFormattingProvider ; Formats code as you type.
     ;; :renameProvider                   ; Refactors/renames symbols.
     ;; :documentLinkProvider             ; Handles clickable links in documents.
     ;; :colorProvider                    ; Provides color information for document.
     ;; :foldingRangeProvider             ; Supports code folding.
     ;; :executeCommandProvider           ; Allows execution of commands.
     ;; :inlayHintProvider                ; Displays inline hints (e.g., parameter names).
     ))
  (eglot-send-changes-idle-time 2.0))

(use-package corfu
  :custom
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-auto nil)
  (corfu-separator ?\s)
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match nil)
  (corfu-preview-current nil)
  (corfu-preselect 'first)
  (corfu-on-exact-match nil)
  (corfu-scroll-margin 5))

(use-package company
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-selection))
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay nil))

(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

(use-package tempel
  :diminish tempel-abbrev-mode global-tempel-abbrev-mode abbrev-mode
  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))
  :init
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
  (add-hook 'conf-mode-hook #'tempel-setup-capf)
  (add-hook 'prog-mode-hook #'tempel-setup-capf)
  (add-hook 'text-mode-hook #'tempel-setup-capf)
  (global-tempel-abbrev-mode))

;;
;; -> keys-visual
;;
(define-key my-win-keymap (kbd "a") #'selected-window-accent-mode)
(define-key my-win-keymap (kbd "m") #'consult-theme)
(define-key my-win-keymap (kbd "w") #'org-wc-display)

;;
;; -> keys-other
;;
(global-set-key (kbd "M-s e") #'my/push-block)
(global-set-key (kbd "M-s g") #'my/grep)
(bind-key* (kbd "M-s c") #'cfw:open-org-calendar)

;;
;; -> keybinding
;;

(global-set-key (kbd "C-c b") #'(lambda ()(interactive)(async-shell-command "do_backup home" "*backup*")))
(global-set-key (kbd "C-c c") #'org-capture)
(bind-key* (kbd "C-c ,") #'embark-act)
(define-key minibuffer-local-map (kbd "C-c c") #'embark-collect)
(define-key minibuffer-local-map (kbd "C-c e") #'embark-export)

;;
;; -> defun
;;
(defun my/sync-tab-bar-to-theme ()
  "Synchronize tab-bar faces with the current theme."
  (interactive)
  (let ((default-bg (face-background 'default))
        (default-fg (face-foreground 'default))
        (inactive-fg (face-foreground 'mode-line-inactive))) ;; Fallback to mode-line-inactive
    (custom-set-faces
     `(tab-bar ((t (:inherit default :font "Monospace 12" :background ,default-bg :foreground ,default-fg))))
     `(tab-bar-tab ((t (:inherit default :background ,default-fg :foreground ,default-bg))))
     `(tab-bar-tab-inactive ((t (:inherit default :background ,default-bg :foreground ,inactive-fg)))))))
(my/sync-tab-bar-to-theme)
;;
(defun my/dired-file-to-org-link ()
  "Transform the file path under the cursor in Dired to an Org mode
  link and copy to kill ring.
  This function transforms the current file path in Dired mode into
  an Org link with attributes for both org-mode and HTML width
  settings. The generated link is then copied to the kill ring for
  easy pasting."
  (interactive)
  (let ((file-path (dired-get-file-for-visit)))
    (if file-path
        (let* ((relative-path (file-relative-name file-path
                                                  (project-root (project-current t))))
               (org-link (concat "#+attr_org: :width 300px\n"
                                 "#+attr_html: :width 100%\n"
                                 "[[file:" relative-path "]]\n")))
          (kill-new org-link)
          (message "Copied to kill ring: %s" org-link))
      (message "No file under the cursor"))))
;;
(defun my/grep (arg)
  "Wrapper to grep with ARG."
  (interactive "p")
  (let ((search-term
         (if (equal major-mode 'dired-mode)
             (read-from-minibuffer "Search : ")
           (read-from-minibuffer "Search : " (thing-at-point 'symbol)))))
    (if (= arg 1)
        (deadgrep search-term default-directory)
      (progn
        (setq current-prefix-arg nil)
        (deadgrep search-term "~")))))

;;
;; -> org
;;

(setq org-src-tab-acts-natively t
      org-edit-src-content-indentation 0
      org-log-done t
      org-tags-sort-function 'org-string-collate-greaterp
      org-export-with-sub-superscripts nil
      org-deadline-warning-days 365
      org-hugo-base-dir "~/DCIM"
      org-image-actual-width (list 50)
      org-return-follows-link t
      org-use-fast-todo-selection 'expert
      org-reverse-note-order t
      org-todo-keywords
      ;; '((sequence "TODO(t)" "DOING(d)" "ORDR(o)" "SENT(s)" "|" "DONE(n)" "CANCELLED(c)"))
      '((sequence "TODO" "DOING" "ORDR" "SENT" "|" "DONE" "CANCELLED"))
      org-todo-keyword-faces
      '(("TODO" . "#ee5566")
        ("DOING" . "#5577aa")
        ("ORDR" . "#bb44ee")
        ("SENT" . "#bb44ee")
        ("DONE" . "#77aa66")
        ("CANCELLED" . "#426b3e"))
      org-cycle-separator-lines 0)

;;
;; -> visuals
;;
(set-frame-parameter nil 'alpha-background 75)
(add-to-list 'default-frame-alist '(alpha-background . 75))

;;
;; -> dired
;;
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "W") 'dired-do-async-shell-command)
  (define-key dired-mode-map (kbd "b") 'my/dired-file-to-org-link)
  (define-key dired-mode-map (kbd "C-c i") 'my/image-dired-sort)
  (setq dired-guess-shell-alist-user
        '(("\\.\\(jpg\\|jpeg\\|png\\|gif\\|bmp\\)$" "gthumb")
          ("\\.\\(mp4\\|mkv\\|avi\\|mov\\|wmv\\|flv\\|mpg\\)$" "mpv")
          ("\\.\\(mp3\\|wav\\|ogg\\|\\)$" "mpv")
          ("\\.\\(kra\\)$" "org.kde.krita")
          ("\\.\\(odt\\|ods\\)$" "libreoffice")
          ("\\.\\(html\\|htm\\)$" "firefox")
          ("\\.\\(pdf\\|epub\\)$" "xournalpp"))))

;;
;; -> spelling
;;

(use-package jinx)

(use-package powerthesaurus
  :init
  (require 'transient)
  (transient-define-prefix my/transient-spelling ()
    "Spelling commands"
    ["Spelling"
     ["Lookups"
      ("t" "Synonyms" powerthesaurus-lookup-synonyms-dwim)
      ("a" "Antonyms" powerthesaurus-lookup-antonyms-dwim)]
     ["Spelling Tools"
      ("l" "Jinx" (lambda ()(interactive)
                    (call-interactively 'jinx-mode)
                    (call-interactively 'flymake-mode)))
      ("j" "Jinx correct" jinx-correct)
      ("s" "Jinx correct" jinx-correct)]
     ["Dictionary"
      ("d" "Lookup" dictionary-lookup-definition)]]
    )
  :bind
  ("C-c s" . my/transient-spelling))

(setq dictionary-default-dictionary "*")
(setq dictionary-server "dict.org")
(setq dictionary-use-single-buffer t)

;;
;; -> shell
;;
(defun my/eshell-hook ()
  "Set up company completions to be a little more fish like."
  (interactive)
  (setq-local completion-styles '(basic partial-completion))
  (capf-autosuggest-mode)
  (setq-local completion-at-point-functions
              (list (cape-capf-super
                     #'pcomplete-completions-at-point
                     #'cape-history)))
  (define-key eshell-hist-mode-map (kbd "M-r") #'consult-history))

(use-package eshell
  :config
  (setq eshell-scroll-to-bottom-on-input t)
  (setq-local tab-always-indent 'complete)
  (setq eshell-history-size 10000) ;; Adjust size as needed
  (setq eshell-save-history-on-exit t) ;; Enable history saving on exit
  (setq eshell-hist-ignoredups t) ;; Ignore duplicates
  :hook
  (eshell-mode . my/eshell-hook))

(use-package popper
  :init
  (setq popper-reference-buffers
        '("\\*eshell.*"
          "\\*convert.*"
          "\\*eldoc.*"
          flymake-diagnostics-buffer-mode
          help-mode
          compilation-mode))
  (popper-mode 1)
  (popper-echo-mode 1)
  :custom
  (popper-window-height 18))

(bind-key* (kbd "C-c '") #'popper-toggle)
(bind-key* (kbd "C-c ;") #'popper-toggle-type)

;;
;; -> linux specific
;;

(when (eq system-type 'gnu/linux)
  (define-key my-jump-keymap (kbd "b") (lambda () (interactive) (find-file "~/bin")))
  (define-key my-jump-keymap (kbd "c") (lambda () (interactive) (find-file "~/DCIM/Camera")))
  (define-key my-jump-keymap (kbd "g") (lambda () (interactive) (find-file "~/.config")))
  (define-key my-jump-keymap (kbd "j") (lambda () (interactive) (find-file "~/DCIM/content/aaa--todo.org")))
  (define-key my-jump-keymap (kbd "n") (lambda () (interactive) (find-file "~/DCIM/Screenshots")))
  (define-key my-jump-keymap (kbd "w") (lambda () (interactive) (find-file "~/DCIM/content/")))
  ;; (setq font-general "Noto Sans Mono 11")
  (setq font-general "Source Code Pro 12")
  ;; (setq font-general "Source Code Pro Light 11")
  ;; (setq font-general "Monospace 11")
  ;;(setq font-general "Nimbus Mono PS 13")
  (set-frame-font font-general nil t)
  (add-to-list 'default-frame-alist `(font . ,font-general))
  (setq diary-file "~/DCIM/content/diary.org"))

;;
;; -> LLM
;;

(use-package ellama
  :bind ("C-c e" . ellama-transient-main-menu)
  :init
  (setopt ellama-language "English")
  (require 'llm-ollama)
  (setopt ellama-provider
          (make-llm-ollama
           :chat-model "qwen2.5-coder-7b-instruct-q5_k_m"
           :embedding-model "qwen2.5-coder-7b-instruct-q5_k_m"
           :default-chat-non-standard-params '(("num_ctx" . 8192))))
  (setopt ellama-providers
          '(("codellama-7b.Q5_K_M" .
             (make-llm-ollama
              :chat-model "codellama-7b.Q5_K_M"
              :embedding-model "codellama-7b.Q5_K_M"))
            ("qwen2.5-coder-7b-instruct-q5_k_m" .
             (make-llm-ollama
              :chat-model "qwen2.5-coder-7b-instruct-q5_k_m"
              :embedding-model "qwen2.5-coder-7b-instruct-q5_k_m"))
            ("Llama-3.2-1B-Instruct-Q8_0" .
             (make-llm-ollama
              :chat-model "Llama-3.2-1B-Instruct-Q8_0"
              :embedding-model "Llama-3.2-1B-Instruct-Q8_0"))))
  ;; (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
  (setopt ellama-naming-scheme 'ellama-generate-name-by-words)
  (setopt ellama-translation-provider (make-llm-ollama
                                       :chat-model "qwen2.5-coder-7b-instruct-q5_k_m"
                                       :embedding-model "nomic-embed-text"))
  :config
  (setq ellama-sessions-directory "~/.config/emacs/ellama-sessions/"
        ellama-sessions-auto-save t))

(use-package gptel
  :config
  (gptel-make-ollama "qwen2.5-coder-7b-instruct-q5_k_m"
    :host "localhost:11434"
    :stream t
    :models '(qwen2.5-coder-7b-instruct-q5_k_m:latest))
  (setq gptel-model 'qwen2.5-coder-7b-instruct-q5_k_m:latest
        gptel-backend (gptel-make-ollama "qwen2.5-coder-7b-instruct-q5_k_m"
                        :host "localhost:11434"
                        :stream t
                        :models '(qwen2.5-coder-7b-instruct-q5_k_m:latest))))

(use-package chatgpt-shell
  :custom
  (chatgpt-shell-openai-key
   (lambda ()
     (auth-source-pass-get 'secret "openai-key")))
  (chatgpt-shell-models
   '(((:version . "chatgpt-4o-latest")
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
      (:validate-command . chatgpt-shell-openai--validate-command))
     ((:provider . "Ollama")
      (:label . "Ollama-qwen")
      (:version . "qwen2.5-coder-7b-instruct-q5_k_m")
      (:short-version)
      (:token-width . 4)
      (:context-window . 8192)
      (:handler . chatgpt-shell-ollama--handle-ollama-command)
      (:filter . chatgpt-shell-ollama--extract-ollama-response)
      (:payload . chatgpt-shell-ollama-make-payload)
      (:url . chatgpt-shell-ollama--make-url))
     ((:provider . "Ollama")
      (:label . "Ollama-llama")
      (:version . "Llama-3.2-1B-Instruct-Q8_0")
      (:short-version)
      (:token-width . 4)
      (:context-window . 8192)
      (:handler . chatgpt-shell-ollama--handle-ollama-command)
      (:filter . chatgpt-shell-ollama--extract-ollama-response)
      (:payload . chatgpt-shell-ollama-make-payload)
      (:url . chatgpt-shell-ollama--make-url)))))

;;
;; -> window-positioning
;;
(add-to-list 'display-buffer-alist
             '("\\*Async" display-buffer-no-window
               (allow-no-window . t)))

(add-to-list 'display-buffer-alist
             '("\\*Messages" display-buffer-same-window))

(add-to-list 'display-buffer-alist
             '("\\*deadgrep"
               (display-buffer-reuse-window display-buffer-in-direction)
               (direction . leftmost)
               (dedicated . t)
               (window-width . 0.33)
               (inhibit-same-window . t)))

(add-to-list 'display-buffer-alist
             '("\\*compilation"
               (display-buffer-reuse-window display-buffer-in-direction)
               (direction . leftmost)
               (dedicated . t)
               (window-width . 0.3)
               (inhibit-same-window . t)))

(add-to-list 'display-buffer-alist
             '("\\Running"
               (display-buffer-reuse-window display-buffer-in-direction)
               (direction . leftmost)
               (dedicated . t)
               (window-width . 0.33)
               (inhibit-same-window . t)))

(add-to-list 'display-buffer-alist
             '("\\*Help\\*"
               (display-buffer-reuse-window display-buffer-same-window)))

;;
;; -> programming
;;

(setq my/old-ada-mode (concat user-emacs-directory "old-ada-mode"))
(when (file-exists-p my/old-ada-mode)
  (use-package ada-mode
    :load-path my/old-ada-mode))

(use-package yaml-mode)

(setq eldoc-echo-area-use-multiline-p nil)

(setq vc-handled-backends '(SVN Git))

;;
;; -> development
;;
(defun without-gc (&rest args)
  (let ((gc-cons-threshold most-positive-fixnum))
    (apply args)))

(defun my/push-block ()
  "Export content from one file to another in various formats given VALUE."
  (interactive)
  (save-excursion
    (without-gc #'org-hugo-export-wim-to-md)
    (mapc 'shell-command
          '("web rsync emacs" "web rsync art"
            "web rsync dyerdwelling"))))

(add-hook 'org-mode-hook 'org-indent-mode)
