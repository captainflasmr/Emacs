;; -*- lexical-binding: t; -*-

;;
;; -> core-configuration
;;
(load-file "~/.emacs.d/Emacs-core/init.el")

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
(use-package org-wc)
(use-package git-timemachine)

(use-package ox-hugo
  :defer t
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

(use-package org-superstar
  :hook
  (org-mode . org-superstar-mode))

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
(set-frame-parameter nil 'alpha-background 50)
(add-to-list 'default-frame-alist '(alpha-background . 50))

;;
;; -> dired
;;
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "W") 'dired-do-async-shell-command)
  (define-key dired-mode-map (kbd "b") 'my/dired-file-to-org-link)
  (setq dired-guess-shell-alist-user
        '(("\\.\\(jpg\\|jpeg\\|png\\|gif\\|bmp\\)$" "gthumb")
          ("\\.\\(mp4\\|mkv\\|avi\\|mov\\|wmv\\|flv\\|mpg\\)$" "mpv")
          ("\\.\\(mp3\\|wav\\|ogg\\|\\)$" "mpv")
          ("\\.\\(kra\\)$" "org.kde.krita")
          ("\\.\\(odt\\|ods\\)$" "libreoffice")
          ("\\.\\(html\\|htm\\)$" "firefox")
          ("\\.\\(pdf\\|epub\\)$" "xournalpp"))))

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

(defun my/org-ql-tags-search-in-current-buffer ()
  "Prompt the user for a tag from the current buffer and generate a TODO list ordered by timestamp."
  (interactive)
  ;; Check if the buffer is in 'org-mode'
  (if (derived-mode-p 'org-mode)
      (let* ((tags (mapcar #'car (org-global-tags-completion-table (list (buffer-file-name)))))
             (chosen-tag (completing-read
                          "Choose a tag from the current buffer: "
                          tags)))
        (org-ql-search
          (current-buffer)
          `(and (tags ,chosen-tag))
          :title (format "tag: %s" chosen-tag)
          :sort 'todo))
    (message "This command must be run in an Org buffer.")))

(bind-key* (kbd "M-s s") #'my/org-ql-tags-search-in-current-buffer)

;;
;; -> modes
;;
(server-mode 1)
