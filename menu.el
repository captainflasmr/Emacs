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

    (defvar my/org-dired-marked-files nil
      "Stores the current dired marked files.")

    (defun my/read-lines (file-path)
      "Return a list of lines of a file at FILE-PATH."
      (with-temp-buffer
        (insert-file-contents file-path)
        (split-string (buffer-string) "\n" t)))

    (defun my/dwim-convert-generic-menu (command)
      "Execute a dwim-shell-command-on-marked-files with the given COMMAND."
      (let* ((unique-text-file "/home/jdyer/bin/category-list-uniq.txt")
             (user-selection nil)
             (files my/org-dired-marked-files)
             (command-and-files (concat command " " (mapconcat 'identity files " "))))
        (prin1 files)
        (when (string= command "PictureTag")
          (setq user-selection (completing-read "Choose an option: "
                                                (my/read-lines unique-text-file)
                                                nil t)))
        (async-shell-command (if user-selection
                                 (concat command " " user-selection " " (mapconcat 'identity files " "))
                               (concat command " " (mapconcat 'identity files " ")))
                             "*convert*")))
    ;; (save-buffers-kill-terminal))

    (defun my/dwim-convert-with-selection-files-command (files-string chosen-command)
      "Prompt user to choose command and execute dwim-shell-command-on-marked-files."
      (interactive)
      (setq my/org-dired-marked-files (split-string files-string ";" t))
      (my/dwim-convert-generic-menu chosen-command))

    (defun my/dwim-convert-with-selection-files (files-string)
      "Prompt user to choose command and execute dwim-shell-command-on-marked-files."
      (interactive)
      (setq my/org-dired-marked-files (split-string files-string ";" t))
      (let ((chosen-command (completing-read "Choose command: "
                                             my/dwim-convert-commands)))
        (my/dwim-convert-generic-menu chosen-command)))
    )
  )
