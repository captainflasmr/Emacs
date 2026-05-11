;;; obp-shortcodes.el --- Custom shortcode handlers for org-bootstrap-publish  -*- lexical-binding: t; -*-

;;; Commentary:

;; Loaded by async/batch builds so custom shortcodes are available
;; when the child Emacs does not load user init.

;;; Code:

(defun my/obp-shortcode-horseprediction (_args)
  (concat
   "<form name=\"horselist\" id=\"horselist\">"
   "<textarea name=\"thehorselist\" rows=\"10\"></textarea>"
   "</form>"
   "<input onclick=\"read_horse_lines();\" type=\"button\" value=\"Find Winner\" />"
   "<input onclick=\"clear_horses();\" type=\"button\" value=\"Clear\" />"
   "<script src=\"/static/assets/js/script.js\" defer></script>"))

(defun my/obp-shortcode-crossword (args)
  (let ((src (plist-get args :src)))
    (concat
     "<h2>" src "</h2>"
     "<div style=\"margin:auto;display:flex;flex-direction:column;height:500px;max-width:500px\">"
     "<iframe src=\"https://crosswordlabs.com/embed/" src
     "\" style=\"flex:1;width:100%;padding:5px 0 0 5px;border:3px solid black\"></iframe>"
     "<a target=\"_blank\" style=\"align-self:center;font-size:12px;color:black;padding-top:10px;text-decoration:none;text-align:center\" href=\"https://crosswordlabs.com\">Crossword Puzzle Maker</a>"
     "</div>")))

(defun my/obp-shortcode-foldergallery (args)
  (let* ((src (plist-get args :src))
         (root (file-name-directory
                (or (and org-bootstrap-publish-source-files
                         (car org-bootstrap-publish-source-files))
                    org-bootstrap-publish-source-file)))
         (dir  (and src root (expand-file-name (concat "static/" src "/") root)))
         (files (and dir (file-directory-p dir)
                     (sort (directory-files
                            dir nil
                            "\\.\\(?:gif\\|webp\\|jpe?g\\|tiff\\|png\\|bmp\\)\\'"
                            t)
                           #'string-greaterp))))
    (if (not files) ""
      (concat
       "<div style=\"display:flex;flex-wrap:wrap;border:0\">"
       (mapconcat
        (lambda (f)
          (let ((url (concat "/static/" src "/" f)))
            (format "<a href=\"%s\" style=\"flex-grow:0;margin:1px;display:flex\"><img src=\"%s\" style=\"height:160px;object-fit:cover\"/></a>"
                    url url)))
        files "")
       "</div>"))))

(provide 'obp-shortcodes)

;;; obp-shortcodes.el ends here