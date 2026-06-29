;;; obp-config.el --- org-bootstrap-publish configuration  -*- lexical-binding: t; -*-

(add-to-list 'load-path
             (expand-file-name "offline-packages/local-packages/org-bootstrap-publish"
                               user-emacs-directory))
(require 'org-bootstrap-publish)

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

(load "obp-shortcodes" t t)

(setq org-bootstrap-publish-sites
      '((dyerdwelling
         (org-bootstrap-publish-source-files
          . ("~/DCIM/content/blog.org"
             "~/DCIM/content/art.org"
             "~/DCIM/content/linux.org"
             "~/DCIM/content/kate.org"
             "~/DCIM/content/dad-dictionary.org"
             "~/DCIM/content/emacs.org"))
         (org-bootstrap-publish-noindex t)
         (org-bootstrap-publish-static-exclude-regex . "evie")
         (org-bootstrap-publish-output-dir   . "~/publish/obp-output")
         (org-bootstrap-publish-layout       . topbar)
         (org-bootstrap-publish-site-title   . "the DyerDwelling!")
         (org-bootstrap-publish-site-tagline . "by James Dyer")
         (org-bootstrap-publish-site-url     . "https://www.dyerdwelling.family/")
         (org-bootstrap-publish-author       . "James Dyer")
         (org-bootstrap-publish-cloudflare-project . "dyerdwelling")
         (org-bootstrap-publish-preview-limit . 10)
         (org-bootstrap-publish-serve-browser . "firefox")
         (org-bootstrap-publish-async-init-files . ("~/.emacs.d/obp-shortcodes.el"))
         (org-bootstrap-publish-static-dirs
          . ("static/art--gallery" "static/art--other" "static/art--videos"
             "static/assets" "static/bingo" "static/blog" "static/bookimages"
             "static/bookthumbs" "static/dad--dictionary" "static/emacs"
             "static/images" "static/kate" "static/linux" "static/music"
             "static/photos" "static/scans" "static/ox-hugo"))
         (org-bootstrap-publish-shortcodes
          . ((horseprediction . my/obp-shortcode-horseprediction)
             (crossword       . my/obp-shortcode-crossword)
             (foldergallery   . my/obp-shortcode-foldergallery)))
         (org-bootstrap-publish-background-image
          . "/static/images/banner/pagefront.jpg")
         (org-bootstrap-publish-background-blur . 8)
         (org-bootstrap-publish-background-opacity . 0.6)
         (org-bootstrap-publish-theme-overrides
          . (("obp-sidebar-bg"    . "#401f33")
             ("obp-sidebar-fg"    . "#e6ecf5")
             ("obp-sidebar-muted" . "#8b9bb4")
             ("obp-accent"        . "#ffb779")
             ("obp-accent-rgb"    . "255, 138, 76")))
         (org-bootstrap-publish-menu-links
          . (
;             ("Evie"       . "/tags/evie/")
             ("gallery"    . "/art--gallery/")
             ("art"        . "/art--all/")
             ("videos"     . "/tags/videos/")
             ("photos"     . "/photos/")
             ("blog"       . "/blog/")
             ("scans"      . "/scans/")
             ("linux"      . "/linux/")
             ("emacs"      . "/tags/emacs/")
             ("stables"    . "/blog/posts--the-stables/")
             ("crosswords" . "/blog/posts--crosswords/")
             ("music"      . "/blog/posts--full-music-list/")
             ("katieboo85" . "/kate/")
             ("about"      . "/blog/posts--about-me/"))))
        (art
         (org-bootstrap-publish-source-files . ("~/DCIM/content/art.org"))
         (org-bootstrap-publish-output-dir   . "~/publish/obp-output")
         (org-bootstrap-publish-layout       . topbar)
         (org-bootstrap-publish-site-title   . "DyerDwelling/art")
         (org-bootstrap-publish-site-tagline . "Welcome to the home of James Dyer wearied traveller")
         (org-bootstrap-publish-site-url     . "https://www.art.dyerdwelling.family/")
         (org-bootstrap-publish-author       . "James Dyer")
         (org-bootstrap-publish-cloudflare-project . "art-dyerdwelling")
         (org-bootstrap-publish-preview-limit . 20)
         (org-bootstrap-publish-serve-browser . "firefox")
         (org-bootstrap-publish-static-dirs
          . ("static/art--gallery" "static/art--videos" "static/art--other"
             "static/images/banner"))
         (org-bootstrap-publish-background-image
          . "/static/images/banner/pagefront.jpg")
         (org-bootstrap-publish-background-blur . 8)
         (org-bootstrap-publish-background-opacity . 1.0)
         (org-bootstrap-publish-theme-overrides
          . (("obp-sidebar-bg"    . "#2a1830")
             ("obp-sidebar-fg"    . "#f1e4ec")
             ("obp-sidebar-muted" . "#a48aa0")
             ("obp-accent"        . "#e7b04b")
             ("obp-accent-rgb"    . "231, 176, 75")))
         (org-bootstrap-publish-menu-links
          . (("gallery" . "/tags/gallery/")
             ("videos"  . "/tags/videos/")
             ("doodles" . "/tags/doodle/")
             ("artrage" . "/tags/artrage/")
             ("2024"    . "/tags/2024/")
             ("2023"    . "/tags/2023/")
             ("2022"    . "/tags/2022/"))))
        (katieboo85
         (org-bootstrap-publish-noindex t)
         (org-bootstrap-publish-source-files . ("~/DCIM/content/kate.org"))
         (org-bootstrap-publish-output-dir   . "~/publish/obp-output")
         (org-bootstrap-publish-layout       . topbar)
         (org-bootstrap-publish-site-title   . "KatieBoo85")
         (org-bootstrap-publish-site-tagline . "Kate's Recipe Collection")
         (org-bootstrap-publish-site-url     . "https://katieboo85.pages.dev/")
         (org-bootstrap-publish-author       . "Katherine Jeffs")
         (org-bootstrap-publish-cloudflare-project . "katieboo85")
         (org-bootstrap-publish-preview-limit . 20)
         (org-bootstrap-publish-serve-browser . "firefox")
         (org-bootstrap-publish-static-dirs  . ("static/kate" "static/assets" "static/images"))
         (org-bootstrap-publish-background-image
          . "/static/images/banner/navbar-katieboo85.jpg")
         (org-bootstrap-publish-background-blur . 8)
         (org-bootstrap-publish-background-opacity . 1.0)
         (org-bootstrap-publish-theme-overrides
          . (("obp-sidebar-bg"    . "#5a1e1e")
             ("obp-sidebar-fg"    . "#fbe9d7")
             ("obp-sidebar-muted" . "#caa18a")
             ("obp-accent"        . "#f3a840")
             ("obp-accent-rgb"    . "243, 168, 64")))
         (org-bootstrap-publish-menu-links   . (("recipes" . "/kate/"))))
        (emacs
         (org-bootstrap-publish-source-files . ("~/DCIM/content/emacs.org"))
         (org-bootstrap-publish-output-dir   . "~/publish/obp-output")
         (org-bootstrap-publish-layout       . topbar)
         (org-bootstrap-publish-site-title   . "Emacs Dwelling")
         (org-bootstrap-publish-site-tagline . "Journeying through the Emacs rabbit hole")
         (org-bootstrap-publish-site-url     . "https://emacs.dyerdwelling.family/")
         (org-bootstrap-publish-author       . "James Dyer")
         (org-bootstrap-publish-static-dirs  . ("static/emacs" "static/images/banner"))
         (org-bootstrap-publish-disqus-shortname . "https-www-emacs-dyerdwelling-family")
         (org-bootstrap-publish-cloudflare-project . "emacs-dyerdwelling")
         (org-bootstrap-publish-preview-limit . 10)
         (org-bootstrap-publish-serve-browser . "firefox")
         (org-bootstrap-publish-background-image
          . "/static/images/banner/emacs-ollama-buddy.jpg")
         (org-bootstrap-publish-background-blur . 4)
         (org-bootstrap-publish-background-opacity . 0.8)
         (org-bootstrap-publish-theme-overrides
          . (("obp-sidebar-bg"    . "#564160")
             ("obp-sidebar-fg"    . "#dcdcf2")
             ("obp-sidebar-muted" . "#8a8ab5")
             ("obp-accent"        . "#9b6ed1")
             ("obp-accent-rgb"    . "155, 110, 209")))
         (org-bootstrap-publish-menu-links
          . (("2026"         . "/tags/2026/")
             ("org"          . "/tags/org/")
             ("ollama-buddy" . "/tags/ollama-buddy/")
             ("dired"        . "/tags/dired/"))))))

(provide 'obp-config)
;;; obp-config.el ends here
