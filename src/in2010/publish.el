;;; publish.el --- Org → HTML publishing (no htmlize) -*- lexical-binding: t; -*-
;; This file is GPT generated as a test of how the current models work today

(require 'ox-html)
(require 'seq)
(require 'org)

;;; Paths relative to this publish.el
(defconst in2010-root
  (file-name-directory (or load-file-name buffer-file-name)))

(defconst in2010-org-dir
  (expand-file-name "org" in2010-root))

;; Publish in2010 under a subdirectory to avoid overwriting the site root
(defconst in2010-out-dir
  (expand-file-name (concat "public" "/" "in2010") in2010-root))

(defconst in2010-assets-dir
  (expand-file-name "assets" in2010-root))

;;; Disable htmlize entirely (no syntax highlighting)
(setq org-html-htmlize-output-type nil)
(setq org-html-htmlize-font-prefix "")

;;; HTML export cleanliness and HTML5
(setq org-html-head-include-default-style nil)
(setq org-html-head-include-scripts nil)
(setq org-html-validation-link nil)
(setq org-html-doctype "html5")
(setq org-html-html5-fancy t)

;;; Preserve whitespace in code blocks (optional, useful for plain text)
(setq org-src-preserve-indentation t)
;; (setq org-export-preserve-breaks t) ;; enable if you want soft line breaks in paragraphs

;;; Use build directory for timestamps
(setq org-publish-timestamp-directory
      (expand-file-name ".org-timestamps" in2010-root))

;;; Minimal sitemap (filters out index.org)
(defun in2010/sitemap-minimal (_title paths)
  (let ((filtered
         (seq-filter
          (lambda (entry)
            (not (string-equal
                  (downcase (file-name-nondirectory (car entry)))
                  "index.org")))
          paths)))
    (concat
     "#+TITLE: Notes\n#+OPTIONS: toc:nil\n\n"
     "#+begin_export html\n<ul>\n#+end_export\n\n"
     (mapconcat
      (lambda (entry)
        (let* ((file (car entry))
               (title (org-publish-find-title file))
               (html (concat
                      (file-name-sans-extension
                       (file-relative-name file in2010-org-dir))
                      ".html")))
          (format "#+begin_export html\n<li><a href=\"%s\">%s</a></li>\n#+end_export"
                  html title)))
      filtered
      "\n")
     "\n#+begin_export html\n</ul>\n#+end_export\n")))

;;; Project definition
(setq org-publish-project-alist
      `(
        ;; Org pages → HTML under public/in2010
        ("in2010-org"
         :base-directory ,in2010-org-dir
         :base-extension "org"
         :recursive t
         :publishing-directory ,in2010-out-dir
         :publishing-function org-html-publish-to-html
         :with-author t
         :with-toc t
         :section-numbers t
         :html-postamble nil
         ;; Use absolute path so pages under /in2010 can find the CSS
         :html-head "<link rel=\"stylesheet\" href=\"/assets/style.css\" />"
         :auto-sitemap t
         :sitemap-filename "index.org"
         :sitemap-title "Notes"
         :sitemap-style list
         :sitemap-sort-files alphabetically
         :sitemap-format-function in2010/sitemap-minimal)

        ;; Static assets → copied to public/assets (shared for whole site)
        ("in2010-assets"
         :base-directory ,in2010-assets-dir
         :base-extension "css\\|js\\|png\\|jpg\\|jpeg\\|gif\\|svg\\|webp\\|ico\\|ttf\\|otf\\|woff\\|woff2"
         :recursive t
         :publishing-directory ,(expand-file-name "public/assets" in2010-root)
         :publishing-function org-publish-attachment)

        ;; Aggregate site
        ("site"
         :components ("in2010-org" "in2010-assets"))))

(provide 'publish)
;;; publish.el ends here
