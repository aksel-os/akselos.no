;;; publish.el --- Org → HTML publishing for src/in2010  -*- lexical-binding: t; -*-

(require 'ox-html)
(require 'seq)

;; Optional: prefer vendored htmlize if present; load gracefully.
(let* ((in2010-root (file-name-directory (or load-file-name buffer-file-name)))
       (vendor-dir (expand-file-name "vendor" in2010-root)))
  (when (file-directory-p vendor-dir)
    (add-to-list 'load-path vendor-dir)))
(condition-case _
    (require 'htmlize)
  (error
   ;; Disableize usage when not available
   (setq org-html-htmlize-output-type nil)))

;; Use a project-local timestamp/cache directory to avoid HOME issues in Nix
(let* ((in2010-root (file-name-directory (or load-file-name buffer-file-name)))
       (cache-dir  (expand-file-name ".org-timestamps" in2010-root)))
  (setq org-publish-timestamp-directory cache-dir)
  (unless (file-directory-p cache-dir)
    (make-directory cache-dir t)))

;; If you prefer fully cacheless builds (always rebuild), uncomment:
;; (setq org-publish-use-timestamps-flag nil)

(defun in2010/publish-force ()
  "Force rebuild of the in2010-org project."
  (interactive)
  (org-publish-project "in2010-org" 'force))

;; -------- Minimal, pretty sitemap generator --------

(defun in2010/sitemap-minimal (title list)
  "Generate a minimal index listing note links; exclude index.org and assets/."
  (let* ((proj-pair (assoc "in2010-org" org-publish-project-alist))
         (proj (cdr proj-pair))
         (base (plist-get proj :base-directory))
         (items
          (seq-filter
           (lambda (entry)
             (let* ((file (car entry))
                    (rel (file-relative-name file base)))
               (and (string-match-p "\\.org\\'" rel)
                    (not (string-equal (downcase rel) "index.org"))
                    (not (string-prefix-p "assets/" rel)))))
           list)))
    (concat
     (format "#+TITLE: %s\n#+OPTIONS: toc:nil\n\n" title)
     "#+begin_export html\n<div class=\"notes-container\">\n<ul class=\"note-list\">\n#+end_export\n\n"
     (mapconcat
      (lambda (entry)
        (let* ((file (car entry))
               (rel (file-relative-name file base))
               (href (concat (file-name-sans-extension rel) ".html"))
               (ntitle (org-publish-find-title file proj)))
          (format "#+begin_export html\n<li><a href=\"%s\">%s</a></li>\n#+end_export\n"
                  href (or ntitle rel))))
      items
      "\n")
     "\n#+begin_export html\n</ul>\n</div>\n#+end_export\n")))

;; -------- Project configuration: src/in2010/org -> src/in2010/docs --------

(setq org-publish-project-alist
      '(("in2010-org"
         :base-directory "org"
         :base-extension "org"
         :recursive t
         :publishing-directory "docs"
         :publishing-function org-html-publish-to-html
         :with-author t
         :with-toc t
         :section-numbers t
         ;; Absolute path from site root (root = src/)
         :html-head "<link rel=\"stylesheet\" href=\"/in2010/assets/style.css\" />"
         :html-postamble nil
         :auto-sitemap t
         :sitemap-filename "index.org"
         :sitemap-title "Notes"
         :sitemap-style list
         :sitemap-sort-files alphabetically
         :sitemap-format-function in2010/sitemap-minimal)
        ("site" :components ("in2010-org"))))

(provide 'publish)
;;; publish.el ends here
