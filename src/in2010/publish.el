;;; publish.el --- Org → HTML publishing for src/in2010  -*- lexical-binding: t; -*-

(require 'ox-html)
(require 'seq)

;; Prefer a vendored htmlize if present; otherwise, load gracefully.
(let* ((in2010-root (file-name-directory (or load-file-name buffer-file-name)))
       (vendor-dir (expand-file-name "vendor" in2010-root)))
  (when (file-directory-p vendor-dir)
    (add-to-list 'load-path vendor-dir)))
(condition-case _
    (require 'htmlize)
  (error
   ;; Disable htmlize usage when not available
   (setq org-html-htmlize-output-type nil)))

(defun in2010/publish-force ()
  "Force rebuild of the in2010-org project."
  (interactive)
  (org-publish-project "in2010-org" 'force))

;; -------- Footer helpers (Git date and conditional injection) --------

(defun in2010/git-last-commit-date (file)
  "Return last commit date (YYYY-MM-DD) for FILE, or nil if not available."
  (let* ((git-root (locate-dominating-file file ".git"))
         (default-directory git-root)
         (buf (generate-new-buffer " *in2010-git-date*"))
         (status (and git-root
                      (with-current-buffer buf
                        (call-process "git" nil t nil
                                      "log" "-1" "--format=%cs" "--" file)))))
    (prog1
        (when (and git-root (eq status 0))
          (string-trim (with-current-buffer buf (buffer-string))))
      (kill-buffer buf))))

(defun in2010/html-footer-filter (output backend info)
  "Append a 'Last updated' footer to HTML OUTPUT except for the sitemap index.
Insert the footer before </body> to ensure it renders."
  (if (not (org-export-derived-backend-p backend 'html))
      output
    (let* ((file (plist-get info :input-file))
           (basename (and file (downcase (file-name-nondirectory file)))))
      (if (and basename (string-equal basename "index.org"))
          output
        (let* ((git-date (and file (in2010/git-last-commit-date file)))
               (mtime (and file (file-attribute-modification-time (file-attributes file))))
               (date (or git-date (and mtime (format-time-string "%Y-%m-%d" mtime)) "—"))
               (footer (format "<footer class=\"page-footer\">Last updated: %s</footer>" date))
               ;; try to inject before </body> (case-insensitive)
               (case-fold-search t))
          (if (string-match "</body>" output)
              (replace-match (concat footer "\n</body>") t t output)
            ;; fallback: append (shouldn’t happen in normal HTML)
            (concat output "\n" footer)))))))


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
         ;; Disable the default postamble; we inject our footer via filter
         :html-postamble nil
         ;; Add our conditional footer filter
         :filters-alist ((:filter-final-output . in2010/html-footer-filter))
         ;; Minimal overview (sitemap)
         :auto-sitemap t
         :sitemap-filename "index.org"
         :sitemap-title "Notes"
         :sitemap-style list
         :sitemap-sort-files alphabetically
         :sitemap-format-function in2010/sitemap-minimal)
        ("site" :components ("in2010-org"))))

(provide 'publish)
;;; publish.el ends here
