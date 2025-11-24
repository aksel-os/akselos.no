;;; publish.el --- Org → HTML publishing for src/in2010  -*- lexical-binding: t; -*-

;; Ensure packages are initialized in batch sessions
(require 'package)
(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

;; Try to load htmlize; install if missing. Fall back gracefully if install fails.
(defvar in2010-htmlize-available nil)
(setq in2010-htmlize-available
      (condition-case _
          (progn
            (require 'htmlize) t)
        (error
         (condition-case _
             (progn
               (unless package-archive-contents
                 (package-refresh-contents))
               (package-install 'htmlize)
               (require 'htmlize)
               t)
           (error
            (message "htmlize not available; code highlighting will be disabled")
            nil)))))

(require 'ox-html)

;; If htmlize is not available, disable syntax highlighting to avoid errors
(unless in2010-htmlize-available
  (setq org-export-use-babel nil)
  (setq org-html-htmlize-output-type nil))

;; Project: src/in2010/org -> src/in2010/docs
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
         ;; Site root is src/, so absolute path starts with /in2010/...
         :html-head "<link rel=\"stylesheet\" href=\"/in2010/assets/style.css\" />"
         :html-postamble nil)
        ("site" :components ("in2010-org"))))

(provide 'publish)
;;; publish.el ends here
