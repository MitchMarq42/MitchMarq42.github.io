#!/bin/sh
":"; exec emacs -Q --script "$0" "$@" # -*- mode: emacs-lisp; -*-
(require 'ox-publish)

;; Set the package installation directory so that packages aren't stored in the
;; ~/.emacs.d/elpa path.
(require 'package)
(setq package-user-dir (expand-file-name "./.packages"))
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Initialize the package system
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install dependencies
(package-install 'htmlize)

(setq org-publish-project-alist
      (list
       (list "my-org-site"
             :recursive t
             :base-directory "./org"
             :publishing-directory "./html"
             :publishing-function 'org-html-publish-to-html
	     :section-numbers nil
	     :with-creator t
	     :with-author nil
	     :with-toc nil
	     )))

(setq org-html-validation-link nil            ;; Don't show validation link
      org-html-head-include-scripts nil       ;; Use our own scripts
      org-html-head-include-default-style t ;nil ;; Use our own styles
      org-html-head "<link rel=\"stylesheet\" href=\"https://mitchmarq42.xyz/style.css\" />"
      org-html-postamble nil
      )

;; Generate the site output
(org-publish-all t)

(message "Build Complete!")