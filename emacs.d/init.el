;;; init --- initialise emacs config
;;; Commentary:

;; This file allows Emacs to initialize my customizations
;; in Emacs lisp embedded in *one* literate Org-mode file.

;;; Code:
(when (version< emacs-version "28")
  (warn "This configuration needs Emacs trunk, but this is %s!" emacs-version))

(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
	        use-package-expand-minimally t))

;; This sets up the load path so that we can override it
;; (package-initialize nil)

;; Override the packages with the git version of Org and other packages
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/org-mode/lisp/")

;; Load the rest of the packages
;; (package-initialize t)
;; (setq package-enable-at-startup nil)

(require 'org)
(org-babel-load-file "~/.emacs.d/gmwils.org")

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;;; init.el ends here
