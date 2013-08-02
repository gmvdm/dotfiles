(require 'cl)

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-startup-message t)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "open"
      marmalade-server "http://marmalade-repo.org/"
      custom-file (expand-file-name "~/.emacs.d/custom.el")
      ido-handle-duplicate-virtual-buffers 2)

;; Setup packages
(when (< emacs-major-version 24)
  (load-file (expand-file-name "~/.emacs.d/package-23.el")))

(require 'package)
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(clojure-mode clojure-test-mode
                      markdown-mode yaml-mode tuareg
		      textmate color-theme magit deft
		      paredit python-mode
		      haskell-mode go-mode
                      js2-mode
                      color-theme-railscasts
                      color-theme-ir-black
                      yasnippet
                      ess
		      ;; ruby-mode inf-ruby
		      scala-mode php-mode
                      workgroups
                      smart-tab protobuf-mode
                      marmalade oddmuse scpaste))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; (setq temporary-file-directory "~/.emacs.d/tmp/")

;; You can keep system- or user-specific customizations here
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(setq system-specific-config (concat dotfiles-dir system-name ".el")
      user-specific-config (concat dotfiles-dir user-login-name ".el")
      user-specific-dir (concat dotfiles-dir user-login-name)
      lang-specific-dir (concat dotfiles-dir "lang")
      common-dir (concat dotfiles-dir "common"))
(add-to-list 'load-path user-specific-dir)
(add-to-list 'load-path lang-specific-dir)
(add-to-list 'load-path common-dir)

(require 'flymake)

(if (file-exists-p system-specific-config) (load system-specific-config))

(if (file-exists-p common-dir)
    (mapc #'load (directory-files common-dir nil ".*el$")))

(if (file-exists-p lang-specific-dir)
    (mapc #'load (directory-files lang-specific-dir nil ".*el$")))

(if (file-exists-p user-specific-dir)
    (mapc #'load (directory-files user-specific-dir nil ".*el$")))

(if (file-exists-p user-specific-config) (load user-specific-config))
