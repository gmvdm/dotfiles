(require 'cl)

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-startup-message t)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "safari"
      marmalade-server "http://marmalade-repo.org/"
      custom-file (expand-file-name "~/.emacs.d/custom.el")
      ido-handle-duplicate-virtual-buffers 2)

;; Setup packages
;; TODO - don't load in emacs 24 from file
(load-file (expand-file-name "~/.emacs.d/package-23.el"))
(require 'package)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(clojure-mode clojure-test-mode
                      markdown-mode yaml-mode tuareg
		      textmate color-theme magit
                      marmalade oddmuse scpaste))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; You can keep system- or user-specific customizations here
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(setq system-specific-config (concat dotfiles-dir system-name ".el")
      user-specific-config (concat dotfiles-dir user-login-name ".el")
      user-specific-dir (concat dotfiles-dir user-login-name))
(add-to-list 'load-path user-specific-dir)

(if (file-exists-p system-specific-config) (load system-specific-config))
(if (file-exists-p user-specific-config) (load user-specific-config))
(if (file-exists-p user-specific-dir)
  (mapc #'load (directory-files user-specific-dir nil ".*el$")))