(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "conkeror"
      marmalade-server "http://marmalade-repo.org/"
      custom-file (expand-file-name "~/.emacs.d/custom.el")
      ispell-extra-args '("--keyboard=dvorak")
      ido-handle-duplicate-virtual-buffers 2)


(load-file (expand-file-name "~/.emacs.d/package-23.el"))
(require 'package)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(clojure-mode clojure-test-mode
                      markdown-mode yaml-mode tuareg
                      marmalade oddmuse scpaste))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
