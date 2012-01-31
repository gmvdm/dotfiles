(require 'textmate)
(textmate-mode)

(require 'clojurescript-mode)
(setq inferior-lisp-program "browser-repl")

(require 'protobuf-mode)
(require 'thrift-mode)
(require 'puppet-mode)
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

;; show trailing whitespace
(setq-default show-trailing-whitespace t)

(global-set-key "\C-c\C-w" 'whitespace-cleanup)

(global-smart-tab-mode 1)

(require 'beamer-templates)

(add-to-list 'auto-mode-alist '("\\.doc$" . doc-mode))
(add-to-list 'auto-mode-alist '("\\.asciidoc$" . doc-mode))


;; TODO move to relevant location
(defun browse-url-default-macosx-browser (url &optional new-window)
  (interactive (browse-url-interactive-arg "URL: "))
  (if (and new-window (>= emacs-major-version 23))
      (ns-do-applescript
       (format (concat "tell application \"Safari\" to make document with properties {URL:\"%s\"}\n"
		       "tell application \"Safari\" to activate") url))
    (start-process (concat "open " url) nil "open" url)))

;; Appearance
(if (featurep 'aquamacs)
    (set-face-font 'default "-apple-PanicSans-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1"))

(require 'color-theme)
(load-file "~/.emacs.d/gmwils/color-theme-railscasts.el")
(color-theme-railscasts)

;; Deft setup - http://jblevins.org/projects/deft/
(when (require 'deft nil 'noerror)
  (setq
   deft-extension "org"
   deft-directory "~/Dropbox/Notes/"
   deft-text-mode 'org-mode))

; Set default file encoding to utf-8 (http://nakkaya.com/2009/11/29/emacs-and-international-characters/)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Flymake setup
(require 'flymake)
(add-hook 'find-file-hook 'flymake-find-file-hook) ;; Auto enable flymake
(setq flymake-run-in-place nil)
(setq flymake-start-syntax-check-on-newline nil)
(custom-set-faces
 '(flymake-errline ((((class color)) (:underline "red"))))
 '(flymake-warnline ((((class color)) (:underline "yellow")))))

(load-file "~/.emacs.d/custom-keys.el")