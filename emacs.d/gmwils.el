(require 'textmate)
(textmate-mode)

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

(defun writeroom ()
  "Switches to a WriteRoom-like fullscreen style"
  (interactive)	
  (when (featurep 'aquamacs)
    ;; switch to white on black
    (color-theme-initialize)
    (color-theme-clarity)
    ;; switch to Garamond 36pt
    (aquamacs-autoface-mode 0)
    (set-frame-font "-apple-garamond-medium-r-normal--36-360-72-72-m-360-iso10646-1")
    ;; switch to fullscreen mode
    (aquamacs-toggle-full-frame)))

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

(load-file "~/.emacs.d/custom-keys.el")