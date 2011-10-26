(require 'textmate)
(textmate-mode)

(require 'beamer-templates)

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

; Set default file encoding to utf-8 (http://nakkaya.com/2009/11/29/emacs-and-international-characters/)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(load-file "~/.emacs.d/custom-keys.el")