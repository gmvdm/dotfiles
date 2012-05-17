;; show trailing whitespace
(setq-default show-trailing-whitespace t)
(global-set-key "\C-c\C-w" 'whitespace-cleanup)
(global-smart-tab-mode 1)

;; Tramp mode - http://www.gnu.org/software/tramp/#Connection-types
(setq tramp-default-method "scpx")

(add-to-list 'auto-mode-alist '("\\.doc$" . doc-mode))
(add-to-list 'auto-mode-alist '("\\.asciidoc$" . doc-mode))

;; Setup for Workgroups (https://github.com/tlh/workgroups.el)
(require 'workgroups)
(setq wg-prefix-key (kbd "C-c w"))
(wg-load "~/.emacs.d/workgroups")

;; Advanced dired
(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")
            ;; Set dired-x global variables here.  For example:
            ;; (setq dired-guess-shell-gnutar "gtar")
            ;; (setq dired-x-hands-off-my-keys nil)
            ))
(add-hook 'dired-mode-hook
          (lambda ()
            ;; Set dired-x buffer-local variables here.  For example:
            ;; (dired-omit-mode 1)
            ))

(setq debug-on-error t)

(require 'yaml-mode)

(require 'textmate)
(textmate-mode)

;; Haskell
;; (autoload 'ghc-init "ghc" nil t)
;; (add-hook 'haskell-mode-hook (lambda () (ghc-init) (flymake-mode)))

(require 'protobuf-mode)
(require 'thrift-mode)
(require 'puppet-mode)
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

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

;; Deft setup - http://jblevins.org/projects/deft/
(when (require 'deft nil 'noerror)
  (setq
   deft-extension "org"
   deft-directory "~/Dropbox/Notes/"
   deft-text-mode 'org-mode))

;; Smarter buffer names
;; (require 'uniquify)
;; (setq uniquify-buffer-name-style (quote post-forward))

; Set default file encoding to utf-8 (http://nakkaya.com/2009/11/29/emacs-and-international-characters/)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(load-file "~/.emacs.d/custom-keys.el")