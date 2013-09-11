;; show trailing whitespace
(setq-default show-trailing-whitespace t)
(global-set-key "\C-c\C-w" 'whitespace-cleanup)
(global-smart-tab-mode 1)

;; Setup Yasnippet - http://github.com/capitaomorte/yasnippet
(yas-global-mode 1)

(require 'ess)

;; Setup the scratch file
(setq initial-scratch-message "")
(setq aquamacs-scratch-file nil
      initial-major-mode 'lisp-interaction-mode)

; don't open new frames when opening files in aquamacs
(setq one-buffer-one-frame-mode nil)

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

(defun conditionally-enable-paredit-mode ()
  "Enable `paredit-mode' in the minibuffer, during `eval-expression'."
  (if (eq this-command 'eval-expression)
      (paredit-mode 1)))

(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)

(setq debug-on-error t)

(require 'yaml-mode)

(require 'textmate)
(textmate-mode)

;; Hive / Hadoop
(add-to-list 'auto-mode-alist '("\\.hql$" . sql-mode))

;; Haskell
;; (autoload 'ghc-init "ghc" nil t)

;; Setup Flycheck - https://github.com/flycheck/flycheck
(condition-case nil
    (progn
      (require 'flycheck)
      (add-hook 'after-init-hook #'global-flycheck-mode))
     (file-error (message "Flycheck not available; not configuring")))

(require 'protobuf-mode)
(require 'thrift-mode)
(require 'puppet-mode)
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

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
;; (require 'color-theme-railscasts)
(require 'color-theme-ir-black)

;; Always fill at 78 chars
(add-hook 'text-mode-hook #'turn-on-auto-fill)
(setq-default fill-column 78)

;; Deft setup - http://jblevins.org/projects/deft/
(when (require 'deft nil 'noerror)
  (setq
   deft-extension "org"
   deft-directory "~/Dropbox/Notes/"
   deft-text-mode 'org-mode))

;; Setup org-babel
;; http://orgmode.org/org.html#Languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (R . t)
   (ruby . t)
   (sh . t)
   (python . t)
   (js . t)))

;; dash-at-point
;; https://github.com/stanaka/dash-at-point#readme
(autoload 'dash-at-point "dash-at-point"
  "Search the word at point with Dash." t nil)
(global-set-key "\C-cd" 'dash-at-point)

;; full screen magit-status
;; http://whattheemacsd.com/setup-magit.el-01.html
(when (require 'magit nil 'noerror)
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen))

  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session))

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; Setup an edit server
;; Used by Chrome plugin: http://www.emacswiki.org/emacs/Google_Chrome
(when (and (window-system) (require 'edit-server nil t))
  (setq edit-server-new-frame nil)
  (edit-server-start)
  (setq edit-server-url-major-mode-alist
        '(("github\\.com" . markdown-mode)
          ("mail\\.google\\.com" . html-mode))))

;; http://emacsredux.com/blog/2013/03/27/copy-filename-to-the-clipboard/
(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;; Auto Save files to temp dir
;; http://emacswiki.org/emacs/AutoSave
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;; http://emacswiki.org/emacs/UnfillParagraph
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))
;; Handy key definition
(define-key global-map "\M-Q" 'unfill-paragraph)

;; Allow for easy use of new lines
(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)

;; Shuffle lines
(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (move-to-column col)))

(global-set-key (kbd "<C-S-down>") 'move-line-down)
(global-set-key (kbd "<C-S-up>") 'move-line-up)

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
