;;; package -- Summary

;;; Commentary:

;;; Code:

;;; Emacs settings:

(setq debug-on-error t)

;; Setup the scratch file
;; See: http://emacsredux.com/blog/2014/07/25/configure-the-scratch-buffers-mode/
(setq initial-scratch-message "")
(setq initial-major-mode 'lisp-interaction-mode)

;; Always fill at 78 chars
(add-hook 'text-mode-hook #'turn-on-auto-fill)
(setq-default fill-column 78)

;; show trailing whitespace
(setq-default show-trailing-whitespace t)
(global-set-key "\C-c\C-w" 'whitespace-cleanup)

; Set default file encoding to utf-8 (http://nakkaya.com/2009/11/29/emacs-and-international-characters/)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Auto Save files to temp dir
;; http://emacswiki.org/emacs/AutoSave
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Tramp mode - http://www.gnu.org/software/tramp/#Connection-types
(setq tramp-default-method "scpx")


;;; Other packages:

(require 'smart-tab)
(global-smart-tab-mode 1)

;; Setup Yasnippet - http://github.com/capitaomorte/yasnippet
(yas-global-mode 1)

;; Setup ESS
(require 'ess-site)
(setq ess-ask-for-ess-directory nil)

;; Doc-mode
(add-to-list 'auto-mode-alist '("\\.doc$" . doc-mode))
(add-to-list 'auto-mode-alist '("\\.asciidoc$" . doc-mode))

(require 'dired-x)

(defun conditionally-enable-paredit-mode ()
  "Enable `paredit-mode' in the minibuffer, during `eval-expression'."
  (if (eq this-command 'eval-expression)
      (paredit-mode 1)))
(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)

(require 'textmate)
(textmate-mode)

;; Projectile mode: https://github.com/bbatsov/projectile
(projectile-global-mode)

;; Hive / Hadoop
(add-to-list 'auto-mode-alist '("\\.hql$" . sql-mode))

;; Setup Flycheck - https://github.com/flycheck/flycheck
(condition-case nil
    (progn
      (require 'flycheck)
      (add-hook 'after-init-hook #'global-flycheck-mode))
     (file-error (message "Flycheck not available; not configuring")))

(require 'protobuf-mode)
(require 'puppet-mode)
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

;; Appearance
(require 'color-theme)
(require 'color-theme-ir-black)


;; Deft setup - http://jblevins.org/projects/deft/
(when (require 'deft nil 'noerror)
  (setq
   deft-extension "org"
   deft-directory "~/Notes/"
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

(provide 'gmwils)
;;; gmwils.el ends here
