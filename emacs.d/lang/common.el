;;;; common.el - things used by all languages

;; Ensure we're using UTF8 as a default
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(ansi-color-for-comint-mode-on)

;; Transparently open compressed files
(auto-compression-mode t)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; Flymake setup
(require 'flymake)
(add-hook 'find-file-hook 'flymake-find-file-hook) ;; Auto enable flymake
(setq flymake-run-in-place nil)
(setq flymake-start-syntax-check-on-newline nil)
; on osx, the flymake gui warning dialog sometimes freezes emacs
(setq flymake-gui-warnings-enabled nil)
; bind some useful keys
(global-set-key [f6] 'flymake-display-err-menu-for-current-line)
(global-set-key "\C-ce" 'flymake-display-err-menu-for-current-line)
(global-set-key "\C-cn" 'flymake-goto-next-error)
(global-set-key "\C-cp" 'flymake-goto-prev-error)
(custom-set-faces
 '(flymake-errline ((((class color)) (:underline "red"))))
 '(flymake-warnline ((((class color)) (:underline "yellow")))))

(defun show-fly-err-at-point ()
  "If the cursor is sitting on a flymake error, display the
  message in the minibuffer"
  (interactive)
  (let ((line-no (line-number-at-pos)))
    (dolist (elem flymake-err-info)
      (if (eq (car elem) line-no)
	  (let ((err (car (second elem))))
	    (message "%s" (flymake-ler-text err)))))))

(defadvice flymake-goto-next-error (after display-message activate compile)
  "Display the error in the mini-buffer rather than having to mouse over it"
  (show-fly-err-at-point))

(defadvice flymake-goto-prev-error (after display-message activate compile)
  "Display the error in the mini-buffer rather than having to mouse over it"
  (show-fly-err-at-point))

;; ido-mode is like magic pixie dust!
(when (> emacs-major-version 21)
  (ido-mode t)
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-max-prospects 10))

(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)

(defalias 'yes-or-no-p 'y-or-n-p)
(random t) ;; Seed the random-number generator

;; Don't clutter up directories with files~
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat dotfiles-dir "backups")))))
;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; NodeJS settings
(require 'sws-mode)
(require 'jade-mode)

;; Associate modes with file extensions

(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.js\\(on\\)?$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))


(eval-after-load 'grep
  '(when (boundp 'grep-find-ignored-files)
    (add-to-list 'grep-find-ignored-files "target")
    (add-to-list 'grep-find-ignored-files "*.class")))

;; Default to unified diffs
(setq diff-switches "-u -w")

;; make emacs use the clipboard
(setq x-select-enable-clipboard t)

(provide 'common)
