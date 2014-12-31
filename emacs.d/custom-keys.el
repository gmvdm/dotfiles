;; Keyboard bindings

;;;###autoload
(progn
  ;; Font size
  (define-key global-map (kbd "C-+") 'text-scale-increase)
  (define-key global-map (kbd "C--") 'text-scale-decrease)

  ;; Use regex searches by default.
  (global-set-key (kbd "C-s") 'isearch-forward-regexp)
  (global-set-key (kbd "\C-r") 'isearch-backward-regexp)
  (global-set-key (kbd "C-M-s") 'isearch-forward)
  (global-set-key (kbd "C-M-r") 'isearch-backward)

  ;; M-S-6 is awkward
  (global-set-key (kbd "C-c q") 'join-line)
  (global-set-key (kbd "M-j")
            (lambda ()
                  (interactive)
                  (join-line -1)))


  ;; Cycle between windows - https://github.com/garybernhardt/dotfiles/blob/master/.emacs
  (global-set-key (kbd "C-o") 'other-window)
  (defun prev-window ()
    (interactive)
    (other-window -1))
  (global-set-key "\M-o" 'prev-window)

  ;; Help should search more than just commands
  (global-set-key (kbd "C-h a") 'apropos)

  (global-set-key (kbd "C-x m") 'magit-status)
  (global-set-key (kbd "M-s")   'fixup-whitespace)

  ;; Toggle auto-fill
  (global-set-key (kbd "C-c q") 'auto-fill-mode)
)

;; Open tabs in Aquamacs
(if (featurep 'aquamacs)
    (global-set-key (kbd "A-T") 'new-tab))

(provide 'custom-keys)
