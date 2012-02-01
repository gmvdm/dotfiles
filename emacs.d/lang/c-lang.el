(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

(defun flymake-cpplint-init ()
  (list "cpplint" (list (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))))
; run cpplint with flymake for these files:
(add-to-list 'flymake-allowed-file-name-masks
             '(".*\\.cpp\\'" flymake-cpplint-init))
(add-to-list 'flymake-allowed-file-name-masks
             '(".*\\.h\\'" flymake-cpplint-init))

(provide 'c-style)