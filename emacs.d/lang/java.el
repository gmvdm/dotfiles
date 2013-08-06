;;;; java.el Java specific configuration

(require 'google-c-style)
(add-hook 'java-mode-hook 'google-set-c-style)
(add-hook 'java-mode-hook 'google-make-newline-indent)

(provide 'java)
