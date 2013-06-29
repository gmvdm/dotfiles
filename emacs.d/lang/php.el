;;;; php.el - settings for PHP programming

(setq auto-mode-alist (cons '("\\.php$" . php-mode) auto-mode-alist))
(autoload 'php-mode "php-mode" "PHP editing mode." t)


(provide 'php)
