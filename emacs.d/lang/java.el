;;;; java.el Java specific configuration

;; Spotify format for Playlist code
(add-hook 'java-mode-hook (lambda ()
                            (setq c-basic-offset 2
                                  tab-width 2
                                  indent-tabs-mode t)))


(provide 'java)
