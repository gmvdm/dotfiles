;;; mode-line --- Provide a modeline for my emacs setup

;; Copyright (C) 2014 Geoff van der Meer

;; Author: Geoff van der Meer <gmwils@pino.local>
;; Created: 31 Dec 2014
;; Version: 1.0
;; Keywords: emacs
;; X-URL: https://github.com/gmwils/dotfiles

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;; - Small tweaks to the mode line
;;;
;;; See: http://www.lunaryorn.com/2014/07/26/make-your-emacs-mode-line-more-useful.html

;;; Code:

;; More compact position display
(setq-default mode-line-position
              '((-3 "%p") (size-indication-mode ("/" (-4 "%I")))
                " "
                (line-number-mode
                 ("%l" (column-number-mode ":%c")))))

(defvar gmwils-projectile-mode-line
  '(:propertize
    (:eval (when (ignore-errors (projectile-project-root))
             (concat " " (projectile-project-name))))
    face font-lock-constant-face)
  "Mode line format for Projectile.")
(put 'gmwils-projectile-mode-line 'risky-local-variable t)

(setq-default mode-line-format
              '("%e" mode-line-front-space
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-buffer-identification
                " " mode-line-position
                gmwils-projectile-mode-line
                (vc-mode vc-mode) ; VC status
                (flycheck-mode flycheck-mode-line) ; Flycheck status
                " "
                mode-line-misc-info
                " "
                mode-line-modes
                mode-line-end-spaces))


(provide 'mode-line)

;;; mode-line.el ends here
