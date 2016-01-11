;;; -*- lexical-binding: t -*-
;;; ohai-flycheck.el --- Spot all the errors.

;; Copyright (C) 2015 Bodil Stokke

;; Author: Bodil Stokke <bodil@bodil.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'ohai-package)

;; Install Flycheck.
(use-package flycheck
  :config
  ;; Start it automatically for all modes except ELisp mode,
  ;; where the linter is just designed to make you mad.
  (add-hook 'find-file-hook
            (lambda ()
              (when (not (equal 'emacs-lisp-mode major-mode))
                (flycheck-mode)))))

;; Jump between current errors with M-n and M-p.
(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)

;; Turn the modeline red when Flycheck has errors.
(use-package flycheck-color-mode-line
  :config
  ;; Configure the theme.
  (with-eval-after-load "flycheck"
    (setq flycheck-highlighting-mode 'symbols)
    (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))


(add-hook
 'ohai-appearance/dark-hook
 (lambda ()
   (with-eval-after-load "flycheck"
     (set-face-background 'flycheck-error "#660000")
     (set-face-foreground 'flycheck-error nil)
     (set-face-background 'flycheck-warning "#331800")
     (set-face-foreground 'flycheck-warning nil)
     (require 'flycheck-color-mode-line)
     (set-face-background 'flycheck-color-mode-line-error-face "#440000")
     (set-face-background 'flycheck-color-mode-line-warning-face "#553300")
     (set-face-background 'flycheck-color-mode-line-info-face nil)
     (set-face-foreground 'flycheck-color-mode-line-error-face "#ffffff")
     (set-face-foreground 'flycheck-color-mode-line-warning-face "#ffffff")
     (set-face-foreground 'flycheck-color-mode-line-info-face nil))))

(add-hook
 'ohai-appearance/light-hook
 (lambda ()
   (with-eval-after-load "flycheck"
     (set-face-background 'flycheck-error "#ff8888")
     (set-face-foreground 'flycheck-error nil)
     (set-face-background 'flycheck-warning "#ffcc88")
     (set-face-foreground 'flycheck-warning nil)
     (require 'flycheck-color-mode-line)
     (set-face-background 'flycheck-color-mode-line-error-face "#ff0000")
     (set-face-foreground 'flycheck-color-mode-line-error-face "#ffffff")
     (set-face-background 'flycheck-color-mode-line-warning-face "#886600")
     (set-face-foreground 'flycheck-color-mode-line-warning-face "#ffffff")
     (set-face-background 'flycheck-color-mode-line-info-face nil)
     (set-face-foreground 'flycheck-color-mode-line-info-face nil))))



(provide 'ohai-flycheck)
