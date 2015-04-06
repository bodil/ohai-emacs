;;; -*- lexical-binding: t -*-
;;; ohai-navigation.el --- Moving around.

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

;; Make PgUp/Dn move the point.
(setq scroll-error-top-bottom t)

;; ace-jump-mode!
(package-require 'ace-jump-mode)
(global-set-key (kbd "C-;") 'ace-jump-mode)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))

;; Smart home key.
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line."
  (interactive "^")
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))
(global-set-key (kbd "<home>") 'smart-beginning-of-line)
(global-set-key (kbd "C-a") 'smart-beginning-of-line)

;; Consider CamelCase chunks as words when navigating.
(global-subword-mode 1)

;; Swap buffers in open windows with C-x C-o.
(package-require 'swap-buffers)
(global-set-key (kbd "C-x C-o") 'swap-buffers)

;; Enhance C-x o when more than two windows are open.
(package-require 'ace-window)
(global-set-key (kbd "C-x o") 'ace-window)

;; Use C-x M-p to kill the buffer in the other window, revealing
;; the next buffer in the stack.
(global-set-key
 (kbd "C-x M-p")
 (lambda () (interactive)
   (save-excursion
     (other-window 1)
     (quit-window))))

;; Display incremental search stats in the modeline.
(package-require 'anzu)
(global-anzu-mode 1)



(provide 'ohai-navigation)
