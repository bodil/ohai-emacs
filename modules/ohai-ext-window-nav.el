;;; -*- lexical-binding: t -*-
;;; ohai-ext-window-nav.el --- Alternative window navigation.

;; Copyright (C) 2016 Bodil Stokke

;; Author: Bodil Stokke <bodil.stokke@tradingtechnologies.com>

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

;; Make C-x o always move to the previous window.
;; C-x C-o will force ace-select window.
;; C-x M-o is ace-swap-window.
(defun ohai/previous-window ()
  (interactive)
  (-let [current (selected-window)]
    (cond
     ((eq ohai/--last-window current)
      (ace-select-window))

     ((window-live-p ohai/--last-window)
      (select-window ohai/--last-window))

     (t
      (ace-select-window)))
    (setq ohai/--last-window current)))

(defun ohai/select-window ()
  (interactive)
  (setq ohai/--last-window (selected-window))
  (ace-select-window))

(setq ohai/--last-window (selected-window))

(global-set-key (kbd "C-x o") 'ohai/previous-window)
(global-set-key (kbd "C-x C-o") 'ohai/select-window)
(global-set-key (kbd "C-x M-o") 'ace-swap-window)



(provide 'ohai-ext-window-nav)
;;; ohai-ext-window-nav.el ends here
