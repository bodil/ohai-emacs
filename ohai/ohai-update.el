;;; -*- lexical-binding: t -*-
;;; ohai-update.el --- Update your ohai-emacs installation.

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
;;; Commentary:

;; Hello world!

;;; Code:

(defun ohai/update ()
  (interactive)
  (let ((default-directory dotfiles-dir)
        (buf (get-buffer-create "*ohai-emacs update*")))
    (switch-to-buffer-other-window buf)
    (shell-command "git pull --ff-only --stat" buf)
    (end-of-buffer)
    (insert "\nRun `M-x ohai/select-modules' to review and install new modules.\n")
    (local-set-key (kbd "q") 'quit-window)))

(provide 'ohai-update)
;;; ohai-update.el ends here
