;;; -*- lexical-binding: t -*-
;;; ohai-emoji.el --- Support graphical emoji when font support is missing.

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

(use-package emojify
  :config
  ;; Set emojify to only replace Unicode emoji, and do it everywhere.
  (setq emojify-emoji-styles '(unicode)
        emojify-inhibit-major-modes '())
  ;; Enable it globally.
  (add-hook 'after-init-hook #'global-emojify-mode))

;; Patch emojify to replace emoji everywhere in programming modes.
(defun emojify-valid-prog-context-p (beg end) 't)




(provide 'ohai-emoji)
;;; ohai-emoji.el ends here
