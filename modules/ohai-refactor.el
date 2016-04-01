;;; -*- lexical-binding: t -*-
;;; ohai-refactor.el --- The EMR generic refactoring package.

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

(use-package emr
  :config
  (add-hook 'prog-mode-hook 'emr-initialize)
  ;; Just hit M-RET to access your refactoring tools in any supported mode.
  (define-key prog-mode-map (kbd "M-RET") 'emr-show-refactor-menu))

(provide 'ohai-refactor)
;;; ohai-refactor.el ends here
