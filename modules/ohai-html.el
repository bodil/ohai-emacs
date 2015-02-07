;;; -*- lexical-binding: t -*-
;;; ohai-html.el --- Dealing with the W3C.

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
(require 'ohai-editing)

;; Tagedit is a Paredit like extension for html-mode.
;; Learn about Tagedit: https://github.com/magnars/tagedit
(package-require 'tagedit)
(with-eval-after-load "sgml-mode"
  (tagedit-add-paredit-like-keybindings)
  (add-hook 'html-mode-hook (lambda () (tagedit-mode 1))))

;; Key for renaming tags
(with-eval-after-load "sgml-mode"
  (define-key sgml-mode-map (kbd "C-c C-r") 'mc/mark-sgml-tag-pair))

;; Colourise colour names in certain modes.
(package-require 'rainbow-mode)
(dolist (mode '(css-mode less-css-mode html-mode nxhtml-mode nxhtml-mumamo-mode))
  (add-hook (intern (concat (symbol-name mode) "-hook"))
            (lambda () (rainbow-mode))))




(provide 'ohai-html)
