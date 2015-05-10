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

;; web-mode is a special mode for HTML which copes with embedded JS/CSS,
;; JSX, various templating systems, etc.
;; Learn about web-mode: http://web-mode.org/
(package-require 'web-mode)

;; We'd like to use web-mode for HTML, instead of the default html-mode.
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; Let's add some extensions from the web-mode docs too.
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

;; Highlight the element under the cursor.
(setq-default web-mode-enable-current-element-highlight t)

;; Key for renaming tags
(with-eval-after-load "web-mode"
  (define-key web-mode-map (kbd "C-c C-r") 'mc/mark-sgml-tag-pair))

;; Colourise colour names in certain modes.
(package-require 'rainbow-mode)
(dolist (mode '(css-mode less-css-mode html-mode web-mode))
  (add-hook (intern (concat (symbol-name mode) "-hook"))
            (lambda () (rainbow-mode))))



(provide 'ohai-html)
