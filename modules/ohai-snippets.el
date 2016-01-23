;;; -*- lexical-binding: t -*-
;;; ohai-snippets.el --- Where there is boilerplate, there must be snippets.

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

;; The s.el package contains a lot of functions useful in snippets.
(use-package s)

;; Install yasnippet and make it available globally.
;; Read about it here: http://capitaomorte.github.io/yasnippet/
(use-package yasnippet
  ;;:commands yas-global-mode
  :config
  (yas-global-mode 1)
  :diminish yas-minor-mode)

(provide 'ohai-snippets)
;;; ohai-snippets.el ends here
