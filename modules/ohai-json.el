;;; -*- lexical-binding: t -*-
;;; ohai-json.el --- JSON is the Universal Data Format!

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

;; This file is not a proper module; it's loaded by both ohai-javascript
;; and ohai-js-web-mode.

;;; Code:

;; Install json-mode and make its reformat keybinding match the global default.
(package-require 'json-mode)
(define-key json-mode-map (kbd "C-c <tab>") 'json-mode-beautify)

(provide 'ohai-json)
;;; ohai-json.el ends here
