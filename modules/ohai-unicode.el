;;; -*- lexical-binding: t -*-
;;; ohai-unicode.el --- Proper Unicode setup, because you need emoji.

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

;; We use the `unicode-fonts' package to set everything up. Beware that the
;; `unicode-fonts-setup' function takes a while to run, which is why this
;; module isn't on by default.

;; You'll need to make sure the necessary fonts are installed for this to
;; work. See https://github.com/rolandwalker/unicode-fonts/#quickstart

(use-package unicode-fonts
  :config
  (unicode-fonts-setup))



(provide 'ohai-unicode)
;;; ohai-unicode.el ends here
