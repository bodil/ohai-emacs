;;; -*- lexical-binding: t -*-
;;; ohai-help.el --- It's dangerous to go alone! Take this.

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

(package-require 'guide-key)
(setq-default guide-key/guide-key-sequence t
              guide-key/recursive-key-sequence-flag t
              guide-key/idle-delay 2
              guide-key/popup-window-position 'bottom
              guide-key/text-scale-amount -2)
(guide-key-mode 1)

;; Get an instant cheat sheet for your current major mode
;; with C-h C-m.
(package-require 'discover-my-major)
(global-set-key (kbd "C-h C-m") 'discover-my-major)



(provide 'ohai-help)
