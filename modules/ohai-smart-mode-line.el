;;; -*- lexical-binding: t -*-
;;; ohai-smart-mode-line.el --- A cleaner modeline.

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

(use-package "rich-minority"
  :commands rich-minority-mode
  :demand t
  :init
  (setq rm-blacklist '(" Helm" " Guide" " $" " ," " Tern" " Ind" " alchemist"
                       " Monroe" " cljr" " Wrap" " Doc"))
  :config
  (rich-minority-mode 1))

(use-package "smart-mode-line"
  :commands sml/setup
  :demand t
  :init
  (setq sml/theme 'respectful
        sml/shorten-directory t
        sml/shorten-modes t
        sml/name-width 40
        sml/mode-width 'full)
  :config
  (nyan-mode 0)
  (sml/setup))



(provide 'ohai-smart-mode-line)
;;; ohai-smart-mode-line.el ends here
