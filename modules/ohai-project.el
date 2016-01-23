;;; -*- lexical-binding: t -*-
;;; ohai-project.el --- Your personal everything manager.

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

;; Install Projectile and activate it for all things.
;; Learn about Projectile: http://batsov.com/projectile/
(use-package projectile
  :demand t
  :commands projectile-global-mode
  :config
  (projectile-global-mode)
  ;; Use C-c C-f to find a file anywhere in the current project.
  :bind ("C-c C-f" . projectile-find-file)
  :diminish projectile-mode)

(provide 'ohai-project)
