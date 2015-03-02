;;; -*- lexical-binding: t -*-
;;; ohai-test.el --- Test environment for ohai-emacs.

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

;; Should be run from the repo with HOME=`pwd`.

(load-file "$HOME/ohai/ohai-module-index.el")

(setq ohai-personal-taste/paredit t
      ohai-personal-taste/run-wizard nil
      ohai/modules (mapcar #'car ohai/available-modules))

(load-file "$HOME/init.el")

(kill-emacs 0)

;;; ohai-test.el ends here
