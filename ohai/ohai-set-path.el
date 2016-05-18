;;; -*- lexical-binding: t -*-
;;; ohai-set-path.el --- Set the executable search path from the user shell.

;; Copyright (C) 2016 Bodil Stokke

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

;; To ensure availability of command line tools, we try to get Emacs's
;; executable path from your shell configuration using the
;; `exec-path-from-shell' package.

(paradox-require 'exec-path-from-shell)

(when (memq window-system '(x mac ns))
  (exec-path-from-shell-initialize))

(provide 'ohai-set-path)
;;; ohai-set-path.el ends here
