;;; -*- lexical-binding: t -*-
;;; ohai-module-index.el --- The index of available Ohai Emacs modules.

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

(setq
 ohai/available-modules
 '((ohai-appearance "how Emacs looks" :recommended)
   (ohai-fonts "adjust font size on the fly" :recommended)
   (ohai-general "basic editor settings" :recommended)
   (ohai-splash "enhance your scratch buffer" :recommended)
   (ohai-ido "improved file selector etc" :recommended)
   (ohai-navigation "moving around better" :recommended)
   (ohai-editing "editing improvements (multiple cursors etc)" :recommended)
   (ohai-complete "auto completion" :recommended)
   (ohai-snippets "snippet management" :recommended)
   (ohai-codestyle "code formatting, whitespace management" :recommended)
   (ohai-dired "enhanced file manager" :recommended)
   (ohai-project "manage projects with Projectile" :recommended)
   (ohai-flycheck "run linters automatically with Flycheck" :recommended)
   (ohai-git "Git tools" :recommended)
   (ohai-orgmode "your personal everything manager" :recommended)
   (ohai-help "ways to get more help" :recommended)
   (ohai-elisp "Emacs Lisp" :recommended)
   (ohai-helm "advanced selection and narrowing" :optional)
   (ohai-eshell "the native Emacs shell" :optional)
   (ohai-unicode "enhanced Unicode support (warning: slows startup)" :optional)
   (ohai-refactor "easy access to refactoring tools" :optional)
   (ohai-emoji "display Unicode emoji even if your system doesn't" :optional)
   (ohai-html "HTML, CSS and friends" :optional)
   (ohai-markdown "Markdown support" :optional)
   (ohai-javascript "JavaScript language support" :optional)
   (ohai-js-web-mode "alternative JS support using web-mode" :optional)
   (ohai-purescript "PureScript language support" :optional)
   (ohai-clojure "Clojure language support" :optional)
   (ohai-erlang "Erlang language support" :optional)
   (ohai-elixir "Elixir language support" :optional)
   (ohai-haskell "Haskell language support" :optional)
   (ohai-flow "the Flow type checker for JS" :optional)))

(require 'cl)
(defcustom ohai/modules (mapcar #'car
                                (remove-if-not
                                 (lambda (i) (equal :recommended (caddr i)))
                                 ohai/available-modules))
  "Your choice of Ohai Emacs modules.")

(defun ohai/load-modules ()
  (interactive)
  (dolist (module ohai/modules) (require module nil t))
  (run-hooks 'ohai/modules-loaded-hook))

(provide 'ohai-module-index)
;;; ohai-module-index.el ends here
