;;; -*- lexical-binding: t -*-
;;; init.el --- This is where all emacs start.

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

;; We start off by telling Emacs where we intend to keep things, and some
;; other basic system setup.

;; Figure out the current hostname.
(setq hostname (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" (with-output-to-string (call-process "hostname" nil standard-output))))

;; Figure out the path to our .emacs.d by getting the path part of the
;; current file (`init.el`).
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

;; We'll keep the various parts of our configuration in the `modules` subfolder
;; of our .emacs.d. We'll need to add that to the system load path so Emacs can
;; find our modules when we ask for them.
(add-to-list 'load-path (concat dotfiles-dir "modules"))

;; Define where we want to keep `loaddefs.el` (our autoload declarations) and
;; `custom.el` (our user settings file).
(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq custom-file (concat dotfiles-dir "custom.el"))



;; List all the modules we want to use, in order.
;; You should comment out modules you won't be needing.
(setq ohai-module-list
      '(
        ohai-startup-wizard ; first run setup (no effect if run already)
        ohai-package        ; initialise package management
        ohai-appearance     ; how Emacs looks
        ohai-general        ; basic editor settings
        ohai-splash         ; enhance your scratch buffer
        ohai-ido            ; improved file selector etc
        ohai-navigation     ; moving around better
        ohai-editing        ; editing improvements (multiple cursors etc)
        ohai-complete       ; auto completion
        ohai-snippets       ; snippet management
        ohai-codestyle      ; code formatting, whitespace management
        ohai-project        ; manage projects with Projectile
        ohai-flycheck       ; run linters automatically with Flycheck
        ohai-git            ; Git tools
        ohai-orgmode        ; your personal everything manager
        ohai-help           ; ways to get more help
        ohai-elisp          ; Emacs Lisp setup (recommended!)
        ohai-javascript     ; if you have to write Javascript
        ohai-html           ; HTML, CSS and friends
        ohai-markdown       ; Markdown
        ))



;; Go through the list and load each module.
(dolist (module ohai-module-list) (require module))

;; When everything else is done, we load the user settings from `custom.el`.
;; This should always come right at the end of init.el, or its settings might
;; be overwritten by your `.el` files.
(load custom-file 'noerror)
