;;; -*- lexical-binding: t -*-
;;; ohai-purescript.el --- Settings for editing PureScript.

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

(require 'ohai-lib)
(require 'ohai-project)

;; Install purescript-mode.
(package-require 'purescript-mode)
(require 'purescript-mode)
(add-to-list 'auto-mode-alist '("\\.purs$" . purescript-mode))
(add-hook 'purescript-mode-hook 'turn-on-purescript-indentation)

;; Change some ASCII art syntax into their corresponding Unicode characters.
;; Rebind the same Unicode characters to insert their ASCII art versions
;; if entered from the keyboard.
(with-eval-after-load "purescript-mode"
  (ohai/font-lock-replace-symbol 'purescript-mode "\\(->\\)" "→")
  (ohai/font-lock-replace-symbol 'purescript-mode "\\(<-\\)" "←")
  (ohai/font-lock-replace-symbol 'purescript-mode "\\(=>\\)" "⇒")
  (define-key purescript-mode-map (kbd "→") (lambda () (interactive) (insert "->")))
  (define-key purescript-mode-map (kbd "←") (lambda () (interactive) (insert "<-")))
  (define-key purescript-mode-map (kbd "⇒") (lambda () (interactive) (insert "=>"))))

;; Define a Flycheck checker for running the PureScript compiler through Pulp.
;; This version comes from https://gist.github.com/cabrera/157699749ea71bae7a16
(with-eval-after-load "flycheck"
  (flycheck-define-checker pulp
    "Use Pulp to flycheck PureScript code."
    :command ("pulp" "--monochrome" "build")
    :error-patterns
    ((error line-start
            (or (and "Error at " (file-name) " line " line ", column " column
                     (one-or-more not-newline)
                     (message (one-or-more (not (in "*")))))

                (and "psc: " (one-or-more not-newline) "\n"
                     (message (one-or-more not-newline) "\n")
                     "at \"" (file-name) "\" (line " line ", column " column ")")
                (and "Unable to parse module:\n"
                     "  \"" (file-name) "\" (line " line ", column " column "):\n"
                     (message (one-or-more not-newline) "\n"
                              (one-or-more not-newline) "\n"
                              (one-or-more not-newline) "\n"))
                )

            line-end
            ))
    :modes purescript-mode)
  (add-to-list 'flycheck-checkers 'pulp))

;; A function for generating a likely module name from the current file path.
;; We use this in the `ps.module' snippet.
(defun purescript-module-name-from-current-buffer-file ()
  (let ((path (f-split (f-relative
                        (f-canonical (f-base (buffer-file-name)))
                        (f-canonical (f-join (projectile-project-root) "src")))))
        (testpath (f-split (f-relative
                            (f-canonical (f-base (buffer-file-name)))
                            (f-canonical (f-join (projectile-project-root) "test"))))))
    (if (string= ".." (car path))
        (if (string= ".." (car testpath)) "Main" (s-join "." (cons "Test" testpath)))
      (s-join "." path))))

;; psc-ide
(setq psc-ide-executable (or (ohai/resolve-exec "psc-ide") "psc-ide"))
(setq psc-ide-server-executable (or (ohai/resolve-exec "psc-ide-server") "psc-ide-server"))
(package-require 'psc-ide)
(add-hook 'purescript-mode-hook 'psc-ide-mode)



(provide 'ohai-purescript)
;;; ohai-purescript.el ends here
