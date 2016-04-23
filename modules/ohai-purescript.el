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
(use-package purescript-mode
  :commands purescript-mode
  :mode (("\\.purs$" . purescript-mode))
  :config
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
    (define-key purescript-mode-map (kbd "⇒") (lambda () (interactive) (insert "=>")))))

(use-package flycheck-purescript
  :commands flycheck-purescript-setup)
(with-eval-after-load "flycheck"
  (add-hook 'purescript-mode-hook 'flycheck-purescript-setup))

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

(use-package psc-ide
  :init
  ;; psc-ide
  (setq psc-ide-executable (or (ohai/resolve-exec "psc-ide") "psc-ide"))
  (setq psc-ide-server-executable (or (ohai/resolve-exec "psc-ide-server") "psc-ide-server"))
  :config
  (add-hook 'purescript-mode-hook 'psc-ide-mode))

(provide 'ohai-purescript)
;;; ohai-purescript.el ends here
