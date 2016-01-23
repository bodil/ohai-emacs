;;; -*- lexical-binding: t -*-
;;; ohai-haskell.el --- FIRE ALL MONAD TRANSFORMERS

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

(use-package haskell-mode
  :commands haskell-mode
  :config
  ;; Setup haskell-mode hooks
  (custom-set-variables
   '(haskell-mode-hook
     '(turn-on-haskell-indentation
       turn-on-haskell-doc)))
  ;; Setup haskell-interactive-mode keybindings. Get started by using C-c C-z from
  ;; a buffer visiting a file in your Haskell project.
  ;; Switch to the current REPL buffer, starting a session if needed.
  (bind-keys :map haskell-mode-map
             ("C-c C-z" . haskell-interactive-switch))
  ;; Switch between REPL sessions.
  (bind-keys :map haskell-mode-map
             ("C-c b" . haskell-session-change))
  ;; Load the current buffer into the REPL.
  (bind-keys :map haskell-mode-map
             ("C-c C-l" . haskell-process-load-file))
  ;; Infer the type of the thing at point.
  (bind-keys :map haskell-mode-map
             ("C-c C-t" . haskell-process-do-type))
  ;; Display info (in the REPL) about the thing at point.
  (bind-keys :map haskell-mode-map
             ("C-c C-i" . haskell-process-do-info))
  ;; Insert the inferred type of the function at point into the code.
  (bind-keys :map haskell-mode-map
             ("C-c C-s" . (lambda () (interactive) (haskell-process-do-type t))))
  ;; Run `cabal test' in a compile buffer.
  (bind-keys :map haskell-mode-map
             ("C-c C-," . ohai-haskell/run-test-suite))
  ;; Change some ASCII art syntax into their corresponding Unicode characters.
  ;; Rebind the same Unicode characters to insert their ASCII art versions
  ;; if entered from the keyboard.
  ;; This is very much a matter of taste, feel free to disable it. Or better yet,
  ;; if you're into that sort of thing, see https://wiki.haskell.org/Unicode-symbols
  ;; for native Unicode support.
  (with-eval-after-load "haskell-mode"
    (ohai/font-lock-replace-symbol 'haskell-mode "\\(->\\)" "→")
    (ohai/font-lock-replace-symbol 'haskell-mode "\\(<-\\)" "←")
    (ohai/font-lock-replace-symbol 'haskell-mode "\\(=>\\)" "⇒")
    (define-key haskell-mode-map (kbd "→") (lambda () (interactive) (insert "->")))
    (define-key haskell-mode-map (kbd "←") (lambda () (interactive) (insert "<-")))
    (define-key haskell-mode-map (kbd "⇒") (lambda () (interactive) (insert "=>")))))


;; A function for launching a compile buffer with `cabal test'.
(defun ohai-haskell/run-test-suite ()
  (interactive)
  (require 'compile)
  (projectile-with-default-dir (projectile-project-root)
    (compile "cabal test")))

;; Flycheck addons
(use-package flycheck-haskell
  :config
  (with-eval-after-load "flycheck"
    (with-eval-after-load "haskell"
      (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))))



(provide 'ohai-haskell)
;;; ohai-haskell.el ends here
