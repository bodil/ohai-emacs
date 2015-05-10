;;; -*- lexical-binding: t -*-
;;; ohai-elixir.el --- Elixir language support.

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

;; Set up the basic Elixir mode.
(package-require 'elixir-mode)

;; Alchemist offers integration with the Mix tool.
(package-require 'alchemist)
(with-eval-after-load "elixir-mode"
  (add-hook 'elixir-mode-hook 'alchemist-mode))

;; Bind some Alchemist commands to more commonly used keys.
(with-eval-after-load "alchemist"
  (define-key alchemist-mode-map
    (kbd "C-c C-l")
    (lambda () (interactive)
      (save-buffer)
      (alchemist-iex-compile-this-buffer)))
  (define-key alchemist-mode-map
    (kbd "C-x C-e") 'alchemist-iex-send-current-line))

;; A Flycheck checker that uses Mix, so it finds project deps.
;; From https://github.com/ananthakumaran/dotfiles/blob/master/.emacs.d/init-elixir.el#L25-L42
(with-eval-after-load "flycheck"
  (flycheck-define-checker elixir-mix
    "An Elixir syntax checker using the Elixir interpreter.
See URL `http://elixir-lang.org/'."
    :command ("mix"
              "compile"
              source)
    :error-patterns
    ((error line-start "** (" (zero-or-more not-newline) ") "
            (zero-or-more not-newline) ":" line ": " (message) line-end)
     (warning line-start
              (one-or-more (not (syntax whitespace))) ":"
              line ": "
              (message)
              line-end))
    :modes elixir-mode)
  (add-to-list 'flycheck-checkers 'elixir-mix))



(provide 'ohai-elixir)
;;; ohai-elixir.el ends here
