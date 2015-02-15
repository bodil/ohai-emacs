;;; -*- lexical-binding: t -*-
;;; ohai-clojure.el --- If you like your parentheses Java flavoured.

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

(package-require 'clojure-mode)

;; We'll be using Monroe, a simple nREPL client. To use it, you'll need
;; to start an nREPL somewhere (running `lein repl' in your project should
;; do the trick) and run `M-x monroe' to connect to it and open a REPL
;; buffer.
(package-require 'monroe)

;; We'll also be using clj-refactor for refactoring support. The features
;; which require CIDER won't work with Monroe.
(package-require 'clj-refactor)

;; We might need Paredit too if that's how you like it.
(package-require 'paredit)

;; Setup
(add-hook
 'clojure-mode-hook
 (lambda ()
   (when ohai-personal-taste/paredit (paredit-mode))
   (clj-refactor-mode 1)
   (require 'monroe)
   (clojure-enable-monroe)))

(with-eval-after-load "clojure-mode"
  ;; Rebind C-x C-e to eval through nREPL in clojure-mode buffers.
  (define-key clojure-mode-map (kbd "C-x C-e")
    'monroe-eval-expression-at-point)
  ;; Define the keybinding prefix for clj-refactor commands.
  ;; From there, see https://github.com/clojure-emacs/clj-refactor.el#usage
  (cljr-add-keybindings-with-prefix "C-c C-m"))

;; Monroe doesn't offer any completion support. Let's build on it to
;; add a company-mode backend which queries the connected nREPL for
;; completions.
(with-eval-after-load "clojure-mode"
  (with-eval-after-load "company"

    (defun monroe-eval-string (s callback)
      (monroe-send-eval-string
       s
       (lambda (response)
         (monroe-dbind-response
          response (id ns value err out ex root-ex status)
          (when ns (setq monroe-buffer-ns ns))
          (when value (funcall callback nil value))
          (when status
            (when (member "eval-error" status) (funcall callback ex nil))
            (when (member "interrupted" status) (funcall callback status nil))
            (when (member "need-input" status) (monroe-handle-input))
            (when (member "done" status) (remhash id monroe-requests)))))))

    (defun monroe-get-completions (word callback)
      (interactive)
      (monroe-eval-string
       (format "(complete.core/completions \"%s\")" word)
       (lambda (err s)
         (when (not err) (funcall callback (read-from-whole-string s))))))

    (defun company-monroe (command &optional arg &rest ignored)
      (interactive (list 'interactive))
      (cl-case command
        (interactive (company-begin-backend 'company-monroe))
        (prefix (and (eq major-mode 'clojure-mode)
                     (get-buffer "*monroe-connection*")
                     (company-grab-symbol)))
        (candidates
         (cons :async
               (lambda (callback)
                 (monroe-get-completions arg callback))))))

    (add-to-list 'company-backends 'company-monroe)))

(provide 'ohai-clojure)
;;; ohai-clojure.el ends here
