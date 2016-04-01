;;; -*- lexical-binding: t -*-
;;; ohai-erlang.el --- Things for working with erlang.

;; Copyright (C) 2015 Bodil Stokke

;; Curated and improved by Bernard Notarianni

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

;; Let's use the regular erlang-mode
(use-package erlang
  :commands erlang-mode
  :config
  ;; Setup C-c e to add an export clause for the function under cursor.
  (add-hook 'erlang-mode-hook (lambda () (local-set-key "\C-ce" 'erlang-export)))
  ;; Avoid Warning about final newline with Erlang mode
  (add-hook 'erlang-mode-hook
            (lambda ()
              (setq require-final-newline nil)
              (setq mode-require-final-newline nil))))

(defun stripwhite (str)
  "Remove any whitespace from STR."
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (replace-regexp-in-string "[ \t\n]*" "" s)))

(defun erlang-move-to-export-insertion ()
  (interactive)
    (goto-char (point-max))
    (if (search-backward-regexp "^-export" 0 t)
    (end-of-line)
      (search-backward-regexp "^-" 0 t)
      (end-of-line)))

(defun erlang-export (fun-arity)
  (interactive
   (list (read-no-blanks-input "function/arity: " (erlang-current-function))))
  (save-excursion
    (erlang-move-to-export-insertion)
    (newline)
    (insert (format "-export ([%s])." fun-arity))))

;; Find the name of the function under the cursor.
(defun erlang-current-function ()
  (save-excursion
    (if (not (equal (point) (point-max))) (forward-char))
    (erlang-beginning-of-function)
    (let ((beg (point))
          (fun-name)
          (fun-arity)
          (result '()))
      (search-forward "(")
      (backward-char)
      (setq fun-name (stripwhite (buffer-substring beg (point))))
      (if (char-equal (char-after) ?\))
          (setq fun-arity 0)
        (forward-char)
        (setq fun-arity 0)
        (while (not (char-equal (char-after) ?\)))
          (erlang-forward-arg)
          (setq fun-arity (+ fun-arity 1))))
      (format "%s/%d" fun-name fun-arity))))

(defun erlang-forward-arg ()
  (forward-sexp))



(provide 'ohai-erlang)
