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

;; Extend purescript-mode with psc-ide.
(use-package psc-ide
  :init
  ;; psc-ide
  (setq psc-ide-client-executable (or (ohai/resolve-exec "psc-ide-client") "psc-ide-client"))
  (setq psc-ide-server-executable (or (ohai/resolve-exec "psc-ide-server") "psc-ide-server"))
  (setq psc-ide-rebuild-on-save nil)
  :config
  (add-hook 'purescript-mode-hook 'psc-ide-mode))

;; Extend Flycheck with psc-ide capabilities.
(with-eval-after-load "flycheck"
  (when (fboundp 'psc-ide-command-rebuild)
    (flycheck-def-option-var flycheck-psc-ide-ignore-error-codes nil psc
      "List of psc error codes to ignore.

The value of this variable is a list of strings, where each
string is a name of an error code to ignore (e.g. \"MissingTypeDeclaration\")."
      :type '(repeat :tag "Extensions" (string :tag "Extension"))
      :safe #'flycheck-string-list-p)

    (flycheck-define-generic-checker 'flycheck-psc-ide
      "Check buffer using psc-ide rebuild."
      :start (lambda (checker done)
               (funcall done 'finished (ohai-purescript/rebuild-to-flycheck)))
      :modes 'purescript-mode)
    (add-to-list 'flycheck-checkers 'flycheck-psc-ide)

    (defun ohai-purescript/rebuild-to-flycheck ()
      "Rebuild the current module."
      (let* ((res (json-read-from-string
                   (psc-ide-send (psc-ide-command-rebuild))))
             (is-success (string= "success" (cdr (assoc 'resultType res))))
             (result (cdr (assoc 'result res))))
        (ohai-purescript/save-suggestions (append result nil))
        (-filter (lambda (i) (not (eq i nil)))
                 (if (not is-success)
                     (-map (lambda (err)
                             (ohai-purescript/error 'error err))
                           result)
                   (if (> (length result) 0)
                       (-map (lambda (err)
                               (ohai-purescript/error 'warning err))
                             result)
                     nil)))))

    (defun ohai-purescript/save-suggestions (errs)
      (setq-local
       ohai-purescript/suggestions
       (-map
        (lambda (err)
          (let* ((err-filename (cdr (assoc 'filename err)))
                 (err-position (cdr (assoc 'position err)))
                 (err-line (cdr (assoc 'startLine err-position)))
                 (err-column (cdr (assoc 'startColumn err-position)))
                 (err-id (concat err-filename ":" (number-to-string err-line)
                                 ":" (number-to-string err-column))))
            (cons err-id err)))
        (-filter (lambda (i) (and (cdr (assoc 'position i))
                                  (cdr (assoc 'suggestion i))))
                 errs))))

    (defun ohai-purescript/error (severity err)
      (let* ((err-message (cdr (assoc 'message err)))
             (err-filename (cdr (assoc 'filename err)))
             (err-position (cdr (assoc 'position err)))
             (err-code (cdr (assoc 'errorCode err)))
             (err-line (cdr (assoc 'startLine err-position)))
             (err-column (cdr (assoc 'startColumn err-position))))
        (when (and err-position (not (member err-code flycheck-psc-ide-ignore-error-codes)))
          (flycheck-error-new-at
           err-line
           err-column
           severity
           err-message
           :id (concat err-filename ":" (number-to-string err-line)
                       ":" (number-to-string err-column))))))

    (defun ohai-purescript/insert-suggestion ()
      (interactive)
      (let* ((id (flycheck-error-id (car (flycheck-overlay-errors-at (point)))))
             (err (cdr (assoc id ohai-purescript/suggestions)))
             (pos (cdr (assoc 'position err)))
             (sugg (cdr (assoc 'suggestion err))))
        (if (and pos sugg)
            (let* ((start (save-excursion
                            (goto-char (point-min))
                            (forward-line (- (cdr (assoc 'startLine pos)) 1))
                            (move-to-column (- (cdr (assoc 'startColumn pos)) 1))
                            (point)))
                   (end (save-excursion
                          (goto-char (point-min))
                          (forward-line (- (cdr (assoc 'endLine pos)) 1))
                          (move-to-column (- (cdr (assoc 'endColumn pos)) 1))
                          (point))))
              (progn
                (kill-region start end)
                (goto-char start)
                (let ((new-end
                       (save-excursion
                         (insert (cdr (assoc 'replacement sugg)))
                         (point))))
                  (set-mark start)
                  (goto-char new-end)
                  (setq deactivate-mark nil))))
          (message "No suggestion available!"))))

    (define-key purescript-mode-map (kbd "C-c M-s")
      'ohai-purescript/insert-suggestion)))

;; Stop eldoc's whining.
(defun purescript-doc-current-info () nil)

(provide 'ohai-purescript)
;;; ohai-purescript.el ends here
