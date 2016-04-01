;;; -*- lexical-binding: t -*-
;;; ohai-elisp.el --- Making your lisp sound more pleasant.

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
(require 'ohai-personal-taste)

;; If you're ready for Paredit, let's do Paredit!
;; Learn Paredit: http://pub.gajendra.net/src/paredit-refcard.pdf
(when ohai-personal-taste/paredit
  (use-package paredit
    :commands paredit-mode
    :config
    (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
    ;; Setup C-c v to eval whole buffer in all lisps.
    (define-key lisp-mode-shared-map (kbd "C-c v") 'eval-buffer)
    :diminish paredit-mode))

;; Highlight the sexp under the cursor.
(use-package highlight-parentheses
  :commands highlight-parentheses-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'highlight-parentheses-mode)
  :diminish highlight-parentheses-mode)

;; When saving an elisp file, remove its compiled version if
;; there is one, as you'll want to recompile it.
(defun ohai-elisp/remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))
(add-hook 'emacs-lisp-mode-hook 'ohai-elisp/remove-elc-on-save)

;; Enable eldoc mode, which provides context based documentation
;; in the minibuffer.
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

;; Use M-. to jump to the definition of the symbol under the cursor.
(define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)



(provide 'ohai-elisp)
