;;; -*- lexical-binding: t -*-
;;; ohai-js-web-mode.el --- Use web-mode to edit JS.

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
(require 'ohai-lib)

;; If npm is installed, add its local prefix to the executable
;; search path, which helps Emacs find linters etc.
;; This isn't Windows compatible, but then neither is npm, really.
(-when-let (npm-prefix (ohai/exec-if-exec "npm" "config get prefix"))
  (setenv "PATH" (concat npm-prefix "/bin:" (getenv "PATH"))))

;; Use web-mode for all JS files.
(package-require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.es6\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))

(setq web-mode-content-types-alist
      '(("jsx" . "\\.jsx?$")))

;; But for JSON files, it's better to stick with plain old js-mode.
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

;; Use Tern for smarter JS.
(package-require 'tern)
(add-hook 'web-mode-hook (lambda () (tern-mode t)))

;; Locate the Tern binary by querying the system search path, which
;; should now include the local npm prefix.
(setq tern-command (list (or (ohai/resolve-exec "tern") "tern")))

;; Setup Tern as an autocomplete source.
(with-eval-after-load "company"
  (package-require 'company-tern)
  (require 'company-tern)
  (add-to-list 'company-backends 'company-tern))

;; Let Flycheck know that we're using web-mode for JS.
(with-eval-after-load "flycheck"
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (setq flycheck-javascript-eslint-executable (or (ohai/resolve-exec "eslint") "eslint")))



(provide 'ohai-js-web-mode)
