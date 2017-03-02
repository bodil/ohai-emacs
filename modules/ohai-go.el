;;; -*- lexical-binding: t -*-
;;; ohai-go.el --- Use various go tools to make editing go better with ohai

;; Copyright (C) 2016 Bodil Stokke

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

(use-package go-mode
  :mode (("\\.go$" . go-mode))
  :init
  ;; If necessary install the missing go dependencies, go get is smart
  ;; enough to to not run the install again for something that we already have
  (if (not (eq (executable-find "go") nil))
      (progn
        (call-process-shell-command "go get github.com/nsf/gocode" nil "*go-get-output*" t)
        (call-process-shell-command "go get golang.org/x/tools/cmd/goimports" nil "*go-get-output*" t)
        (call-process-shell-command "go get github.com/rogpeppe/godef" nil "*go-get-output*" t)
        (call-process-shell-command "go get github.com/golang/lint" nil "*go-get-output*" t)
        (call-process-shell-command "go get golang.org/x/tools/cmd/gorename" nil "*go-get-output*" t)
        (call-process-shell-command "go get golang.org/x/tools/cmd/guru" nil "*go-get-output*" t))
      (message "go executable not found please install go from https://golang.org/download"))

  ;; go-eldoc gives us really nice info for go functions, like elisp
  (use-package go-eldoc
    :config
    (add-hook 'go-mode-hook 'go-eldoc-setup))

  :config
  (setq tab-width 4)
  (defvaralias 'c-basic-offset 'tab-width))

(use-package flycheck)

(with-eval-after-load "company"
  (use-package company-go
    :config
    (add-to-list 'company-backends 'company-go)))

(defun go-mode-update-tools ()
  (interactive)
  (message "updating go tools via go get")
  (call-process-shell-command "go get -u github.com/nsf/gocode" nil "*go-get-output*" t)
  (call-process-shell-command "go get -u github.com/alecthomas/gometalinter" nil "*go-get-output*" t)
  (call-process-shell-command "go get -u golang.org/x/tools/cmd/goimports" nil "*go-get-output*" t)
  (call-process-shell-command "go get -u github.com/rogpeppe/godef" nil "*go-get-output*" t)
  (call-process-shell-command "go get -u github.com/golang/lint" nil "*go-get-output*" t)
  (call-process-shell-command "go get -u golang.org/x/tools/cmd/gorename" nil "*go-get-output*" t)
  (call-process-shell-command "go get -u golang.org/x/tools/cmd/guru" nil "*go-get-output*" t))

(provide 'ohai-go)
