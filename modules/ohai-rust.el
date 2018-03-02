;;; -*- lexical-binding: t -*-
;;; ohai-rust.el --- Rust language support.

;; Copyright (C) 2017 Bodil Stokke

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

(use-package rust-mode)

;; (setenv "RUSTC" (ohai/resolve-exec "rustc"))

;; (use-package flycheck-rust
;;   :config
;;   (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
;; (use-package racer
;;   :config
;;   (add-hook 'rust-mode-hook #'racer-mode)
;;   (add-hook 'racer-mode-hook #'eldoc-mode)
;;   :diminish racer-mode)
(use-package cargo
  :hook (rust-mode . cargo-minor-mode)
  :config
  (setq compilation-ask-about-save nil)
  ;; Automatically re-run compilation command on manual save inside a project.
  ;; Will do nothing if a compilation hasn't been manually triggered
  ;; in the past.
  (with-eval-after-load "projectile"
    (bind-key "C-c C-s" #'ohai-rust/save-and-recompile)
    (bind-key "C-c s" #'ohai-rust/save-all-and-recompile))
  :diminish cargo-minor-mode)

;; If the LSP module is enabled, set up RLS support.
(with-eval-after-load "ohai-lsp"
  (require 'ohai-flycheck)
  (use-package lsp-rust
    :hook ((rust-mode . lsp-rust-enable)
           (rust-mode . flycheck-mode))))

(defun ohai-rust/save-and-recompile ()
  (interactive)
  (save-buffer)
  (when compile-history
    (let ((cmd (car compile-history)))
      (projectile-run-compilation cmd))))

(defun ohai-rust/save-all-and-recompile ()
  (interactive)
  (save-some-buffers)
  (when compile-history
    (let ((cmd (car compile-history)))
      (projectile-run-compilation cmd))))



(provide 'ohai-rust)
;;; ohai-rust.el ends here
