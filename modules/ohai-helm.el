;;; -*- lexical-binding: t -*-
;;; ohai-helm.el --- The Grand Emacs Incremental Narrowing Thingy.

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

(package-require 'helm)
(require 'helm-config)
(require 'helm)

(helm-mode 1)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-g") 'helm-do-grep)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x c g") 'helm-google-suggest)

(with-eval-after-load "ohai-project"
  (package-require 'helm-projectile)
  (global-set-key (kbd "C-c C-f") 'helm-projectile-find-file-dwim))

(helm-autoresize-mode 1)

(setq-default helm-display-header-line nil
              helm-autoresize-min-height 10
              helm-autoresize-max-height 35
              helm-split-window-in-side-p t

              helm-M-x-fuzzy-match t
              helm-buffers-fuzzy-matching t
              helm-recentf-fuzzy-match t
              helm-apropos-fuzzy-match t)

(set-face-attribute 'helm-source-header nil :height 0.75)

(provide 'ohai-helm)
;;; ohai-helm.el ends here
