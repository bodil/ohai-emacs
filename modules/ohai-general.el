;;; -*- lexical-binding: t -*-
;;; ohai-general.el --- Setting up the basics.

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

;; Make sure we always use UTF-8.
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(load-library "iso-transl")

;; Always ask for y/n keypress instead of typing out 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; Emacs writes backup files to `filename~` by default. This is messy,
;; so let's tell it to write them to `~/.emacs.d/bak` instead.
;; If you have an accident, check this directory - you might get lucky.
(setq backup-directory-alist
      `(("." . ,(expand-file-name (concat dotfiles-dir "bak")))))

;; Automatically save buffers before launching M-x compile and friends,
;; instead of asking you if you want to save.
(setq compilation-ask-about-save nil)

;; Make the selection work like most people expect.
(delete-selection-mode t)
(transient-mark-mode t)

;; Automatically update unmodified buffers whose files have changed.
(global-auto-revert-mode 1)

;; If available, use `xdg-open' to open URLs.
(when (ohai/is-exec "xdg-open")
  (setq-default
   browse-url-browser-function (quote browse-url-generic)
   browse-url-generic-program "xdg-open"))

(provide 'ohai-general)
