;;; -*- lexical-binding: t -*-
;;; ohai-eshell.el --- Make eshell marginally less intolerable.

;; Copyright (C) 2015

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

;; Define a keybinding to get to your eshell quickly.
(global-set-key (kbd "C-c e") 'eshell)

;; Visual commands are commands which require a proper terminal.
;; eshell will run them in a term buffer when you invoke them.
(setq eshell-visual-commands
      '("less" "tmux" "htop" "top" "bash" "zsh" "fish"))
(setq eshell-visual-subcommands
      '(("git" "log" "l" "diff" "show")))

;; Suggest alternatives for mistyped commands.
;; (use-package eshell-did-you-mean
;;   :config
;;   (eshell-did-you-mean-setup))

;; Define a pretty prompt.
(use-package eshell-git-prompt
  :config
  (eshell-git-prompt-use-theme 'powerline))

(setq eshell-cmpl-cycle-completions nil)

(provide 'ohai-eshell)
;;; ohai-eshell.el ends here
