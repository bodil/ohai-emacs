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

;; Disable company-mode for eshell, falling back to pcomplete,
;; which feels more natural for a shell.
(add-hook 'eshell-mode-hook
          (lambda ()
            (company-mode 0)))

;; When completing with multiple options, complete only as much as
;; possible and wait for further input.
(setq eshell-cmpl-cycle-completions nil)

;; esh-autosuggest provides fish shell like autosuggestion from history.
(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode))

;; Extend pcomplete with smart completion provided by the fish shell, or
;; bash if fish isn't available.
(use-package bash-completion
  :if (ohai/is-exec "bash")
  :commands bash-completion-dynamic-complete
  :init
  (add-hook 'shell-dynamic-complete-functions #'bash-completion-dynamic-complete)
  (setq fish-completion-fallback-on-bash-p t))
(use-package fish-completion
  :if (ohai/is-exec "fish")
  :config
  (global-fish-completion-mode))



(provide 'ohai-eshell)
;;; ohai-eshell.el ends here
