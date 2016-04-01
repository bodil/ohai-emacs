;;; -*- lexical-binding: t -*-
;;; ohai-help.el --- It's dangerous to go alone! Take this.

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

;; which-key prompts you with available options when you type a partial
;; command sequence. Try it out: hit C-x and just wait for two seconds.
(use-package which-key
  :commands which-key-mode
  :demand t
  :config
  (which-key-mode)
  ;; Set the delay before which-key appears.
  (setq-default which-key-idle-delay 2.0)
  ;; which-key will truncate special keys by default, eg. SPC turns into
  ;; an orange D. Turn this off to avoid confusion.
  (setq-default which-key-special-keys nil)
  ;; Hit C-h C-k to have which-key show you all top level key bindings.
  :bind ("C-h C-k" . which-key-show-top-level)
  :diminish which-key-mode)

;; Get an instant cheat sheet for your current major mode
;; with C-h C-m.
(use-package discover-my-major
  :commands (discover-my-major discover-my-mode)
  :bind ("C-h C-m" . discover-my-major))



(provide 'ohai-help)
