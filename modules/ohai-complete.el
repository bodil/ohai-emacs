;;; -*- lexical-binding: t -*-
;;; ohai-complete.el --- All cool IDEs have IntelliSense.

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
(require 'ohai-appearance)

(use-package company
  :demand t
  :commands company-mode
  :config
  ;; Enable company-mode globally.
  (global-company-mode)
  ;; Except when you're in term-mode.
  (setq company-global-modes '(not term-mode))
  ;; Give Company a decent default configuration.
  (setq company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-show-numbers t
        company-tooltip-align-annotations t
        company-require-match nil
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil)
  ;; Sort completion candidates that already occur in the current
  ;; buffer at the top of the candidate list.
  (setq company-transformers '(company-sort-by-occurrence))
  ;; Show documentation where available for selected completion
  ;; after a short delay.
  (use-package company-quickhelp
    :config
    (setq company-quickhelp-delay 1)
    (company-quickhelp-mode 1))
  ;; Add a completion source for emoji. 😸
  (use-package company-emoji
    :config
    (company-emoji-init))
  ;; Company's default colours look OK with the light scheme,
  ;; but hideous with the dark one, so let's pick something nicer.
  (add-hook
   'ohai-appearance/dark-hook
   (lambda ()
     (set-face-foreground 'company-tooltip "#000")
     (set-face-background 'company-tooltip "#ddd")
     (set-face-background 'company-scrollbar-bg "#fff")
     (set-face-background 'company-scrollbar-fg "#999")
     (set-face-background 'company-tooltip-selection "#aaa")
     (set-face-foreground 'company-tooltip-common "#9a0000")
     (set-face-foreground 'company-tooltip-common-selection "#9a0000")
     (set-face-foreground 'company-tooltip-annotation "#00008e")))
  ;; Use C-\ to activate the Company autocompleter.
  ;; We invoke company-try-hard to gather completion candidates from multiple
  ;; sources if the active source isn't being very forthcoming.
  (use-package company-try-hard
    :commands company-try-hard
    :bind ("C-\\" . company-try-hard)
    :config
    (bind-keys :map company-active-map
               ("C-\\" . company-try-hard)))
  :diminish company-mode)

(provide 'ohai-complete)
