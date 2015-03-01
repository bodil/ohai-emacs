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

(package-require 'company)

;; Use C-\ to activate the Company autocompleter.
(require 'company)
(global-company-mode)
(global-set-key (kbd "C-\\") 'company-complete)

(setq company-global-modes '(not term-mode))

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
(package-require 'company-quickhelp)
(setq company-quickhelp-delay 1)
(company-quickhelp-mode 1)

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



(provide 'ohai-complete)
