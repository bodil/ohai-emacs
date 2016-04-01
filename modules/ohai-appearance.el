;;; -*- lexical-binding: t -*-
;;; ohai-appearance.el --- Fashion and aesthetics.

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
(require 'ohai-personal-taste)

(require 'term)

;; Get rid of the training wheels, if you're ready for it.
(when (not ohai-personal-taste/training-wheels)
  (dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1))))

;; Configure the light colour scheme.
(defun ohai-appearance/light ()
  (interactive)
  (load-theme 'leuven)

  (set-face-attribute 'mode-line nil
                      :foreground "#cccccc"
                      :background "#000000"
                      :box nil
                      :weight 'bold)
  (set-face-attribute 'mode-line-buffer-id nil
                      :foreground "white"
                      :weight 'bold)
  (set-face-foreground 'which-func "#3cb64a")
  (set-face-attribute 'linum nil :height 0.7)
  (set-face-attribute 'linum-highlight-face nil :foreground nil :background "#ddd" :height 0.7)

  (set-face-foreground 'term-color-black "#ffffff")
  (set-face-foreground 'term-color-red "#f5666d")
  (set-face-foreground 'term-color-green "#3cb64a")
  (set-face-foreground 'term-color-yellow "#ce5c00")
  (set-face-foreground 'term-color-blue "#00578e")
  (set-face-foreground 'term-color-magenta "#d020f0")
  (set-face-foreground 'term-color-cyan "#6799cc")
  (set-face-foreground 'term-color-white "#000000")

  (run-hooks 'ohai-appearance/hook)
  (run-hooks 'ohai-appearance/light-hook))

;; Configure the dark colour scheme.
(defun ohai-appearance/dark ()
  (interactive)
  (use-package material-theme)
  (load-theme 'material)

  (set-face-background 'default "#000")

  (set-face-background 'region "#223355")
  (set-face-background 'fringe "#000")
  (set-face-attribute
   'linum nil
   :foreground "#678" :background "#000" :height 0.9)
  (set-face-attribute
   'linum-highlight-face nil
   :foreground "#96989c" :background "#263238" :height 0.9)
  (set-face-foreground 'which-func "#7f9f7f")

  (set-face-foreground 'term-color-black "#3f3f3f")
  (set-face-foreground 'term-color-red "#cc9393")
  (set-face-foreground 'term-color-green "#7f9f7f")
  (set-face-foreground 'term-color-yellow "#f0dfaf")
  (set-face-foreground 'term-color-blue "#8cd0d3")
  (set-face-foreground 'term-color-magenta "#dc8cc3")
  (set-face-foreground 'term-color-cyan "#93e0e3")
  (set-face-foreground 'term-color-white "#dcdccc")

  (run-hooks 'ohai-appearance/hook)
  (run-hooks 'ohai-appearance/dark-hook))

;; Setup hooks to re-run after all modules have loaded, allowing
;; other modules to tweak the theme without having to wait
;; until they're loaded to switch to it.
(add-hook
 'ohai/modules-loaded-hook
 (lambda ()
   (run-hooks 'ohai-appearance/hook)
   (cond
    ((equal ohai-personal-taste/style 'dark)
     (run-hooks 'ohai-appearance/dark-hook))
    ((equal ohai-personal-taste/style 'light)
     (run-hooks 'ohai-appearance/light-hook)))))

;; Maximise the Emacs frame if that's how you like it.
(if (equal ohai-personal-taste/window-state 'maximised)
    (set-frame-parameter nil 'fullscreen 'maximized))

;; Don't defer screen updates when performing operations.
(setq redisplay-dont-pause t)

;; When not in a terminal, configure a few window system specific things.
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

;; Show line numbers in buffers.
(global-linum-mode t)
(setq linum-format (if (not window-system) "%4d " "%4d"))

;; Highlight the line number of the current line.
(use-package hlinum
  :config
  (hlinum-activate))

;; Show column numbers in modeline.
(setq column-number-mode t)

;; Show current function in modeline.
(which-function-mode)

;; Ensure linum-mode is disabled in certain major modes.
(setq linum-disabled-modes
      '(term-mode slime-repl-mode magit-status-mode help-mode nrepl-mode
        mu4e-main-mode mu4e-headers-mode mu4e-view-mode
        mu4e-compose-mode))
(defun linum-on ()
  (unless (or (minibufferp) (member major-mode linum-disabled-modes))
    (linum-mode 1)))

;; Highlight matching braces.
(show-paren-mode 1)

;; Engage Nyan Cat!
(use-package nyan-mode
  :config
  (nyan-mode 1)
  (setq nyan-bar-length 16
        nyan-wavy-trail t))

;; Unclutter the modeline
(use-package diminish)


(eval-after-load "eldoc" '(diminish 'eldoc-mode))
(eval-after-load "autopair" '(diminish 'autopair-mode))
(eval-after-load "abbrev" '(diminish 'abbrev-mode))
(eval-after-load "js2-highlight-vars" '(diminish 'js2-highlight-vars-mode))
(eval-after-load "mmm-mode" '(diminish 'mmm-mode))
(eval-after-load "skewer-html" '(diminish 'skewer-html-mode))
(eval-after-load "skewer-mode" '(diminish 'skewer-mode))
(eval-after-load "auto-indent-mode" '(diminish 'auto-indent-minor-mode))
;; (eval-after-load "subword" '(diminish 'subword-mode))
(eval-after-load "cider" '(diminish 'cider-mode))
(eval-after-load "smartparens" '(diminish 'smartparens-mode))

(eval-after-load "js2-mode"
  '(defadvice js2-mode (after js2-rename-modeline activate)
     (setq mode-name "JS+")))
(eval-after-load "clojure-mode"
  '(defadvice clojure-mode (after clj-rename-modeline activate)
     (setq mode-name "Clj")))
(eval-after-load "typescript"
  '(defadvice typescript-mode (after typescript-rename-modeline activate)
     (setq mode-name "TS")))
(eval-after-load "nxhtml-mode"
  '(defadvice nxhtml-mode (after nxhtml-rename-modeline activate)
     (setq mode-name "HTML")))
(eval-after-load "js"
  '(defadvice js-mode (after js-rename-modeline activate)
     (setq mode-name "JS")))
(defadvice emacs-lisp-mode (after elisp-rename-modeline activate)
  (setq mode-name "ELisp"))

;; Handle ANSI colours in compile buffer output.
;; From https://gist.github.com/jwiegley/8ae7145ba5ce64250a05
(defun compilation-ansi-color-process-output ()
  (ansi-color-process-output nil)
  (set (make-local-variable 'comint-last-output-start)
       (point-marker)))
(add-hook 'compilation-filter-hook #'compilation-ansi-color-process-output)

;; Install the colour scheme according to personal taste.
(defun ohai-appearance/apply-style ()
  (interactive)
  (cond
   ((equal ohai-personal-taste/style 'dark)
    (ohai-appearance/dark))
   ((equal ohai-personal-taste/style 'light)
    (ohai-appearance/light))))

(ohai-appearance/apply-style)



(provide 'ohai-appearance)
;;; ohai-appearance.el ends here
