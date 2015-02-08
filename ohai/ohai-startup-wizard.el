;;; -*- lexical-binding: t -*-
;;; ohai-startup-wizard.el --- Ask questions and configure on first run.

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

(require 'ohai-personal-taste)

(defun ohai-startup-wizard/get-window-state ()
  (x-popup-dialog
   t '("Normal or maximised window?"
       ("Normal" . normal)
       ("Maximised" . maximised))))

(defun ohai-startup-wizard/get-style ()
  (x-popup-dialog
   t '("Light or dark background?"
       ("Light" . light)
       ("Dark" . dark))))

(defun ohai-startup-wizard/get-splash ()
  (x-popup-dialog
   t '("What is your favourite animal?

We will put it on your splash screen for you, a fresh picture
every day, because your Emacs loves you.

This won't work if you're running Emacs in a terminal. Sorry.
Don't do that unless you really have to."
       ("Puppies" . emergency-puppy)
       ("The noble otter" . daily-otter)
       ("I hate animals" . nil))))

(defun ohai-startup-wizard/get-paredit ()
  (x-popup-dialog
   t '("Do you want to enable Paredit style editing for
the Emacs Lisp mode?

This will add a slight learning curve to your Elisp
journey, but trust me, it will be worth it."
       ("OK, let's paredit" . t)
       ("Not right now" . nil))))

(defun ohai-startup-wizard/get-training-wheels ()
  (x-popup-dialog
   t '("Would you like to get rid of the toolbar,
menu bar and scrollbar?

This is how adult emacsen roll, but you
might want to get comfortable with being
emacs before you assume your final form.

This is OK too; no pressure yet."
       ("I'm ready" . nil)
       ("Wait, I'm scared" . t))))

(defun ohai-startup-wizard ()
  (interactive)

  (x-popup-dialog
   t '("Welcome to the Church of Emacs, my child!

This is the first time you've run it, so let's
start off by asking you some basic questions
about how you like to emacs.

If you change your mind about any of these
decisions, you can re-run this wizard with
`M-x ohai-startup-wizard` (that is Alt+X
ohai-startup-wizard <enter> to non-native
speakers)."
       ("I am ready to emacs" . t)) t)

  (customize-save-variable
   'ohai-personal-taste/window-state
   (ohai-startup-wizard/get-window-state))

  (customize-save-variable
   'ohai-personal-taste/style
   (ohai-startup-wizard/get-style))

  (customize-save-variable
   'ohai-personal-taste/splash
   (ohai-startup-wizard/get-splash))

  (customize-save-variable
   'ohai-personal-taste/paredit
   (ohai-startup-wizard/get-paredit))

  (customize-save-variable
   'ohai-personal-taste/training-wheels
   (ohai-startup-wizard/get-training-wheels))

  (customize-save-variable 'ohai-personal-taste/run-wizard nil)
  (setq ohai/wizard-did-run t)
  (ohai/select-modules))

(when ohai-personal-taste/run-wizard
  (ohai-startup-wizard))



(provide 'ohai-startup-wizard)
