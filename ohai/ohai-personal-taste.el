;;; -*- lexical-binding: t -*-
;;; ohai-personal-taste.el --- Ohai Emacs preferences.

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

(defgroup ohai-emacs nil
  "Your personal taste in Ohai Emacs."
  :prefix "ohai-personal-taste/")

(defcustom ohai-personal-taste/run-wizard t
  "Should we run the Ohai Emacs startup wizard on the next startup?"
  :group 'ohai-emacs
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil)))

(defcustom ohai-personal-taste/window-state 'normal
  "Should Emacs maximise its frame on startup, or leave it alone?"
  :group 'ohai-emacs
  :type '(choice (const :tag "Normal" normal)
                 (const :tag "Maximise" maximised)))

(defcustom ohai-personal-taste/style 'light
  "Light or dark colour scheme?"
  :group 'ohai-emacs
  :type '(choice (const :tag "Light" light)
                 (const :tag "Dark" dark)))

(defcustom ohai-personal-taste/splash nil
  "What sort of image to load into the initial scratch buffer."
  :group 'ohai-emacs
  :type '(choice (const :tag "Emergency Puppy" emergency-puppy)
                 (const :tag "Daily Otter" daily-otter)
                 (const :tag "Nothing" nil)))

(defcustom ohai-personal-taste/paredit nil
  "Do you want Paredit in Emacs Lisp buffers?"
  :group 'ohai-emacs
  :type '(choice (const :tag "Yes, please" t)
                 (const :tag "I'm not ready for that" nil)))

(defcustom ohai-personal-taste/training-wheels nil
  "Would you prefer an Emacs experience without the clutter of the menu bar,
toolbar and scrollbar?"
  :group 'ohai-emacs
  :type '(choice (const :tag "Yes, please" t)
                 (const :tag "I'm not ready for that" nil)))



(provide 'ohai-personal-taste)
;;; ohai-personal-taste.el ends here
