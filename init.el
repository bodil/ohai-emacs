;;; -*- lexical-binding: t -*-
;;; init.el --- This is where all emacs start.

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

;; Do an Emacs version check before going any further.
(when (or (< emacs-major-version 24)
          (and (= emacs-major-version 24) (< emacs-minor-version 4)))
  (x-popup-dialog
   t `(,(format "Sorry, you need GNU Emacs version 24.4 or higher
to run Ohai Emacs.

Your installed Emacs reports:
%s" (emacs-version))
       ("OK :(" . t)))
  (save-buffers-kill-emacs t))

;; We start off by telling Emacs where we intend to keep things, and some
;; other basic system setup.

;; Initialise the package system first of all.
(package-initialize)

;; Skip the default splash screen.
(setq inhibit-startup-message t)

;; Figure out the current hostname.
(setq hostname (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" (with-output-to-string (call-process "hostname" nil standard-output))))

;; Figure out the path to our .emacs.d by getting the path part of the
;; current file (`init.el`).
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) (file-chase-links load-file-name))))

;; We need to tell Emacs where to find Ohai Emacs's library packages.
(add-to-list 'load-path (concat dotfiles-dir "ohai"))

;; We'll keep the various parts of our configuration in the `modules` subfolder
;; of our .emacs.d. We'll need to add that to the system load path so Emacs can
;; find our modules when we ask for them.
(add-to-list 'load-path (concat dotfiles-dir "modules"))

;; Define where we want to keep `loaddefs.el` (our autoload declarations) and
;; `custom.el` (our user settings file).
(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq custom-file (concat dotfiles-dir "custom.el"))

;; Load the user settings from `custom.el`.
(load custom-file 'noerror)

;; Load the Ohai Emacs fundamentals.
(require 'ohai-lib)
(require 'ohai-package)
(require 'ohai-module-index)
(require 'ohai-module-selector)
(require 'ohai-update)
(require 'ohai-personal-taste)
(require 'ohai-startup-wizard)
(require 'ohai-set-path)

;; Load the enabled modules.
(when (not (boundp 'ohai/wizard-did-run)) (ohai/load-modules))

;; Load the user's config, if it exists.
(load (concat dotfiles-dir "user.el") 'noerror)
