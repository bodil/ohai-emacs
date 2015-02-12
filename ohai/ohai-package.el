;;; -*- lexical-binding: t -*-
;;; ohai-package.el --- Package system configuration.

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

;; `(online?)` is a function that tries to detect whether you are online.
;; We want to refresh our package list on Emacs start if we are.
(require 'cl)
(defun online? ()
  (if (and (functionp 'network-interface-list)
           (network-interface-list))
      (some (lambda (iface) (unless (equal "lo" (car iface))
                         (member 'up (first (last (network-interface-info
                                                   (car iface)))))))
            (network-interface-list))
    t))

;; Emacs comes with a package manager for installing more features.
;; The default package repository doesn't contain much, so we tell it
;; to use a few more: MELPA and Marmalade.
(setq package-user-dir (concat dotfiles-dir "elpa"))
(require 'package)
(dolist (source '(("melpa" . "http://melpa.org/packages/")
                  ("marmalade" . "http://marmalade-repo.org/packages/")))
  (add-to-list 'package-archives source t))

;; To get the package manager going, we invoke its initialise function.
(package-initialize)

;; If we're online, we attempt to fetch the package directories if
;; we don't have a local copy already. This lets us start installing
;; packages right away from a clean install.
(when (online?)
  (unless package-archive-contents (package-refresh-contents)))

;; We're going to try to declare the packages each feature needs as we
;; define it. To do this, we define a function `(package-require)`
;; which will fetch and install a package from the repositories if it
;; isn't already installed. Eg. to ensure the hypothetical package
;; `ponies` is installed, you'd call `(package-require 'ponies)`.
(defun package-require (pkg)
  "Install a package only if it's not already installed."
  (when (not (package-installed-p pkg))
    (package-install pkg)))



(provide 'ohai-package)
