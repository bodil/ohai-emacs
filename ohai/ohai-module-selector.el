;;; -*- lexical-binding: t -*-
;;; ohai-module-selector.el --- Interface for choosing Ohai Emacs modules.

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

(require 'ohai-lib)
(require 'widget)
(require 'cus-edit)

(defun ohai/select-modules ()
  "Select the modules Ohai Emacs should load on startup."
  (interactive)

  (switch-to-buffer "*Ohai Emacs Modules*")
  (kill-all-local-variables)
  (-let [inhibit-read-only t] (erase-buffer))
  (remove-overlays)

  (setq-local selected-modules ohai/modules)

  (-let ((save-settings
          (lambda (&rest ignore)
            (interactive)
            (customize-save-variable 'ohai/modules selected-modules)
            (package-refresh-contents)
            (ohai/load-modules)
            (kill-buffer))))

    (widget-insert (propertize "Ohai Emacs Modules" 'face 'custom-group-tag))
    (widget-insert "\n")

    (widget-insert "
This menu allows you to select feature modules for your Ohai Emacs.

Navigate between checkboxes using <tab> and S-<tab>, or use the cursor
keys to move around. Hit <return> to toggle checkboxes, or to press the
buttons. When you're done, press the `Save' or `Cancel' buttons, or just
save the buffer (C-x C-s).

You can also use your mouse, but you must resist the urge to do this.
An Emacs Master does not use mice.
")

    (widget-insert "\n  ")
    (widget-create 'push-button
                   :tag "Save"
                   :notify save-settings)
    (widget-insert "  ")
    (widget-create 'push-button :tag "Cancel"
                   :notify (lambda (&rest ignore) (kill-buffer)))

    (widget-insert "\n\n  ")
    (apply 'widget-create 'checklist
           :indent 2
           :greedy t
           :value selected-modules
           :notify (lambda (this &rest ignore)
                     (setq-local selected-modules (widget-value this)))
           (-map (lambda (mod)
                   (-let [(sym desc) mod]
                     `(item :tag ,(s-concat (s-pad-right 24 " " (symbol-name sym)))
                            :doc ,desc
                            :value ,sym
                            :format "%t %d")))
                 ohai/available-modules))

    (widget-insert "\n")

    (use-local-map (copy-keymap widget-keymap))
    (local-set-key (kbd "C-x C-s") save-settings)

    (widget-setup)
    (widget-forward 1)))



(provide 'ohai-module-selector)
;;; ohai-module-selector.el ends here
