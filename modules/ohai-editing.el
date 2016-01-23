;;; -*- lexical-binding: t -*-
;;; ohai-editing.el --- Configure your editing style.

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

;; Multiple cursors!
;; Use <insert> to place a cursor on the next match for the current selection.
;; Use S-<insert> to place one on the previous match.
;; Use C-' to use extended mark mode, giving you more control.
;; Use C-" to place cursors on all matches.
;; Select a region and C-M-' to place cursors on each line of the selection.
;; Bonus: <insert> key no longer activates overwrite mode.
;; What is that thing for anyway?
(use-package multiple-cursors
  :commands multiple-cursors-mode
  :config
  ;; MC has `mc-hide-unmatched-lines-mode' bound to C-', which interferes
  ;; with our ability to add more cursors, so we'll just clear the binding.
  ;; TODO: add `mc-hide-unmatched-lines-mode' back somewhere else?
  (bind-keys :map mc/keymap
             ("C-'" . nil))
  :bind (("<insert>" . mc/mark-next-like-this)
	 ("S-<insert>" . mc/mark-previous-like-this)
	 ("C-'" . mc/mark-more-like-this-extended)
	 ("C-\"" . mc/mark-all-like-this-dwim)
	 ("C-M-'" . mc/edit-lines)))

;; Use C-= to select the innermost logical unit your cursor is on.
;; Keep hitting C-= to expand it to the next logical unit.
;; Protip: this goes really well with multiple cursors.
(use-package expand-region
  :commands er/expand-region
  :bind ("C-=" . er/expand-region))

;; Remap join-line to M-j where it's easier to get to.
;; join-line will join the line you're on with the line above it
;; in a reasonable manner for the type of file you're editing.
(global-set-key (kbd "M-j") 'join-line)

;; Hit C-c <tab> to auto-indent the entire buffer you're in.
(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))
(global-set-key (kbd "C-c <tab>") 'indent-buffer)

;; Automatically insert matching braces and do other clever
;; things pertaining to braces and such.
(electric-pair-mode 1)

;; Duplicate start of line or region with C-M-<end>.
;; From http://www.emacswiki.org/emacs/DuplicateStartOfLineOrRegion
(defun duplicate-start-of-line-or-region ()
  (interactive)
  (if mark-active
      (duplicate-region)
    (duplicate-start-of-line)))
(defun duplicate-start-of-line ()
  (if (bolp)
      (progn
        (end-of-line)
        (duplicate-start-of-line)
        (beginning-of-line))
    (let ((text (buffer-substring (point)
                                  (beginning-of-thing 'line))))
      (forward-line)
      (push-mark)
      (insert text)
      (open-line 1))))
(defun duplicate-region ()
  (let* ((end (region-end))
         (text (buffer-substring (region-beginning) end)))
    (goto-char end)
    (insert text)
    (push-mark end)
    (setq deactivate-mark nil)
    (exchange-point-and-mark)))
(global-set-key (kbd "C-M-<end>") 'duplicate-start-of-line-or-region)

;; Hack for setting a fixed wrap column in visual-line-mode.
(defun set-visual-wrap-column (new-wrap-column &optional buffer)
  "Force visual line wrap at NEW-WRAP-COLUMN in BUFFER (defaults
    to current buffer) by setting the right-hand margin on every
    window that displays BUFFER.  A value of NIL or 0 for
    NEW-WRAP-COLUMN disables this behavior."
  (interactive (list (read-number "New visual wrap column, 0 to disable: " (or visual-wrap-column fill-column 0))))
  (if (and (numberp new-wrap-column)
           (zerop new-wrap-column))
      (setq new-wrap-column nil))
  (with-current-buffer (or buffer (current-buffer))
    (visual-line-mode t)
    (set (make-local-variable 'visual-wrap-column) new-wrap-column)
    (add-hook 'window-configuration-change-hook 'update-visual-wrap-column nil t)
    (let ((windows (get-buffer-window-list)))
      (while windows
        (when (window-live-p (car windows))
          (with-selected-window (car windows)
            (update-visual-wrap-column)))
        (setq windows (cdr windows))))))
(defun update-visual-wrap-column ()
  (if (not visual-wrap-column)
      (set-window-margins nil nil)
    (let* ((current-margins (window-margins))
           (right-margin (or (cdr current-margins) 0))
           (current-width (window-width))
           (current-available (+ current-width right-margin)))
      (if (<= current-available visual-wrap-column)
          (set-window-margins nil (car current-margins))
        (set-window-margins nil (car current-margins)
                            (- current-available visual-wrap-column))))))

;; A function for easily editing a file as root through TRAMP.
(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (if (fboundp 'helm-read-file-name)
                             (helm-read-file-name "File: ")
                           (ido-read-file-name "File: "))))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; A key for intelligently shrinking whitespace.
;; See https://github.com/jcpetkovich/shrink-whitespace.el for details.
(use-package shrink-whitespace
  :commands shrink-whitespace
  :bind ("C-c DEL" . shrink-whitespace))

;; Highlight changed areas with certain operations, such as undo, kill, yank.
(use-package volatile-highlights
  :commands volatile-highlights-mode
  :config
  (volatile-highlights-mode t)
  :diminish volatile-highlights-mode)

(provide 'ohai-editing)
