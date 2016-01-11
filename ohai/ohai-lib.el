;;; -*- lexical-binding: t -*-
;;; ohai-lib.el --- Utility functions for use elsewhere.

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

;; Ensure the New Standard Library is installed and always available.
;; f.el    - files and paths  https://github.com/rejeep/f.el
;; s.el    - strings          https://github.com/magnars/s.el
;; dash.el - lists            https://github.com/magnars/dash.el
(use-package f)
(use-package s)
(use-package dash)



(defun ohai/font-lock-replace-symbol (mode reg sym)
  "Given a major mode `mode', replace the regular expression `reg' with
the symbol `sym' when rendering."
  (font-lock-add-keywords
   mode `((,reg
           (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                     ,sym 'decompose-region)))))))



(defun ohai/exec (command)
  "Run a shell command and return its output as a string, whitespace trimmed."
  (s-trim (shell-command-to-string command)))

(defun ohai/exec-with-rc (command &rest args)
  "Run a shell command and return a list containing two values: its return
code and its whitespace trimmed output."
  (with-temp-buffer
    (list (apply 'call-process command nil (current-buffer) nil args)
          (s-trim (buffer-string)))))

(defun ohai/is-exec (command)
  "Returns true if `command' is an executable on the system search path."
  (f-executable? (s-trim (shell-command-to-string (s-concat "which " command)))))

(defun ohai/resolve-exec (command)
  "If `command' is an executable on the system search path, return its absolute path.
Otherwise, return nil."
  (-let [path (s-trim (shell-command-to-string (s-concat "which " command)))]
    (when (f-executable? path) path)))

(defun ohai/exec-if-exec (command args)
  "If `command' satisfies `ohai/is-exec', run it with `args' and return its
output as per `ohai/exec'. Otherwise, return nil."
  (when (ohai/is-exec command) (ohai/exec (s-concat command " " args))))



(defun ohai/getent (user)
  "Get the /etc/passwd entry for the user `user' as a list of strings,
or nil if there is no such user. Empty fields will be represented as nil,
as opposed to empty strings."
  (-let [ent (ohai/exec (s-concat "getent passwd " user))]
    (when (not (s-blank? ent))
      (-map (lambda (i) (if (s-blank? i) nil i))
            (s-split ":" ent)))))

(defun ohai/user-full-name ()
  "Guess the user's full name. Returns nil if no likely name could be found."
  (or (ohai/exec-if-exec "git" "config --get user.name")
      (elt (ohai/getent (getenv "USER")) 4)))

(defun ohai/user-email ()
  "Guess the user's email address. Returns nil if none could be found."
  (or (ohai/exec-if-exec "git" "config --get user.email")
      (getenv "EMAIL")))



(provide 'ohai-lib)
;;; ohai-lib.el ends here
