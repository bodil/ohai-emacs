;;; -*- lexical-binding: t -*-
;;; ohai-flow.el --- JavaScript is slightly less horrid with types.

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

(require 'ohai-project)
(require 'ohai-editing)
(with-eval-after-load "projectile"
  (add-to-list 'projectile-project-root-files ".flowconfig"))

;; There's no Flow major mode, so we'll fake it by copying TypeScript's.
(use-package typescript-mode)
(define-derived-mode flow-mode typescript-mode "Flow"
  "JavaScript with Flow type checking")

;; Locate the Flow binary.
(setq ohai-flow/flow-binary (or (ohai/resolve-exec "flow") "flow"))

;; Bind keys for Flow actions.
(define-key flow-mode-map (kbd "C-c C-s") 'ohai-flow/start-server)
(define-key flow-mode-map (kbd "C-c C-t") 'ohai-flow/show-type)
(define-key flow-mode-map (kbd "C-c C-f") 'ohai-flow/suggest-types)
(define-key flow-mode-map (kbd "M-.") 'ohai-flow/jump-to-def)



;; Utility functions for Flow actions.

(defun ohai-flow/position (l c)
  (save-excursion
    (goto-char (point-min))
    (forward-line (- l 1))
    (move-to-column (- c 1))
    (point)))

(defun ohai-flow/parse-range (s)
  (-when-let (res (s-match "\\([^:]*\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\),\\([[:digit:]]+\\):\\([[:digit:]]+\\)" s))
    (cons (cadr res) (-partition 2 (-map 'string-to-number (cddr res))))))

(defun column-number-at-pos (pos)
  "column number at pos"
  (save-excursion (goto-char pos) (current-column)))

(defun ohai-flow/string-of-region ()
  "string of region"
  (if (use-region-p)
      (let ((begin (region-beginning))
            (end (region-end)))
        (format ":%d:%d,%d:%d"
                (line-number-at-pos begin)
                (column-number-at-pos begin)
                (line-number-at-pos end)
                (column-number-at-pos end)))
    ""))



;; A function for explicitly launching a Flow server.

(defun ohai-flow/start-server ()
  "Initialize flow"
  (interactive)
  (compilation-start (format "cd %s; %s start; %s status --from emacs; exit 0" (projectile-project-root) ohai-flow/flow-binary ohai-flow/flow-binary)))



;; Show the type of the symbol at point.

(defun ohai-flow/show-type ()
  "show type"
  (interactive)
  (save-some-buffers)
  (-let* ((output (shell-command-to-string
                   (format "%s type-at-pos --from emacs %s %d %d"
                           ohai-flow/flow-binary
                           (buffer-file-name)
                           (line-number-at-pos)
                           (1+ (current-column))))))
    (if (equal (s-trim output) "(unknown)")
        (progn
          (message "(unknown)")
          (popup-tip "(unknown)"))
      (-let* ((result (s-lines output))
              (type (car result))
              ((_ startp endp) (ohai-flow/parse-range (cadr result)))
              (start (apply 'ohai-flow/position startp))
              (end (apply 'ohai-flow/position endp)))
        (message type)
        (vhl/add-range start (1+ end))
        (popup-tip type :point start)))))



;; Suggest types for unannotated symbols in the buffer.

(defun ohai-flow/suggest-types ()
  "suggest types"
  ;; TODO: find a way to apply patches
  (interactive)
  (save-some-buffers)
  (let ((file (buffer-file-name))
        (region (ohai-flow/string-of-region))
        (buffer (switch-to-buffer-other-window "*Flow Suggestions*")))
    (shell-command
     (format "%s suggest --from emacs %s%s"
             ohai-flow/flow-binary
             file
             region)
     buffer)
    (diff-mode)
    (diff-auto-refine-mode)))



;; Jump to the definition of the symbol at point.

(defun ohai-flow/jump-to-def ()
  "jump to definition"
  (interactive)
  (save-some-buffers)
  (-let* ((output (s-trim
                   (shell-command-to-string
                    (format "%s get-def %s %d %d"
                            ohai-flow/flow-binary
                            (buffer-file-name)
                            (line-number-at-pos)
                            (1+ (current-column)))))))
    (if (equal output ":0:1,0:0")
        (message "No definition found.")
      (-let* (((file startp endp) (ohai-flow/parse-range output))
              (start (apply 'ohai-flow/position startp))
              (end (apply 'ohai-flow/position endp))
              (buffer (find-file file)))
        (switch-to-buffer buffer)
        (goto-char start)
        (vhl/add-range start (1+ end))))))



;; A Company autocompletion backend.

(defun ohai-flow/autocomplete (callback)
  "autocomplete"
  (when (and (boundp 'ohai-flow/autocomplete-process)
             (not (null ohai-flow/autocomplete-process)))
    (delete-process ohai-flow/autocomplete-process))
  (setq ohai-flow/autocomplete-result "")
  (setq ohai-flow/autocomplete-process nil)
  (setq ohai-flow/autocomplete-callback callback)
  (-let* ((file (buffer-file-name))
          (line (line-number-at-pos))
          (col (current-column))
          (proc (start-process "flow-autocomplete" "*Flow Autocomplete*"
                               ohai-flow/flow-binary
                               "autocomplete"

                               file
                               (number-to-string line)
                               (number-to-string (1+ col)))))
    (setq ohai-flow/autocomplete-process proc)
    (set-process-sentinel proc
                          (lambda (p e)
                            (funcall ohai-flow/autocomplete-callback
                                     (-map
                                      (lambda (l) (propertize (cadr l) 'meta (caddr l)))
                                      (-remove
                                       'null
                                       (-map
                                        (lambda (s) (s-match "\\([^ ]*\\) \\(.*\\)" s))
                                        (-filter
                                         (lambda (s) (not (equal s "")))
                                         (s-lines ohai-flow/autocomplete-result))))))))
    (set-process-filter proc
                        (lambda (p s)
                          (setq ohai-flow/autocomplete-result
                                (concat ohai-flow/autocomplete-result s))))
    (process-send-string proc (buffer-string))
    (process-send-eof proc)))

(defun ohai-flow/autocomplete-meta (v)
  (-let ((meta (get-text-property 0 'meta v)))
    (if (not (equal meta ""))
        (concat v " :: " meta)
      v)))

(defun ohai-flow/autocomplete-annotation (v)
  (s-truncate 30 (get-text-property 0 'meta v)))

(defun company-flow (command &optional arg)
  "Flow backend for company-mode."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-flow))
    (prefix (when (eq major-mode 'flow-mode)
              (company-grab-symbol)))
    (candidates (cons :async 'ohai-flow/autocomplete))
    (annotation (ohai-flow/autocomplete-annotation arg))
    (meta (ohai-flow/autocomplete-meta arg))))

(with-eval-after-load "company"
  (add-to-list 'company-backends 'company-flow))



;; Define a Flow checker for Flycheck.

(defun ohai-flow/format-message (l)
  (-let [(&alist 'start start 'end end 'line line 'endline endline
                 'path path 'level level 'descr descr) l]
    (if (eq path "")
        descr
      (format "%s:%s:%s,%s:%s: %s"
              path line start endline end
              descr))))

(defun ohai-flow/parse-errors (msg checker buffer)
  (-let [json-array-type 'list]
    (-let [o (json-read-from-string msg)]
      (-map
       (lambda (e)
         (-let [errs (cdr (car e))]
           (-let [(&alist 'start start 'end end 'line line 'endline endline
                          'path path 'level level 'descr descr) (car errs)]
             (flycheck-error-new
              :line line
              :column start
              :level 'error
              :message (s-join "\n" (-map #'ohai-flow/format-message errs))
              :filename (f-relative
                         path (f-dirname (file-truename (buffer-file-name))))
              :buffer buffer
              :checker checker))))
       (cdr (assoc 'errors o))))))

(with-eval-after-load "flycheck"
  (require 'json)
  (require 'ohai-lib)
  (require 'ohai-flycheck)
  (flycheck-define-command-checker 'javascript-flow
    "Static type checking using Flow."
    :command (list ohai-flow/flow-binary "--json" 'source-original)
    :error-parser 'ohai-flow/parse-errors
    :modes 'flow-mode)
  (add-to-list 'flycheck-checkers 'javascript-flow))



;; Highlight affiliated errors.
(defun ohai-flow/highlight-affiliated (errors)
  (-let* ((locs (-remove
                 'null
                 (-map
                  'ohai-flow/parse-range
                  (-mapcat
                   (lambda (err) (s-lines (flycheck-error-message err)))
                   errors)))))
    (-each locs
      (lambda (loc)
        (-let* (((file startp endp) loc)
                (start (apply 'ohai-flow/position startp))
                (end (apply 'ohai-flow/position endp)))
          (when (equal (buffer-file-name) file)
            (vhl/add-range start (+ end 1) nil 'flycheck-error)))))))

(add-hook 'next-error-hook
          (lambda ()
            (ohai-flow/highlight-affiliated (flycheck-overlay-errors-at (point)))))
(setq flycheck-display-errors-function
      (lambda (errors)
        (ohai-flow/highlight-affiliated errors)
        (flycheck-display-error-messages errors)))



;; Tell Flycheck we want to eslint Flow files.
(with-eval-after-load "flycheck"
  (flycheck-add-mode 'javascript-eslint 'flow-mode)
  (setq flycheck-javascript-eslint-executable (or (ohai/resolve-exec "eslint") "eslint")))

;; Use conf-mode when editing `.flowconfig' files.
(add-to-list 'auto-mode-alist '("\\.flowconfig\\'" . conf-mode))



(provide 'ohai-flow)
;;; ohai-flow.el ends here
