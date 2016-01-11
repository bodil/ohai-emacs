;;; -*- lexical-binding: t -*-
;;; ohai-splash.el --- Customise the scratch buffer.

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

(setq ohai-splash/sources
      (cond
       ((equal ohai-personal-taste/splash 'emergency-puppy)
        '(ohai-splash/emergency-puppy))
       ((equal ohai-personal-taste/splash 'daily-otter)
        '(ohai-splash/daily-otter))))

(use-package dash)
(use-package s)
(use-package web)


(defun ohai-splash/load-and-map (url map-fn cb)
  (web-http-get
   (lambda (con header data)
     (funcall cb (funcall map-fn data header)))
   :url url))

(defun ohai-splash/load-and-match (url regex format-str cb)
  (ohai-splash/load-and-map
   url
   (lambda (data header)
     (-when-let (m (s-match regex data))
         (s-format format-str 'elt m)))
   cb))

(defun ohai-splash/pick-first (fetchers cb)
  (-if-let (fetcher (car fetchers))
      (if (stringp fetcher) (funcall cb fetcher)
        (funcall fetcher
                 (lambda (url)
                   (if url (funcall cb url)
                     (ohai-splash/pick-first (cdr fetchers) cb)))))
    (funcall cb nil)))

(defun ohai-splash/load-image (url cb)
  (web-http-get
   (lambda (con header data)
     (funcall cb (create-image (string-to-unibyte data) nil t)))
   :url url))

(defun ohai-splash/dilbert (cb)
  (ohai-splash/load-and-match "http://dilbert.com/"
                        "/dyn/str_strip/[0-9/]+\.strip\\(\.sunday\\)?\.gif"
                        "http://dilbert.com$0" cb))

(defun ohai-splash/imgur (search-term)
  (lambda (cb)
    (ohai-splash/load-and-match
     (s-concat "http://imgur.com/search/score/day?q_size_px=med&q_size_mpx=med&q="
               (url-hexify-string search-term))
     "<div id=\"\\([0-9a-zA-Z]+\\)\" class=\"post\""
     "http://i.imgur.com/$1.jpg" cb)))

(defun ohai-splash/placepuppy (cb)
  (cb "http://placepuppy.it/600/400"))

(defun ohai-splash/emergency-puppy (cb)
  (ohai-splash/load-and-match
   "https://twitter.com/EmergencyPuppy/media"
   "data-image-url=\"\\([^\"]*\\)\""
   "$1" cb))

(defun ohai-splash/daily-otter (cb)
  (ohai-splash/load-and-match
   "http://dailyotter.org/"
   "<img class=\"aligncenter wp-image-.*src=\"\\([^\"]*\\)\""
   "$1" cb))

(defun ohai-splash/run (sources cb)
  (ohai-splash/pick-first
   (or sources ohai-splash/sources)
   (lambda (url)
     (ohai-splash/load-image url cb))))

(defun ohai-splash/inject-help-text ()
  (with-current-buffer (get-buffer "*scratch*")
    (end-of-buffer)
    (delete-region 1 (point))
    (insert ";; Blessed art thou, who hath come to the One True Editor.

;; If you are currently experiencing panic and need to get out, you can exit
;; by typing C-x C-c. That is, press Control+X, then Control+C.

;; If you are new to GNU Emacs, you should start the interactive tutorial by
;; typing C-h t. That is, Control+H, then the letter T.

;; This is your scratch buffer. You can enter temporary notes here, or you
;; can write Emacs Lisp commands and evaluate them (C-x C-e) from this buffer.
;; To get straight to work, press C-x C-f to open or create a file.

")))

(defun ohai-splash/inject-spinner ()
  (with-current-buffer (get-buffer "*scratch*")
    (insert "Downloading internets... ")))

(defun ohai-splash/inject-picture (img)
  (with-current-buffer (get-buffer "*scratch*")
    (end-of-buffer)
    (move-beginning-of-line nil)
    (kill-line)
    (insert-image img "ohai")
    (move-beginning-of-line nil)
    (mark-word)
    (delete-other-windows)))

(defun ohai-splash/go ()
  (ohai-splash/inject-help-text)
  (when (and (online?) ohai-splash/sources)
    (ohai-splash/inject-spinner)
    (ohai-splash/run nil (lambda (img) (ohai-splash/inject-picture img))))
  t)

(when window-system
  (setq initial-buffer-choice 'ohai-splash/go))

;; A hack here to force the splash screen after the first run wizard's
;; module selection, as `initial-buffer-choice' will already have run.
(when (boundp 'ohai/wizard-did-run)
  (ohai-splash/go))


(provide 'ohai-splash)
