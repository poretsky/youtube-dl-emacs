;;; youtube-dl-w3m.el --- Provides w3m integration -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Igor B. Poretsky <poretsky@mlbox.ru>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.


;;; Commentary:

;; This module provides w3m browser integration.

;;; Code:

(require 'cl-lib)
(cl-eval-when (load)
  (require 'youtube-dl)
  (require 'w3m))

(declare-function youtube-dl "youtube-dl" (url &rest args))
(declare-function youtube-dl-list "youtube-dl" (&optional position))
(declare-function youtube-dl--request-immediate "youtube-dl")
(declare-function youtube-dl-playable-p "youtube-dl" (url))
(declare-function youtube-dl-play "youtube-dl-play" (url))
(declare-function youtube-dl-view "youtube-dl-view" (url))
(declare-function w3m-view-this-url "w3m")

;;;###autoload
(defgroup youtube-dl-w3m ()
  "W3m browser integration related options."
  :group 'youtube-dl)

(defcustom youtube-dl-w3m-downloadable-urls
  '("https?://\\(?:www\\.\\)?youtube\\.com/.+"
    "https?://\\(?:www\\.\\)?\\(?:disk\\.yandex\\.ru\\|yadi\\.sk\\)/d/.+")
  "Additional patterns that match to downloadable URLs."
  :group 'youtube-dl-w3m
  :type '(repeat regexp))

(defcustom youtube-dl-w3m-auto-play 'preview
  "Wether to start playback from a link when visiting if it is
distinguished as directly playable. If nil, it is disabled.
`always' means to start playback on visiting links whenever it seems
reasonable without asking. `preview' means to retrieve and show
a description page. Any other value means to ask for each
link that is detected as directly playable."
  :group 'youtube-dl-w3m
  :type '(choice (const :tag "Yes" always)
                 (const :tag "No" nil)
                 (const :tag "Preview" preview)
                 (const :tag "Ask" t)))

(defcustom youtube-dl-w3m-auto-download t
  "Wether to schedule download for a link when visiting if it is
distinguished as downloadable. If nil, it is disabled.
`always' means to schedule download on visiting links whenever it
seems reasonable without asking. Any other value means to ask for each
link that is detected as downloadable. Automatic downloads are always
started as paused."
  :group 'youtube-dl-w3m
  :type '(choice (const :tag "Yes" always)
                 (const :tag "No" nil)
                 (const :tag "Ask" t)))

(defun youtube-dl-w3m--downloadable-p (url)
  "Test given URL if it is downloadable."
  (or (youtube-dl-playable-p url)
      (let ((patterns youtube-dl-w3m-downloadable-urls)
            (matched nil))
        (while (and patterns (not matched))
          (if (string-match (car patterns) url)
              (setq matched t)
            (setq patterns (cdr patterns))))
        matched)))

(defun youtube-dl-w3m--current-anchor ()
  "Get an URL from current anchor if any."
  (get-text-property (point) 'w3m-href-anchor))

(defun youtube-dl-w3m--guess-url ()
  "Guess an URL for operation or request it from user."
  (cl-declare (special w3m-current-url))
  (or (youtube-dl-w3m--current-anchor)
      (read-from-minibuffer "URL: "
                            (or (thing-at-point 'url)
                                w3m-current-url))))

(defun youtube-dl-w3m--request-args ()
  "Interactively request download arguments from user."
  (list (youtube-dl-w3m--guess-url)
        (youtube-dl--request-immediate)
        current-prefix-arg))

(defun youtube-dl-w3m (url &optional immediate reverse audio-only)
  "Schedules an URL for youtube-dl download. Being called interactively
uses an anchor at point. If no anchor exists at point, requests
an URL from user suggesting reasonable default. If second argument
is non-nil, download starts immediately. Otherwise it is queued as
paused. If URL points to a playlist and third argument is non-nil
then playlist will be reversed. If third argument is non-nil, only
audio content is downloaded.

On completion download list is shown with point positioned
at the newly added items."
  (interactive (youtube-dl-w3m--request-args))
  (cl-declare (special youtube-dl-items))
  (let ((position (length youtube-dl-items)))
    (youtube-dl url immediate reverse
                :extract-audio audio-only)
    (youtube-dl-list position)))

(defun youtube-dl-w3m-audio (url &optional immediate reverse)
  "Schedules an URL for youtube-dl download audio content. Being called
interactively uses an anchor at point. If no anchor exists at point,
requests an URL from user suggesting reasonable default. If second
argument is non-nil, download starts immediately. Otherwise it is
queued as paused. If URL points to a playlist and third argument
is non-nil then playlist will be reversed.

On completion download list is shown with point positioned
at the newly added items."
  (interactive (youtube-dl-w3m--request-args))
  (youtube-dl-w3m url immediate reverse t))

(defun youtube-dl-w3m-play (url)
  "Starts playback from specified URL. Being called interactively uses
an anchor at point. If no anchor exists at point, requests an URL from
user suggesting reasonable default."
  (interactive (list (youtube-dl-w3m--guess-url)))
  (youtube-dl-play url))

(defvar youtube-dl-w3m-menu
  (let ((menu (make-sparse-keymap "YouTube download and playback")))
    (define-key menu [schedule]
      '(menu-item "Submit download" youtube-dl-w3m))
    (define-key menu [schedule-audio]
      '(menu-item "Submit download audio" youtube-dl-w3m-audio))
    (define-key menu [play]
      '(menu-item "Play video clip" youtube-dl-w3m-play))
    (define-key menu [list]
      '(menu-item "Show download queue" youtube-dl-list))
    menu)
  "Actions menu for use in w3m.")

;;;###autoload
(defun youtube-dl-w3m-menu-popup ()
  "Pops up the actions menu."
  (interactive)
  (tmm-prompt youtube-dl-w3m-menu))

;;;###autoload
(defun youtube-dl-w3m-dispatch ()
  "Dispatches link visiting operation depending on the link nature.
Uses `w3m-view-this-url' as a fallback."
  (interactive)
  (let ((url (youtube-dl-w3m--current-anchor)))
    (cond
     ((and youtube-dl-w3m-auto-play
           (not current-prefix-arg)
           (youtube-dl-playable-p url)
           (or (eq youtube-dl-w3m-auto-play 'always)
               (eq youtube-dl-w3m-auto-play 'preview)
               (y-or-n-p "Start playback? ")))
      (if (eq youtube-dl-w3m-auto-play 'preview)
          (youtube-dl-view url)
        (youtube-dl-play url)))
     ((and youtube-dl-w3m-auto-download
           (not current-prefix-arg)
           (youtube-dl-w3m--downloadable-p url)
           (or (eq youtube-dl-w3m-auto-download 'always)
               (y-or-n-p "Schedule download? ")))
      (youtube-dl-w3m url))
     (t (call-interactively 'w3m-view-this-url)))))

(provide 'youtube-dl-w3m)

;;; youtube-dl-w3m.el ends here
