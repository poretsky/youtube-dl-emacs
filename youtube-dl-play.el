;;; youtube-dl-play.el --- plays video clips with mpv player and youtube-dl -*- lexical-binding: t; -*-

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

;; This module plays video clips with mpv player and youtube-dl
;; command line program, which serves as its back end.

;; The `youtube-dl-play' command starts playback of an item
;; under point in the youtube-dl download list or from specified URL.

;;; Code:

(require 'cl-lib)
(cl-eval-when (load)
  (require 'youtube-dl))

(declare-function youtube-dl-thing "youtube-dl")
(declare-function youtube-dl-item-p "youtube-dl" (item))
(declare-function youtube-dl-item-url "youtube-dl" (item))

;;;###autoload
(defgroup youtube-dl-play ()
  "YouTube video playback control options."
  :group 'youtube-dl)

(defcustom youtube-dl-play-program "mpv"
  "The name of the program invoked for playing YouTube videos."
  :group 'youtube-dl-play
  :type 'string)

(defcustom youtube-dl-play-fullscreen "--fs"
  "Fullscreen playback mode option."
  :group 'youtube-dl-play
  :type '(choice (const :tag "according to player configuration" nil)
                 (const :tag "yes" "--fs")
                 (const :tag "no" "--no-fs")))

(defcustom youtube-dl-play-format nil
  "Playback format. It can be specified explicitly when default does not fit."
  :group 'youtube-dl-play
  :type '(choice (const :tag "default" nil)
                 (const "best")
                 (const "worst")
                 (const "mp4")
                 (const "webm")))

(defvar youtube-dl-play-process nil
  "Youtube video playback process.")

(defun youtube-dl-play--sentinel (process event)
  "YouTube video playback process events handler."
  (unless (process-live-p process)
    (setf youtube-dl-play-process nil))
  (message "Process %s %s" (process-name process) event))

;;;###autoload
(defun youtube-dl-play-stop ()
  "Stops currently playing youtube video if any."
  (interactive)
  (when (and (processp youtube-dl-play-process)
             (process-live-p youtube-dl-play-process))
    (kill-process youtube-dl-play-process)))

;;;###autoload
(defun youtube-dl-play (thing &optional start)
  "Plays video from specified URL or, being invoked interactively
in the download list, from an item under point. Optional second
argument, if non-nil, is treated as start time specification string."
  (interactive (youtube-dl-thing))
  (youtube-dl-play-stop)
  (let* ((url
          (if (youtube-dl-item-p thing)
              (youtube-dl-item-url thing)
            thing))
         (proc
          (apply #'start-process "mpv" nil youtube-dl-play-program
                 "--profile=pseudo-gui" "--ytdl"
                 (nconc (list youtube-dl-play-fullscreen)
                        (when youtube-dl-play-format
                          `("--ytdl-format" ,youtube-dl-play-format))
                        (when start
                          `("--start" ,start))
                        `(,url)))))
    (setf youtube-dl-play-process proc)
    (set-process-sentinel proc #'youtube-dl-play--sentinel)))

(provide 'youtube-dl-play)

;;; youtube-dl-play.el ends here
