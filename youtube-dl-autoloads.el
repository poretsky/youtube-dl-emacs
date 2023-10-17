;;; youtube-dl-autoloads.el --- youtube-dl autoload bindings -*- lexical-binding: t; -*-

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

;; This file should be loaded from startup to have access
;; to the youtube-dl functionality.

;;; Code:

(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

(require 'youtube-dl-loaddefs)

;; Global menu bindings:
(define-key global-map [menu-bar tools youtube-dl]
  (cons "YouTube download and playback"
        (let ((youtube-dl-menu (make-sparse-keymap "youtube-dl")))
          (define-key youtube-dl-menu [customize]
            '("Customize" . youtube-dl-customize))
          (define-key youtube-dl-menu [schedule]
            '("Submit download" . youtube-dl))
          (define-key youtube-dl-menu [schedule-audio]
            '("Submit download audio" . youtube-dl-audio))
          (define-key youtube-dl-menu [play]
            '("Play video clip" . youtube-dl-play))
          (define-key youtube-dl-menu [view]
            '("View video clip info" . youtube-dl-view))
          (define-key youtube-dl-menu [list]
            '("Show download queue" . youtube-dl-list))
          youtube-dl-menu)))

;; Key bindings for w3m:
(eval-after-load 'w3m
  '(progn
     (define-key w3m-mode-map "y" #'youtube-dl-w3m-menu-popup)
     (define-key w3m-mode-map "Y" #'youtube-dl-w3m-invidious)
     (define-key w3m-mode-map "\r" #'youtube-dl-w3m-dispatch)))
