;;; youtube-dl-view.el --- shows video clip info in a separate buffer -*- lexical-binding: t; -*-

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

;; This module shows video clip info in a separate buffer. In this
;; buffer you can navigate and activate highlighted references.

;; The `youtube-dl-view-url' command retrieves video description
;; from specified URL and shows it.

;;; Code:

(require 'cl-lib)
(cl-eval-when '(load)
  (require 'youtube-dl))

(declare-function youtube-dl--pointed-item "youtube-dl")
(declare-function youtube-dl--request-url "youtube-dl")
(declare-function youtube-dl--url-from-id "youtube-dl" (id))
(declare-function youtube-dl-item-id "youtube-dl" (item))
(declare-function youtube-dl-play-url "youtube-dl-play" (url &key start))

;;;###autoload
(defgroup youtube-dl-view ()
  "YouTube video descriptions view settings."
  :group 'youtube-dl)

(defface youtube-dl-play-start-time
  '((t :inherit font-lock-builtin-face))
  "Face for highlighting play start time references."
  :group 'youtube-dl-view)

(defface youtube-dl-view-link
  '((t :inherit font-lock-function-name-face))
  "Face for highlighting external links."
  :group 'youtube-dl-view)

(defface youtube-dl-view-mail
  '((t :inherit font-lock-variable-name-face))
  "Face for highlighting e-mail addresses."
  :group 'youtube-dl-view)

(defvar youtube-dl-view-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map "\t" #'youtube-dl-view-next-reference)
      (define-key map [backtab] #'youtube-dl-view-prev-reference)
      (define-key map "\r" #'youtube-dl-view-action)))
  "Keymap for `youtube-dl-view-mode'")

(define-derived-mode youtube-dl-view-mode special-mode "youtube-dl-view"
  "Major mode for viewing youtube-dl items info."
  :group 'youtube-dl-view
  (use-local-map youtube-dl-view-mode-map))

;;;###autoload
(defun youtube-dl-view-url (url)
  "Retrieve and show info from specified URL."
  (interactive (youtube-dl--request-url))
  (cl-declare (special youtube-dl-program youtube-dl-current-url))
  (with-current-buffer (get-buffer-create " *youtube-dl view*")
    (youtube-dl-view-mode)
    (let ((window (get-buffer-window))
          (inhibit-read-only t))
      (erase-buffer)
      (when (zerop (call-process youtube-dl-program nil t nil
                                 "--ignore-config"
                                 "--get-description"
                                 url))
        (goto-char (point-min))
        (while
            (search-forward-regexp
             "\\([a-zA-Z0-9]@[a-zA-Z0-9]\\)\\|\\(\\(https?://\\)[a-zA-Z0-9]+\\.[a-zA-Z0-9]\\)\\|^\\([0-9]+:\\)?[0-9][0-9]:[0-9][0-9]\\(\\.[0-9]+\\)?"
             (point-max) t)
          (cond
           ((match-string 1)
            (let ((link (bounds-of-thing-at-point 'email)))
              (when link
                (put-text-property (car link) (cdr link)
                                   'face 'youtube-dl-view-mail))))
           ((match-string 2)
            (let ((link (bounds-of-thing-at-point 'url)))
              (when link
                (put-text-property (car link) (cdr link)
                                   'face 'youtube-dl-view-link))))
           (t (put-text-property (match-beginning 0) (match-end 0)
                                 'face 'youtube-dl-play-start-time))))
        (set (make-local-variable 'youtube-dl-current-url) url)
        (setf (point) (point-min))
        (when window
          (set-window-point window (point-min)))
        (pop-to-buffer (current-buffer))))))

;;;###autoload
(defun youtube-dl-view ()
  "Show info about an item under point."
  (interactive)
  (youtube-dl-view-url
   (youtube-dl--url-from-id
    (youtube-dl-item-id (youtube-dl--pointed-item)))))

(defun youtube-dl-view-action ()
  "Performs an action associated with the reference under point."
  (interactive)
  (cl-declare (special youtube-dl-current-url))
  (unless (eq major-mode 'youtube-dl-view-mode)
    (error "Not in youtube-dl view buffer."))
  (cond
   ((eq (get-text-property (point) 'face) 'youtube-dl-play-start-time)
    (youtube-dl-play-url youtube-dl-current-url
                         :start (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (or (next-single-property-change (point) 'face) (point-max)))))
   ((eq (get-text-property (point) 'face) 'youtube-dl-view-link)
    (browse-url-at-point))
   ((eq (get-text-property (point) 'face) 'youtube-dl-view-mail)
    (compose-mail (thing-at-point 'email)))
   (t (error "No reference at this point."))))

(defun youtube-dl-view-next-reference ()
  "Move to the next reference if any."
  (interactive)
  (unless (eq major-mode 'youtube-dl-view-mode)
    (error "Not in youtube-dl view buffer."))
  (let ((start (point))
        (pos (next-single-property-change (point) 'face)))
    (while pos
      (if (or (eq 'youtube-dl-play-start-time (get-text-property pos 'face))
              (eq 'youtube-dl-view-link (get-text-property pos 'face))
              (eq 'youtube-dl-view-mail (get-text-property pos 'face)))
          (progn
            (goto-char pos)
            (setq pos nil))
        (setq pos (next-single-property-change pos 'face))))
    (when (equal (point) start)
      (error "No more references."))))

(defun youtube-dl-view-prev-reference ()
  "Move to the previous reference if any."
  (interactive)
  (unless (eq major-mode 'youtube-dl-view-mode)
    (error "Not in youtube-dl view buffer."))
  (let ((start (point))
        (pos (previous-single-property-change (point) 'face)))
    (while pos
      (cond
       ((eq 'youtube-dl-play-start-time (get-text-property pos 'face))
        (goto-char pos)
        (beginning-of-line)
        (setq pos nil))
       ((eq 'youtube-dl-view-link (get-text-property pos 'face))
        (goto-char pos)
        (goto-char (car (bounds-of-thing-at-point 'url)))
        (setq pos nil))
       ((eq 'youtube-dl-view-mail (get-text-property pos 'face))
        (goto-char pos)
        (goto-char (car (bounds-of-thing-at-point 'email)))
        (setq pos nil))
       (t (setq pos (previous-single-property-change pos 'face)))))
    (when (equal (point) start)
      (error "No more references."))))

(provide 'youtube-dl-view)

;;; youtube-dl-view.el ends here
