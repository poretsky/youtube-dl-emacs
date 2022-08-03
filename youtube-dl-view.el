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

;; The `youtube-dl-view' command retrieves video description
;; from specified URL or from an item under point in the download
;; list and shows it.

;;; Code:

(require 'cl-lib)
(require 'button)
(cl-eval-when (load)
  (require 'youtube-dl))

(declare-function youtube-dl "youtube-dl" (url &rest args))
(declare-function youtube-dl-submit "youtube-dl" (videos &rest args))
(declare-function youtube-dl-playable-p "youtube-dl" (url))
(declare-function youtube-dl-thing "youtube-dl")
(declare-function youtube-dl-request-immediate "youtube-dl")
(declare-function youtube-dl-playlist-list "youtube-dl" (url))
(declare-function youtube-dl-item-p "youtube-dl" (item))
(declare-function youtube-dl-item-url "youtube-dl" (item))
(declare-function youtube-dl-item-title "youtube-dl" (item))
(declare-function youtube-dl-item-filesize "youtube-dl" (item))
(declare-function youtube-dl-item-duration "youtube-dl" (item))
(declare-function youtube-dl-item-description "youtube-dl" (item))
(declare-function youtube-dl-item-title-set "youtube-dl" (item title))
(declare-function youtube-dl-item-filesize-set "youtube-dl" (item value))
(declare-function youtube-dl-item-duration-set "youtube-dl" (item value))
(declare-function youtube-dl-item-description-set "youtube-dl" (item text))
(declare-function youtube-dl-play "youtube-dl-play" (url &optional start))
(declare-function w3m-bookmark-add "w3m-bookmark" (url &optional title))

;;;###autoload
(defgroup youtube-dl-view ()
  "YouTube video descriptions view settings."
  :group 'youtube-dl)

(defcustom youtube-dl-view-fill-column 0
  "Column beyond which automatic line-wrapping should happen
in view buffer. Positive number means just that column. If it is 0,
full window width is used. Negative number means respective
subtraction from the window width. If it is nil, no filling
will be applied."
  :group 'youtube-dl-view
  :type '(choice (const :tag "No filling" nil) integer))

(defgroup youtube-dl-view-faces ()
  "Video description display faces."
  :group 'youtube-dl-view)

(defface youtube-dl-view-title
  '((t :inherit font-lock-comment-face))
  "Face for highlighting item title."
  :group 'youtube-dl-view-faces)

(defface youtube-dl-view-header
  '((t :inherit font-lock-comment-delimiter-face))
  "Face for highlighting header titles."
  :group 'youtube-dl-view-faces)

(defface youtube-dl-view-header-value
  '((t :inherit font-lock-comment-face))
  "Face for highlighting header values."
  :group 'youtube-dl-view-faces)

(define-button-type 'youtube-dl-view-play-start-time 'action
  (lambda (button)
    (cl-declare (special youtube-dl-view-current-url))
    (youtube-dl-play youtube-dl-view-current-url
                     (or (button-get button 'start-time)
                         (buffer-substring-no-properties
                          (button-start button)
                          (button-end button)))))
  :supertype 'button)

(define-button-type 'youtube-dl-view-play 'action
  (lambda (_button)
    (cl-declare (special youtube-dl-view-current-url))
    (youtube-dl-play youtube-dl-view-current-url))
  :supertype 'button)

(define-button-type 'youtube-dl-view-download 'action
  (lambda (button)
    (cl-declare (special youtube-dl-view-current-url))
    (youtube-dl youtube-dl-view-current-url
                (youtube-dl-request-immediate) nil
                :extract-audio (button-get button 'audio-only)
                :display t))
  :supertype 'button)

(defvar youtube-dl-view-history nil
  "Viewing history.")

(define-button-type 'youtube-dl-view-link 'action
  (lambda (_button)
    (cl-declare (special youtube-dl-program youtube-dl-view-current-url))
    (unless (and youtube-dl-view-history
                 (string-equal (car youtube-dl-view-history) youtube-dl-view-current-url))
      (push youtube-dl-view-current-url youtube-dl-view-history))
    (let ((url (thing-at-point 'url t)))
      (if (zerop (call-process youtube-dl-program nil nil nil
                               "--ignore-config"
                               "--simulate"
                               "--flat-playlist"
                               url))
          (youtube-dl-view url)
        (browse-url url))))
  :supertype 'button)

(define-button-type 'youtube-dl-view-mail 'action
  (lambda (_button)
    (compose-mail (thing-at-point 'email t)))
  :supertype 'button)

(define-button-type 'youtube-dl-view-back 'action
  (lambda (_button)
    (when (and youtube-dl-view-history
               (string-equal (car youtube-dl-view-history) youtube-dl-view-current-url))
      (pop youtube-dl-view-history))
    (unless youtube-dl-view-history
      (error "No more history."))
    (youtube-dl-view (pop youtube-dl-view-history)))
  :supertype 'button)

(defun youtube-dl-view-quit ()
  "Clear history and close window."
  (interactive)
  (unless (eq major-mode 'youtube-dl-view-mode)
    (error "Not in youtube-dl-view buffer."))
  (setq youtube-dl-view-history nil)
  (quit-window))

(defun youtube-dl-view-add-w3m-bookmark ()
  "Add currently viewed clip to the w3m bookmarks."
  (interactive)
  (cl-declare (special youtube-dl-view-current-url))
  (unless (eq major-mode 'youtube-dl-view-mode)
    (error "Not in youtube-dl-view buffer."))
  (require 'w3m-bookmark)
  (w3m-bookmark-add youtube-dl-view-current-url header-line-format)
  (message "Bookmark added"))

(defvar youtube-dl-view-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (set-keymap-parent map button-buffer-map)
      (define-key map "a" #'youtube-dl-view-add-w3m-bookmark)
      (define-key map "q" #'youtube-dl-view-quit)))
  "Keymap for `youtube-dl-view-mode'")

(define-derived-mode youtube-dl-view-mode special-mode "youtube-dl-view"
  "Major mode for viewing youtube-dl items info."
  :group 'youtube-dl-view
  (use-local-map youtube-dl-view-mode-map))

(defconst youtube-dl-view-time-spec
  "\\(?:[0-9]+:\\)?[0-9][0-9]:[0-9][0-9]\\(?:\\.[0-9]+\\)?"
  "Timespec matching regexp.")

(cl-defun youtube-dl-view--show-description
    (text url submitted-p &key title filesize duration)
  "Show a description represented by given text.
The second argument specifies source URL for reference.
The third argument indicates whether this URL is already submitted
for download."
  (cl-declare (special youtube-dl-view-current-url))
  (with-current-buffer (get-buffer-create " *youtube-dl view*")
    (youtube-dl-view-mode)
    (let ((window (get-buffer-window))
          (inhibit-read-only t)
          (playable-p (youtube-dl-playable-p url)))
      (erase-buffer)
      (when title
        (setq header-line-format
              (propertize title 'face 'youtube-dl-view-title)))
      (when filesize
        (insert (propertize "FileSize" 'face 'youtube-dl-view-header)
                "  "
                (propertize (format "%d" filesize) 'face 'youtube-dl-view-header-value)
                "\n"))
      (when duration
        (insert (propertize "Duration" 'face 'youtube-dl-view-header)
                "  "
                (propertize (format-time-string "%T" (seconds-to-time duration) t)
                            'face 'youtube-dl-view-header-value)
                "\n"))
      (unless (bobp)
        (insert "\n"))
      (when playable-p
        (insert-button "Play"
                       :type 'youtube-dl-view-play))
      (unless submitted-p
        (unless (bolp)
          (insert "  "))
        (insert-button "Download"
                       :type 'youtube-dl-view-download)
        (when playable-p
          (insert "  ")
          (insert-button "Download audio" 'audio-only t
                         :type 'youtube-dl-view-download)))
      (unless (bolp)
        (insert "\n\n"))
      (let ((start (point)))
        (insert (or text ""))
        (unless (or (bobp) (= (char-before) ?\n))
          (insert "\n"))
        (when youtube-dl-view-history
          (insert "\n")
          (insert-button "Back"
                         :type 'youtube-dl-view-back)
          (insert "\n"))
        (goto-char start)
        (while
            (re-search-forward
             (concat
              "\\( +\\)"
              youtube-dl-view-time-spec)
             nil t)
          (replace-match "\n" nil nil nil 1))
        (when (integerp youtube-dl-view-fill-column)
          (let ((fill-column
                 (if (> youtube-dl-view-fill-column 0)
                     youtube-dl-view-fill-column
                   (- (window-body-width) youtube-dl-view-fill-column))))
            (fill-individual-paragraphs start (point-max) nil
                                        (concat
                                         youtube-dl-view-time-spec
                                         " \\|-*[0-9]+\\. \\|.+[ \t\n][^ \t\n]+\\(?:@\\|://\\)"))))
        (goto-char start))
      (while
          (re-search-forward
           (concat
            "\\([a-zA-Z0-9]@[a-zA-Z0-9]\\)\\|\\(https?://[a-zA-Z0-9]+\\.[a-zA-Z0-9]\\)\\|^\\(?:\\(-*\\([0-9]+\\.\\) .*(\\("
            youtube-dl-view-time-spec
            "\\))\\)\\|"
            youtube-dl-view-time-spec
            "\\)")
           nil t)
        (cond
         ((match-string 1)
          (let ((link (bounds-of-thing-at-point 'email)))
            (when link
              (make-button (car link) (cdr link)
                           :type 'youtube-dl-view-mail))))
         ((match-string 2)
          (let ((link (bounds-of-thing-at-point 'url)))
            (when link
              (make-button (car link) (cdr link)
                           :type 'youtube-dl-view-link))))
         ((match-string 3)
          (make-button (match-beginning 4) (match-end 4)
                       'start-time (buffer-substring-no-properties (match-beginning 5) (match-end 5))
                       :type 'youtube-dl-view-play-start-time))
         (t (make-button (match-beginning 0) (match-end 0)
                         :type 'youtube-dl-view-play-start-time))))
      (set (make-local-variable 'youtube-dl-view-current-url) url)
      (setf (point) (point-min))
      (when window
        (set-window-point window (point-min)))
      (let ((split-width-threshold nil))
        (pop-to-buffer (current-buffer))))))

;;;###autoload
(defun youtube-dl-view (thing)
  "Retrieves and shows info from specified URL or, being invoked
in the download listing, for an item under point. If specified URL
points to a playlist and it has more than one item, this playlist
is submitted for download as paused and shown in the listing."
  (interactive (youtube-dl-thing))
  (let* ((submitted-p (youtube-dl-item-p thing))
         (playlist
          (cond
           ((stringp thing)
            (youtube-dl-playlist-list thing))
           ((and submitted-p
                 (or (not (youtube-dl-item-description thing))
                     (not (youtube-dl-item-title thing))))
            (youtube-dl-playlist-list (youtube-dl-item-url thing)))
           (t nil)))
         (item
          (if playlist
              (car playlist)
            thing))
         (title
          (if (youtube-dl-item-p item)
              (youtube-dl-item-title item)
            (or (plist-get item :title)
                (plist-get item :id))))
         (filesize
          (if (youtube-dl-item-p item)
              (youtube-dl-item-filesize item)
            (plist-get item :filesize)))
         (duration
          (if (youtube-dl-item-p item)
              (youtube-dl-item-duration item)
            (plist-get item :duration)))
         (text
          (or (if (youtube-dl-item-p item)
                  (youtube-dl-item-description item)
                (plist-get item :description))
              ""))
         (url
          (if (youtube-dl-item-p item)
              (youtube-dl-item-url item)
            (or (plist-get item :url) thing))))
    (if (> (length playlist) 1)
        (youtube-dl-submit playlist
                           :display t)
      (when (and submitted-p playlist)
        (youtube-dl-item-title-set thing title)
        (youtube-dl-item-description-set thing text)
        (youtube-dl-item-filesize-set thing filesize)
        (youtube-dl-item-duration-set thing duration))
      (youtube-dl-view--show-description text url submitted-p
                                         :title title
                                         :filesize filesize
                                         :duration duration))))

(provide 'youtube-dl-view)

;;; youtube-dl-view.el ends here
