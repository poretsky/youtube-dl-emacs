;;; youtube-dl.el --- manages a youtube-dl queue -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Igor B. Poretsky <poretsky@mlbox.ru>

;; Original author: Christopher Wellons <wellons@nullprogram.com>
;; URL: https://github.com/skeeto/youtube-dl-emacs
;; Version: 1.0
;; Package-Requires: ((emacs "24.3"))

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

;; This package manages a video download queue for the youtube-dl
;; command line program, which serves as its back end. It manages a
;; single youtube-dl subprocess to download one video at a time. New
;; videos can be queued at any time.

;; The `youtube-dl' command queues a URL for download. Failures are
;; retried up to `youtube-dl-max-failures'. Items can be paused or set
;; to be downloaded at a slower rate (`youtube-dl-slow-rate').

;; The `youtube-dl-playlist' command queues an entire playlist, just
;; as if you had individually queued each video on the playlist.

;; The `youtube-dl-list' command displays a list of all active video
;; downloads. From this list, items under point can be canceled (d),
;; paused (p), slowed (s), and have its priority adjusted ([ and ]).

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'hl-line)

(defgroup youtube-dl ()
  "Download queue for the youtube-dl command line program."
  :group 'external)

(defcustom youtube-dl-download-directory "~/download"
  "Default directory for downloads."
  :group 'youtube-dl
  :type 'directory)

(defcustom youtube-dl-program "youtube-dl"
  "The name of the program invoked for downloading YouTube videos."
  :group 'youtube-dl
  :type 'string)

(defcustom youtube-dl-omit-mtime nil
  "Whether to omit timestamp from the `Last-modified' header
for downloaded files."
  :group 'youtube-dl
  :type 'boolean)

(defcustom youtube-dl-restrict-filenames nil
  "Whether to restrict downloaded file names to only ASCII characters."
  :group 'youtube-dl
  :type 'boolean)

(defcustom youtube-dl-max-failures 8
  "Maximum number of retries for a single video."
  :group 'youtube-dl
  :type 'integer)

(defcustom youtube-dl-slow-rate "2M"
  "Download speed for \"slow\" items (argument for --rate-limit)."
  :group 'youtube-dl
  :type 'string)

(defconst youtube-dl-audio-quality
  '(radio :tag "Quality level"
          (const :tag "default" nil)
          (const :doc "The best quality but slowest conversion" "0")
          (const "1")
          (const "2")
          (const "3")
          (const "4")
          (const :doc "Default level" "5")
          (const "6")
          (const "7")
          (const "8")
          (const "9")
          (const :doc "The worst quality but fast conversion" "10"))
  "Audio quality choice for some formats extraction.")

(defcustom youtube-dl-preferred-audio-format nil
  "Preferred format for audio extraction."
  :group 'youtube-dl
  :type `(choice (const :tag "default" nil)
                 (cons :tag "aac" (const "aac") ,youtube-dl-audio-quality)
                 (const "flac")
                 (cons :tag "mp3" (const "mp3") ,youtube-dl-audio-quality)
                 (const "m4a")
                 (cons :tag "opus" (const "opus") ,youtube-dl-audio-quality)
                 (cons :tag "vorbis" (const "vorbis") ,youtube-dl-audio-quality)
                 (const "wav")
                 (const "alac")))

(defface youtube-dl-active
  '((t :inherit font-lock-function-name-face))
  "Face for highlighting the active download item."
  :group 'youtube-dl)

(defface youtube-dl-slow
  '((t :inherit font-lock-variable-name-face))
  "Face for highlighting the slow (S) tag."
  :group 'youtube-dl)

(defface youtube-dl-pause
  '((t :inherit font-lock-type-face))
  "Face for highlighting the pause (P) tag."
  :group 'youtube-dl)

(defface youtube-dl-audio-content
  '((t :inherit font-lock-preprocessor-face))
  "Face for highlighting the audio content (A) tag."
  :group 'youtube-dl)

(defface youtube-dl-priority
  '((t :inherit font-lock-keyword-face))
  "Face for highlighting the priority marker."
  :group 'youtube-dl)

(defface youtube-dl-failure
  '((t :inherit font-lock-warning-face))
  "Face for highlighting the failure marker."
  :group 'youtube-dl)

(defvar-local youtube-dl--log-item nil
  "Item currently being displayed in the log buffer.")

(cl-defstruct (youtube-dl-item (:constructor youtube-dl-item--create)
                               (:copier nil))
  "Represents a single video to be downloaded with youtube-dl."
  id           ; YouTube video ID (string)
  audio-p      ; Non-nil if only audio should be extracted
  directory    ; Working directory for youtube-dl (string or nil)
  destination  ; Preferred destination file (string or nil)
  failures     ; Number of video download failures (integer)
  priority     ; Download priority (integer)
  title        ; Listing display title (string or nil)
  progress     ; Current download progress (string or nil)
  total        ; Total download size (string or nil)
  log          ; All program output (list of strings)
  log-end      ; Last log item (list of strings)
  paused-p     ; Non-nil if download is paused
  slow-p)      ; Non-nil if download should be rate limited

(defvar youtube-dl-items ()
  "List of all items still to be downloaded.")

(defvar youtube-dl-process nil
  "The currently active youtube-dl process.")

(defun youtube-dl--next ()
  "Returns the next item to be downloaded."
  (let (best best-score)
    (dolist (item youtube-dl-items best)
      (let* ((failures (youtube-dl-item-failures item))
             (priority (youtube-dl-item-priority item))
             (paused-p (youtube-dl-item-paused-p item))
             (score (- priority failures)))
        (when (and (not paused-p)
                   (< failures youtube-dl-max-failures))
          (cond ((null best)
                 (setf best item
                       best-score score))
                ((> score best-score)
                 (setf best item
                       best-score score))))))))

(defun youtube-dl--remove (item)
  "Remove ITEM from the queue."
  (setf youtube-dl-items (cl-delete item youtube-dl-items)))

(defun youtube-dl--add (item)
  "Add ITEM to the queue."
  (setf youtube-dl-items (nconc youtube-dl-items (list item))))

(defun youtube-dl--sentinel (proc status)
  (let ((item (plist-get (process-plist proc) :item)))
    (setf youtube-dl-process nil)
    (if (equal status "finished\n")
        (youtube-dl--remove item)
      (cl-incf (youtube-dl-item-failures item)))
    (youtube-dl--run)))

(defun youtube-dl--progress (output)
  "Return the download progress for the given output.
Progress lines that straddle output chunks are lost. That's fine
since this is just used for display purposes."
  (let ((start 0)
        (pair nil))
    (while (string-match "\\([^ ]+%\\) +of +\\([^ ]+\\) " output start)
      (setf pair (cons (match-string 1 output) (match-string 2 output))
            start (match-end 0)))
    pair))

(defun youtube-dl--destination (output)
  "Return the destination file for the given output (if any).
The destination filename may potentially straddle two output
chunks, but this is incredibly unlikely. It's only used for
display purposes anyway."
  (when (string-match " Destination: \\([^\n]+\\)" output)
    (match-string 1 output)))

(defun youtube-dl--filter (proc output)
  (let* ((item (plist-get (process-plist proc) :item))
         (out (replace-regexp-in-string "\033[^ ]*m" "" output))
         (progress (youtube-dl--progress out))
         (destination (unless (youtube-dl-item-title item)
                        (youtube-dl--destination out))))
    ;; Append to program log.
    (let ((logged (list output)))
      (if (youtube-dl-item-log item)
          (setf (cdr (youtube-dl-item-log-end item)) logged
                (youtube-dl-item-log-end item) logged)
        (setf (youtube-dl-item-log item) logged
              (youtube-dl-item-log-end item) logged)))
    ;; Update progress information.
    (when progress
      (cl-destructuring-bind (percentage . total) progress
        (setf (youtube-dl-item-progress item) percentage
              (youtube-dl-item-total item) total)))
    ;; Set a title if needed.
    (when destination
      (setf (youtube-dl-item-title item) destination))
    (youtube-dl--redisplay)))

(defun youtube-dl--current ()
  "Return the item currently being downloaded."
  (when youtube-dl-process
    (plist-get (process-plist youtube-dl-process) :item)))

(defun youtube-dl--run ()
  "As necessary, start or switch to the highest priority item."
  (let ((item (youtube-dl--next))
        (current-item (youtube-dl--current)))
    (if (eq item current-item)
        (youtube-dl--redisplay) ; do nothing
      (if youtube-dl-process
          (progn
            ;; Switch to higher priority job, but offset error count first.
            (cl-decf (youtube-dl-item-failures current-item))
            (kill-process youtube-dl-process)) ; sentinel will clean up
        ;; No subprocess running, start a one.
        (let* ((directory (youtube-dl-item-directory item))
               (destination (youtube-dl-item-destination item))
               (default-directory
                 (if directory
                     (concat (directory-file-name directory) "/")
                   (concat (directory-file-name youtube-dl-download-directory) "/")))
               (id (youtube-dl-item-id item))
               (audio-p (youtube-dl-item-audio-p item))
               (audio-format
                (and audio-p
                     (or (and (consp youtube-dl-preferred-audio-format)
                              (car youtube-dl-preferred-audio-format))
                         youtube-dl-preferred-audio-format)))
               (audio-quality
                (and audio-format
                     (consp youtube-dl-preferred-audio-format)
                     (cdr youtube-dl-preferred-audio-format)))
               (slow-p (youtube-dl-item-slow-p item))
               (proc (progn
                       (mkdir default-directory t)
                       (apply #'start-process
                              "youtube-dl" nil youtube-dl-program "--newline"
                              (nconc
                               (when youtube-dl-omit-mtime
                                 (list "--no-mtime"))
                               (when youtube-dl-restrict-filenames
                                 (list "--restrict-filenames"))
                               (when audio-p
                                 (list "-x"))
                               (when audio-format
                                 `("--audio-format" ,audio-format))
                               (when audio-quality
                                 `("--audio-quality" ,audio-quality))
                               (when slow-p
                                 `("--rate-limit" ,youtube-dl-slow-rate))
                               (when destination
                                 `("--output" ,destination))
                               `("--" ,id))))))
          (set-process-plist proc (list :item item))
          (set-process-sentinel proc #'youtube-dl--sentinel)
          (set-process-filter proc #'youtube-dl--filter)
          (setf youtube-dl-process proc))))
    (youtube-dl--redisplay)))

(defun youtube-dl--id-from-url (url)
  "Return the 11-character video ID for URL."
  (save-match-data
    (when (string-match
           "\\(?:\\.be/\\|v=\\|v%3D\\|^\\)\\([-_a-zA-Z0-9]\\{11\\}\\)" url)
      (match-string 1 url))))

;;;###autoload
(cl-defun youtube-dl
    (url &key title extract-audio (priority 0) directory destination paused slow)
  "Queues URL for download using youtube-dl, returning the new item."
  (interactive
   (list (read-from-minibuffer
          "URL: " (or (thing-at-point 'url)
                      (when interprogram-paste-function
                        (funcall interprogram-paste-function))))))
  (let* ((id (youtube-dl--id-from-url url))
         (full-dir (expand-file-name (or directory "") youtube-dl-download-directory))
         (item (youtube-dl-item--create :id id
                                        :audio-p extract-audio
                                        :failures 0
                                        :priority priority
                                        :paused-p paused
                                        :slow-p slow
                                        :directory full-dir
                                        :destination destination
                                        :title title)))
    (prog1 item
      (when id
        (youtube-dl--add item)
        (youtube-dl--run)))))

;;;###autoload
(cl-defun youtube-dl-audio
    (url &key title (priority 0) directory destination paused slow)
  "Queues URL for download audio content using youtube-dl, returning the new item."
  (interactive
   (list (read-from-minibuffer
          "URL: " (or (thing-at-point 'url)
                      (when interprogram-paste-function
                        (funcall interprogram-paste-function))))))
  (youtube-dl url
              :title title
              :extract-audio t
              :priority priority
              :directory directory
              :destination destination
              :paused paused
              :slow slow))

(defun youtube-dl--playlist-list (playlist)
  "For each video, return one plist with :index, :id, and :title."
  (with-temp-buffer
    (when (zerop (call-process youtube-dl-program nil t nil
                               "--ignore-config"
                               "--dump-json"
                               "--flat-playlist"
                               playlist))
      (setf (point) (point-min))
      (cl-loop with json-object-type = 'plist
               for index upfrom 1
               for video = (ignore-errors (json-read))
               while video
               collect (list :index index
                             :id    (plist-get video :id)
                             :title (plist-get video :title))))))

(defun youtube-dl--playlist-reverse (list)
  "Return a copy of LIST with the indexes reversed."
  (let ((max (cl-loop for entry in list
                      maximize (plist-get entry :index))))
    (cl-loop for entry in list
             for index = (plist-get entry :index)
             for copy = (copy-sequence entry)
             collect (plist-put copy :index (- (1+ max) index)))))

(defun youtube-dl--playlist-cutoff (list n)
  "Return a sorted copy of LIST with all items except where :index < N."
  (let ((key (lambda (v) (plist-get v :index)))
        (filter (lambda (v) (< (plist-get v :index) n)))
        (copy (copy-sequence list)))
    (cl-delete-if filter (cl-stable-sort copy #'< :key key))))

;;;###autoload
(cl-defun youtube-dl-playlist
    (url &key extract-audio directory (first 1) paused (priority 0) reverse slow)
  "Add entire playlist to download queue, with index prefixes.

:extract-audio BOOL -- Extract audio content.

:directory PATH -- Destination directory for all videos.

:first INDEX -- Start downloading from a given one-based index.

:paused BOOL -- Start all download entries as paused.

:priority PRIORITY -- Use this priority for all download entries.

:reverse BOOL -- Reverse the video numbering, solving the problem
of reversed playlists.

:slow BOOL -- Start all download entries in slow mode."
  (interactive
   (list (read-from-minibuffer
          "URL: "
          (when interprogram-paste-function
            (funcall interprogram-paste-function)))))
  (message "Fetching playlist ...")
  (let ((videos (youtube-dl--playlist-list url)))
    (if (null videos)
        (error "Failed to fetch playlist (%s)." url)
      (let* ((max (cl-loop for entry in videos
                           maximize (plist-get entry :index)))
             (width (1+ (floor (log max 10))))
             (prefix-format (format "%%0%dd" width)))
        (when reverse
          (setf videos (youtube-dl--playlist-reverse videos)))
        (dolist (video (youtube-dl--playlist-cutoff videos first))
          (let* ((index (plist-get video :index))
                 (prefix (format prefix-format index))
                 (title (format "%s-%s" prefix (plist-get video :title)))
                 (dest (format "%s-%s" prefix "%(title)s-%(id)s.%(ext)s")))
            (youtube-dl (plist-get video :id)
                        :title title
                        :extract-audio extract-audio
                        :priority priority
                        :directory directory
                        :destination dest
                        :paused paused
                        :slow slow)))))))

;;;###autoload
(cl-defun youtube-dl-playlist-audio
    (url &key directory (first 1) paused (priority 0) reverse slow)
  "Add entire playlist to download queue, with index prefixes.
Only audio content will be extracted.

:directory PATH -- Destination directory for all videos.

:first INDEX -- Start downloading from a given one-based index.

:paused BOOL -- Start all download entries as paused.

:priority PRIORITY -- Use this priority for all download entries.

:reverse BOOL -- Reverse the video numbering, solving the problem
of reversed playlists.

:slow BOOL -- Start all download entries in slow mode."
  (interactive
   (list (read-from-minibuffer
          "URL: "
          (when interprogram-paste-function
            (funcall interprogram-paste-function)))))
  (youtube-dl-playlist url
                       :extract-audio t
                       :directory directory
                       :first first
                       :paused paused
                       :priority priority
                       :reverse reverse
                       :slow slow))

;; List user interface:

(defun youtube-dl-list-redisplay ()
  "Immediately redraw the queue list buffer."
  (interactive)
  (with-current-buffer (youtube-dl--buffer)
    (let ((save-point (point))
          (window (get-buffer-window (current-buffer))))
      (youtube-dl--fill-listing)
      (setf (point) save-point)
      (when window
        (set-window-point window save-point))
      (when hl-line-mode
        (hl-line-highlight)))))

(defun youtube-dl--redisplay ()
  "Redraw the queue list buffer only if visible."
  (let ((log-buffer (youtube-dl--log-buffer)))
    (when log-buffer
      (with-current-buffer log-buffer
        (let ((inhibit-read-only t)
              (save-point (point))
              (save-max-point (point-max))
              (window (get-buffer-window log-buffer)))
          (erase-buffer)
          (mapc #'insert (youtube-dl-item-log youtube-dl--log-item))
          (when window
            (set-window-point window
                              (if (< save-point save-max-point)
                                  save-point
                                (point-max))))))))
  (when (get-buffer-window (youtube-dl--buffer))
    (youtube-dl-list-redisplay)))

(defun youtube-dl--pointed-item ()
  "Get item under point. Signal an error if none."
  (unless (eq (current-buffer) (youtube-dl--buffer))
    (error "The operation is not available in this buffer."))
  (let ((item (nth (1- (line-number-at-pos)) youtube-dl-items)))
    (unless item
      (error "No item at this line."))
    item))

(defun youtube-dl-list-log ()
  "Display the log of the video under point."
  (interactive)
  (display-buffer (youtube-dl--log-buffer (youtube-dl--pointed-item)))
  (youtube-dl--redisplay))

(defun youtube-dl-list-kill-log ()
  "Kill the youtube-dl log buffer."
  (interactive)
  (let ((buffer (youtube-dl--log-buffer)))
    (when buffer
      (kill-buffer buffer))))

(defun youtube-dl-list-yank ()
  "Copy the URL of the video under point to the clipboard."
  (interactive)
  (let ((url (concat "https://youtu.be/" (youtube-dl-item-id (youtube-dl--pointed-item)))))
    (if (fboundp 'gui-set-selection)
        (gui-set-selection nil url)     ; >= Emacs 25
      (with-no-warnings
        (x-set-selection 'PRIMARY url))) ; <= Emacs 24
    (message "Yanked %s" url)))

(defun youtube-dl-list-kill ()
  "Remove the selected item from the queue."
  (interactive)
  (when (= (line-number-at-pos) (length youtube-dl-items))
    (forward-line -1))
  (youtube-dl--remove (youtube-dl--pointed-item))
  (youtube-dl--run))

(defun youtube-dl-list-priority-modify (delta)
  "Change priority of item under point by DELTA."
  (cl-incf (youtube-dl-item-priority (youtube-dl--pointed-item)) delta)
  (youtube-dl--run))

(defun youtube-dl-list-toggle-pause (item)
  "Toggle pause on item under point."
  (interactive
   (list (youtube-dl--pointed-item)))
  (let ((paused-p (youtube-dl-item-paused-p item)))
    (setf (youtube-dl-item-paused-p item) (not paused-p))
    (if (eq item (youtube-dl--current))
        (youtube-dl--run)
      (youtube-dl--redisplay))))

(defun youtube-dl-list-toggle-pause-all ()
  "Toggle pause on all items. When paused items are in the minority,
all other items are paused, and vice versa."
  (interactive)
  (let* ((count (length  youtube-dl-items))
         (paused-count (cl-count-if #'youtube-dl-item-paused-p youtube-dl-items))
         (target (< paused-count (- count paused-count))))
    (dolist (item youtube-dl-items)
      (unless (eq target (youtube-dl-item-paused-p item))
        (youtube-dl-list-toggle-pause item)))
    (unless (or target (youtube-dl--current))
      (youtube-dl--run))))

(defun youtube-dl-list-toggle-slow (item)
  "Toggle slow mode on item under point."
  (interactive
   (list (youtube-dl--pointed-item)))
  (let ((slow-p (youtube-dl-item-slow-p item)))
    (setf (youtube-dl-item-slow-p item) (not slow-p))
    (if (not (eq item (youtube-dl--current)))
        (youtube-dl--redisplay)
      ;; Offset error count and restart the process.
      (cl-decf (youtube-dl-item-failures item))
      (kill-process youtube-dl-process))))

(defun youtube-dl-list-toggle-slow-all ()
  "Toggle slow mode on all items. When slow items are in the minority,
all other items are made slow, and vice versa."
  (interactive)
  (let* ((count (length  youtube-dl-items))
         (slow-count (cl-count-if #'youtube-dl-item-slow-p youtube-dl-items))
         (target (< slow-count (- count slow-count))))
    (dolist (item youtube-dl-items)
      (unless (eq target (youtube-dl-item-slow-p item))
        (youtube-dl-list-toggle-slow item)))))

(defun youtube-dl-list-priority-up ()
  "Decrease priority of item under point."
  (interactive)
  (youtube-dl-list-priority-modify 1))

(defun youtube-dl-list-priority-down ()
  "Increase priority of item under point."
  (interactive)
  (youtube-dl-list-priority-modify -1))

(defvar youtube-dl-list-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map "a" #'youtube-dl)
      (define-key map "g" #'youtube-dl-list-redisplay)
      (define-key map "l" #'youtube-dl-list-log)
      (define-key map "L" #'youtube-dl-list-kill-log)
      (define-key map "y" #'youtube-dl-list-yank)
      (define-key map "j" #'next-line)
      (define-key map "k" #'previous-line)
      (define-key map "d" #'youtube-dl-list-kill)
      (define-key map "p" #'youtube-dl-list-toggle-pause)
      (define-key map "P" #'youtube-dl-list-toggle-pause-all)
      (define-key map "s" #'youtube-dl-list-toggle-slow)
      (define-key map "S" #'youtube-dl-list-toggle-slow-all)
      (define-key map "]" #'youtube-dl-list-priority-up)
      (define-key map "[" #'youtube-dl-list-priority-down)))
  "Keymap for `youtube-dl-list-mode'")

(define-derived-mode youtube-dl-list-mode special-mode "youtube-dl"
  "Major mode for listing the youtube-dl download queue."
  :group 'youtube-dl
  (use-local-map youtube-dl-list-mode-map)
  (hl-line-mode)
  (setf truncate-lines t
        header-line-format
        (format "%s%-11s %-6.6s %-10.10s %s"
                (propertize " " 'display '((space :align-to 0)))
                "id" "done" "size" "title")))

(defun youtube-dl--buffer ()
  "Returns the queue listing buffer."
  (with-current-buffer (get-buffer-create " *youtube-dl list*")
    (youtube-dl-list-mode)
    (current-buffer)))

(defun youtube-dl--log-buffer (&optional item)
  "Returns a youtube-dl log buffer for ITEM."
  (let* ((name " *youtube-dl log*")
         (buffer (if item (get-buffer-create name) (get-buffer name))))
    (when (or item (and buffer (get-buffer-window buffer)))
      (with-current-buffer buffer
        (unless (eq major-mode 'special-mode)
          (special-mode))
        (when item
          (setf youtube-dl--log-item item))
        (current-buffer)))))

(defun youtube-dl--fill-listing ()
  "Erase and redraw the queue in the queue listing buffer."
  (with-current-buffer (youtube-dl--buffer)
    (let* ((inhibit-read-only t)
           (active (youtube-dl--current))
           (string-audio (propertize "A" 'face 'youtube-dl-audio-content))
           (string-slow (propertize "S" 'face 'youtube-dl-slow))
           (string-paused (propertize "P" 'face 'youtube-dl-pause)))
      (erase-buffer)
      (dolist (item youtube-dl-items)
        (let ((id (youtube-dl-item-id item))
              (failures (youtube-dl-item-failures item))
              (priority (youtube-dl-item-priority item))
              (progress (youtube-dl-item-progress item))
              (audio-p (youtube-dl-item-audio-p item))
              (paused-p (youtube-dl-item-paused-p item))
              (slow-p (youtube-dl-item-slow-p item))
              (total (youtube-dl-item-total item))
              (title (youtube-dl-item-title item)))
          (insert
           (format "%-11s %-6.6s %-10.10s %s%s%s%s\n"
                   (if (eq active item)
                       (propertize id 'face 'youtube-dl-active)
                     id)
                   (or progress "0.0%")
                   (or total "???")
                   (if (= failures 0)
                       ""
                     (propertize (format "[%d] " failures)
                                 'face 'youtube-dl-failure))
                   (if (= priority 0)
                       ""
                     (propertize (format "%+d " priority)
                                 'face 'youtube-dl-priority))
                   (if (or slow-p paused-p audio-p)
                       (concat
                        (if slow-p
                            string-slow
                          "")
                        (if paused-p
                            string-paused
                          "")
                        (if audio-p
                            string-audio
                          "")
                        " ")
                     "")
                   (or title ""))))))))

;;;###autoload
(defun youtube-dl-list ()
  "Display a list of all videos queued for download."
  (interactive)
  (youtube-dl--fill-listing)
  (pop-to-buffer (youtube-dl--buffer)))

(provide 'youtube-dl)

;;; youtube-dl.el ends here
