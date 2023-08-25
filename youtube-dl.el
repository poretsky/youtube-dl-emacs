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

;; The `youtube-dl' command queues a URL for download. The command
;; `youtube-dl-audio' does the same, but only audio content is
;; retrieved. Failures are retried up to `youtube-dl-max-failures'.
;; Items can be paused or set to be downloaded at a slower rate
;; (`youtube-dl-slow-rate').

;; The `youtube-dl-list' command displays a list of all active video
;; downloads. From this list, items under point can be canceled (d),
;; paused (p), slowed (s), and have its priority adjusted ([ and ]).

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'hl-line)

(declare-function youtube-dl-play "youtube-dl-play")
(declare-function youtube-dl-view "youtube-dl-view")

;;;###autoload
(defgroup youtube-dl
  '((youtube-dl-play custom-group)
    (youtube-dl-view custom-group)
    (youtube-dl-w3m custom-group))
  "YouTube video download queue control options."
  :group 'external)

(defcustom youtube-dl-download-directory "~/download/youtube"
  "Default directory for downloads."
  :group 'youtube-dl
  :type 'directory)

(defcustom youtube-dl-video-folder "video"
  "Folder name for download YouTube videos. it will be created
by need as a subfolder of the download directory specified
by the option `youtube-dl-download-directory'. Actually here go
the downloads from URLs satisfying criteria listed in the option
 `youtube-dl-playable-urls' unless audio extraction is requested."
  :group 'youtube-dl
  :type 'string)

(defcustom youtube-dl-audio-folder "audio"
  "Folder name for download audio extractions. it will be created
by need as a subfolder of the download directory specified
by the option `youtube-dl-download-directory'."
  :group 'youtube-dl
  :type 'string)

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

(defcustom youtube-dl-playable-urls
  '("https?://youtu\\.be/[-_a-zA-Z0-9]\\{11\\}"
    "https?://\\(?:www\\.\\)?youtube\\.com/shorts/[-_a-zA-Z0-9]\\{11\\}"
    "https?://\\(?:www\\.\\)?youtube\\.com/watch\\?v\\(?:=\\|%3D\\)[-_a-zA-Z0-9]\\{11\\}"
    "https?://music.yandex.ru/album/[0-9]\\{8\\}/track/[0-9]\\{8\\}")
  "Patterns that match to directly playable URLs."
  :group 'youtube-dl
  :type '(repeat regexp))

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

(defcustom youtube-dl-immediate t
  "Wether to start download immediately when queueing.
If nil, items are queued as paused.
`always' means to start download immediately without asking.
Any other value means to ask for each queueing item."
  :group 'youtube-dl
  :type '(choice (const :tag "Yes" always)
                 (const :tag "No" nil)
                 (const :tag "Ask" t)))

(defcustom youtube-dl-include-metadata nil
  "Whether to include metadata in the downloaded files."
  :group 'youtube-dl
  :type 'boolean)

(defgroup youtube-dl-faces ()
  "Download listing display faces."
  :group 'youtube-dl)

(defface youtube-dl-active
  '((t :inherit font-lock-function-name-face))
  "Face for highlighting the active download item."
  :group 'youtube-dl-faces)

(defface youtube-dl-playlist-title
  '((t :inherit font-lock-comment-face))
  "Face for highlighting a playlist title."
  :group 'youtube-dl-faces)

(defface youtube-dl-slow
  '((t :inherit font-lock-variable-name-face))
  "Face for highlighting the slow (S) tag."
  :group 'youtube-dl-faces)

(defface youtube-dl-pause
  '((t :inherit font-lock-type-face))
  "Face for highlighting the pause (P) tag."
  :group 'youtube-dl-faces)

(defface youtube-dl-audio-content
  '((t :inherit font-lock-preprocessor-face))
  "Face for highlighting the audio content (A) tag."
  :group 'youtube-dl-faces)

(defface youtube-dl-priority
  '((t :inherit font-lock-keyword-face))
  "Face for highlighting the priority marker."
  :group 'youtube-dl-faces)

(defface youtube-dl-failure
  '((t :inherit font-lock-warning-face))
  "Face for highlighting the failure marker."
  :group 'youtube-dl-faces)

(defvar-local youtube-dl--log-item nil
  "Item currently being displayed in the log buffer.")

(cl-defstruct (youtube-dl-item (:constructor youtube-dl-item--create)
                               (:copier nil))
  "Represents a single video to be downloaded with youtube-dl."
  id           ; YouTube video ID (string)
  url          ; Original URL (string)
  playlist     ; Playlist title (string or nil)
  playlist-url ; URL of playlist (string or nil)
  audio-p      ; Non-nil if only audio should be extracted
  directory    ; Working directory for youtube-dl (string or nil)
  destination  ; Preferred destination file (string or nil)
  failures     ; Number of video download failures (integer)
  priority     ; Download priority (integer)
  dest-name    ; Listing display title (string or nil)
  title        ; Info display title (string or nil)
  filesize     ; Declared file size (number or nil)
  duration     ; Declared duration (number or nil)
  description  ; Item description cache (string or nil)
  progress     ; Current download progress (string or nil)
  total        ; Total download size (string or nil)
  log          ; All program output (list of strings)
  log-end      ; Last log item (list of strings)
  paused-p     ; Non-nil if download is paused
  slow-p)      ; Non-nil if download should be rate limited

(defun youtube-dl-item-title-set (item title)
  "Set title for specified item."
  (setf (youtube-dl-item-title item) title))

(defun youtube-dl-item-dest-name-set (item name)
  "Set destination name for specified item."
  (setf (youtube-dl-item-dest-name item) name))

(defun youtube-dl-item-filesize-set (item value)
  "Set filesize value for specified item."
  (setf (youtube-dl-item-filesize item) value))

(defun youtube-dl-item-duration-set (item value)
  "Set duration value for specified item."
  (setf (youtube-dl-item-duration item) value))

(defun youtube-dl-item-description-set (item text)
  "Set description text for specified item."
  (setf (youtube-dl-item-description item) text))

(defun youtube-dl-playable-p (url)
  "Test given URL if it is directly playable."
  (let ((patterns youtube-dl-playable-urls)
        (matched nil))
    (while (and patterns (not matched))
      (if (string-match (car patterns) url)
          (setq matched t)
        (setq patterns (cdr patterns))))
    matched))

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
    (while (string-match "\\([^ ]+%\\) +of +\\(~?\\) *\\([^ \n]+\\)[ \n]" output start)
      (setf pair (cons (match-string 1 output)
                       (concat (match-string 2 output) (match-string 3 output)))
            start (match-end 0)))
    pair))

(defun youtube-dl--destination (item output)
  "Return the destination name for the given item and output (if any).
The destination filename may potentially straddle two output
chunks, but this is incredibly unlikely. It's only used for
display purposes anyway."
  (when (string-match (format " Destination: \\([^\n]+\\)-%s\\."
                              (youtube-dl-item-id item))
                      output)
    (match-string 1 output)))

(defun youtube-dl--filter (proc output)
  (let* ((item (plist-get (process-plist proc) :item))
         (progress (youtube-dl--progress output))
         (destination (unless (youtube-dl-item-title item)
                        (youtube-dl--destination item output))))
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
      (setf (youtube-dl-item-dest-name item) destination))
    (youtube-dl-redisplay)))

(defun youtube-dl--current ()
  "Return the item currently being downloaded."
  (when youtube-dl-process
    (plist-get (process-plist youtube-dl-process) :item)))

(defun youtube-dl--run ()
  "As necessary, start or switch to the highest priority item."
  (let ((item (youtube-dl--next))
        (current-item (youtube-dl--current)))
    (if (eq item current-item)
        (youtube-dl-redisplay) ; do nothing
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
               (url (youtube-dl-item-url item))
               (playable-p (youtube-dl-playable-p url))
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
                              "youtube-dl" nil youtube-dl-program
                              "--newline" "--no-color"
                              (nconc
                               (when youtube-dl-omit-mtime
                                 (list "--no-mtime"))
                               (when youtube-dl-restrict-filenames
                                 (list "--restrict-filenames"))
                               (when (and playable-p youtube-dl-include-metadata)
                                 (list "--add-metadata"))
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
                               `("--" ,url))))))
          (set-process-plist proc (list :item item))
          (set-process-sentinel proc #'youtube-dl--sentinel)
          (set-process-filter proc #'youtube-dl--filter)
          (setf youtube-dl-process proc))))
    (youtube-dl-redisplay)))

(defvar youtube-dl-url-input-history nil
  "URL input history for youtube-dl.")
(defvar youtube-dl-x nil)

(defun youtube-dl-request-url (&optional alternative)
  "Interactively requests URL from user.
Suggests URL under point if any or an alternative provided
as the optional argument. The alternative may be a list of variants."
  (let* ((url-under-point (thing-at-point 'url t))
         (default (nconc (and url-under-point (list url-under-point))
                         (if (listp alternative)
                             alternative
                           (list alternative)))))
    (setq youtube-dl-x default)
    (read-string (format "URL%s: "
                         (if default
                             (format " (default is %s)" (car default))
                           ""))
                 nil 'youtube-dl-url-input-history default)))

(defun youtube-dl--request-url ()
  "Interactively requests URL from user."
  (youtube-dl-request-url
   (and interprogram-paste-function
        (with-temp-buffer
          (insert (or (funcall interprogram-paste-function) ""))
          (thing-at-point 'url t)))))

(defun youtube-dl-request-immediate ()
  "Ask user about immediate download if necessary."
  (or (eq youtube-dl-immediate 'always)
      (and youtube-dl-immediate
           (y-or-n-p "Start download immediately? "))))

(defun youtube-dl--request-args ()
  "Interactively request download arguments from user."
  (list (youtube-dl--request-url)
        (youtube-dl-request-immediate)
        current-prefix-arg))

(defun youtube-dl-thing ()
  "Being invoked in the download listing buffer returns an item
under point. Otherwise requests URL from user. Returns result
as a list of one element suitable for use in `interactive' form."
  (list
   (or (and (eq (current-buffer) (youtube-dl--buffer))
            (youtube-dl--pointed-item))
       (youtube-dl--request-url))))

(defun youtube-dl--playlist-list (url)
  "For each video, return one plist with :index, :id,
:url, :playlist, :playlist-url, :title, and :description."
  (with-temp-buffer
    (when (zerop (call-process youtube-dl-program nil '(t nil) nil
                               "--ignore-config"
                               "--dump-json"
                               "--flat-playlist"
                               "--"
                               url))
      (setf (point) (point-min))
      (cl-loop with json-object-type = 'plist
               for index upfrom 1
               for video = (ignore-errors (json-read))
               while video
               collect (list :index       index
                             :id           (plist-get video :id)
                             :url          (or (plist-get video :webpage_url)
                                               (plist-get video :original_url))
                             :playlist     (plist-get video :playlist)
                             :playlist-url (and (plist-get video :playlist) url)
                             :description  (plist-get video :description)
                             :filesize     (plist-get video :filesize)
                             :duration     (plist-get video :duration)
                             :title        (plist-get video :title))))))

(defun youtube-dl-playlist-list (url)
  "Fetch playlist info from the URL. Return result as a list of plists."
  (message "Fetching playlist ...")
  (let ((result (youtube-dl--playlist-list url)))
    (unless result
      (error "Failed to fetch playlist (%s)." url))
    result))

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

(cl-defun youtube-dl-submit
    (videos
     &key immediate reverse extract-audio directory (first 1) (priority 0) slow display)
  "Submit listed videos to the download queue.

:immediate BOOL -- Submit for immediate download.

:reverse BOOL -- Reverse playlist order.

:extract-audio BOOL -- Extract audio content.

:directory PATH -- Destination directory for all videos.

:first INDEX -- Start downloading from a given one-based index.

:priority PRIORITY -- Use this priority for all download entries.

:slow BOOL -- Start all download entries in slow mode.

:display BOOL -- Pop up the listing positioned at the newly added content."
  (let* ((max (cl-loop for entry in videos
                       maximize (or (plist-get entry :index) 0)))
         (width (1+ (floor (log max 10))))
         (prefix-format (format "%%0%dd-" width))
         (position (length youtube-dl-items)))
    (when reverse
      (setf videos (youtube-dl--playlist-reverse videos)))
    (dolist (video (youtube-dl--playlist-cutoff videos first))
      (let* ((index (plist-get video :index))
             (playlist (plist-get video :playlist))
             (prefix (if (and playlist index)
                         (format prefix-format index)
                       ""))
             (title (format "%s%s" prefix
                            (or (plist-get video :title)
                                (plist-get video :id))))
             (dest (format "%s%s" prefix "%(title)s-%(id)s.%(ext)s"))
             (url (plist-get video :url))
             (full-dir (expand-file-name
                        (or directory
                            (and (not youtube-dl-restrict-filenames) playlist)
                            "")
                        (cond
                         (extract-audio
                          (expand-file-name (or youtube-dl-audio-folder "")
                                            youtube-dl-download-directory))
                         ((youtube-dl-playable-p url)
                          (expand-file-name (or youtube-dl-video-folder "")
                                            youtube-dl-download-directory))
                         (t youtube-dl-download-directory))))
             (item (youtube-dl-item--create
                    :id (plist-get video :id)
                    :url url
                    :description (plist-get video :description)
                    :filesize (plist-get video :filesize)
                    :duration (plist-get video :duration)
                    :title (plist-get video :title)
                    :dest-name title
                    :playlist playlist
                    :playlist-url (plist-get video :playlist-url)
                    :audio-p extract-audio
                    :failures 0
                    :priority priority
                    :paused-p (not immediate)
                    :slow-p slow
                    :directory full-dir
                    :destination dest)))
        (when (and (youtube-dl-item-id item)
                   (youtube-dl-item-url item))
          (setf youtube-dl-items (nconc youtube-dl-items (list item)))
          (youtube-dl--run))))
    (when display
      (youtube-dl-list position))))

;;;###autoload
(cl-defun youtube-dl
    (url
     &optional immediate reverse
     &key extract-audio directory (first 1) (priority 0) slow display)
  "Submit video pointed by URL to the download queue. If URL points
to a playlist, all its items are added with index prefixes.
If second argument is nil, the items start as paused.
If URL points to a playlist and third argument is non-nil
then playlist will be reversed.

:extract-audio BOOL -- Extract audio content.

:directory PATH -- Destination directory for all videos.

:first INDEX -- Start downloading from a given one-based index.

:priority PRIORITY -- Use this priority for all download entries.

:slow BOOL -- Start all download entries in slow mode.

:display BOOL -- Pop up the listing positioned at the newly added content."
  (interactive (youtube-dl--request-args))
  (youtube-dl-submit (youtube-dl-playlist-list url)
                     :immediate immediate
                     :reverse reverse
                     :extract-audio extract-audio
                     :directory directory
                     :first first
                     :priority priority
                     :slow slow
                     :display display))

;;;###autoload
(cl-defun youtube-dl-audio
    (url
     &optional immediate reverse
     &key directory (first 1) (priority 0) slow display)
  "Submit video pointed by URL to the download queue. If URL points
to a playlist, all its items are added with index prefixes.
Only audio content will be retrieved.
If second argument is nil, the item starts as paused.
If URL points to a playlist and third argument is non-nil
then playlist will be reversed.

:directory PATH -- Destination directory for all videos.

:first INDEX -- Start downloading from a given one-based index.

:priority PRIORITY -- Use this priority for all download entries.

:slow BOOL -- Start all download entries in slow mode.

:display BOOL -- Pop up the listing positioned at the newly added content."
  (interactive (youtube-dl--request-args))
  (youtube-dl url immediate reverse
              :extract-audio t
              :directory directory
              :first first
              :priority priority
              :slow slow
              :display display))

;; List user interface:

(defun youtube-dl-redisplay ()
  "Redraw the queue list and log buffers only if visible."
  (let ((listing-buffer (youtube-dl--buffer))
        (log-buffer (youtube-dl--log-buffer)))
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
                                (point-max)))))))
    (when (get-buffer-window listing-buffer)
      (with-current-buffer listing-buffer
        (let ((save-point (point))
              (window (get-buffer-window)))
          (youtube-dl--fill-listing)
          (setf (point) save-point)
          (when window
            (set-window-point window save-point))
          (when hl-line-mode
            (hl-line-highlight)))))))

(defun youtube-dl--pointed-item ()
  "Get item under point. Signal an error if none."
  (unless (eq (current-buffer) (youtube-dl--buffer))
    (error "The operation is not available in this buffer."))
  (let* ((index (get-text-property (point) 'youtube-dl-item-index))
         (item (and index (nth index youtube-dl-items))))
    (unless item
      (error "No item at this line."))
    item))

(defun youtube-dl-list-log ()
  "Display the log of the video under point."
  (interactive)
  (display-buffer (youtube-dl--log-buffer (youtube-dl--pointed-item)))
  (youtube-dl-redisplay))

(defun youtube-dl-list-kill-log ()
  "Kill the youtube-dl log buffer."
  (interactive)
  (let ((buffer (youtube-dl--log-buffer)))
    (when buffer
      (kill-buffer buffer))))

(defun youtube-dl-list-yank ()
  "Copy the URL of the video under point to the clipboard."
  (interactive)
  (let ((url (or (get-text-property (point) 'youtube-dl-playlist-url)
                 (youtube-dl-item-url (youtube-dl--pointed-item)))))
    (kill-new url)
    (message "Yanked %s" url)))

(defun youtube-dl--file-lists (items)
  "Returns list of existing files associated with the items listed
in argument. Each element of the result list in turn is a list
with directory path in car element and file names in the rest ones."
  (cl-loop for item in items
           for path = (youtube-dl-item-directory item)
           for files = (when (file-accessible-directory-p path)
                         (let ((default-directory path)
                               (pattern (format "*-%s.*" (youtube-dl-item-id item))))
                           (file-expand-wildcards pattern)))
           when files
           collect (cons path files)))

(defun youtube-dl--playlist-items (playlist)
  "Returns the list of playlist items."
  (cl-loop for item in youtube-dl-items
           when (equal (youtube-dl-item-playlist item) playlist)
           collect item))

(defun youtube-dl-list-kill (items &optional files)
  "Remove listed items from the queue. If a list of associated
files is specified as optional second argument they will be deleted
as well. Being invoked interactively in the download listing
operates on an item under point or on a playlist which header
is under point."
  (interactive
   (let* ((playlist (get-text-property (point) 'youtube-dl-playlist-title))
          (items
           (if playlist
               (and (yes-or-no-p "Kill entire playlist? ")
                    (youtube-dl--playlist-items playlist))
             (list (youtube-dl--pointed-item))))
          (files (youtube-dl--file-lists items)))
     (list items
           (and files
                (yes-or-no-p "Delete associated files as well? ")
                files))))
  (dolist (item items)
    (youtube-dl--remove item))
  (when items
    (youtube-dl--run))
  (when files
    (dolist (item-files files)
      (let ((default-directory (car item-files)))
        (dolist (file (cdr item-files))
          (delete-file file)))
      (unless (or (delete "." (delete ".." (directory-files (car item-files))))
                  (file-equal-p (car item-files) youtube-dl-download-directory)
                  (not (file-in-directory-p (car item-files) youtube-dl-download-directory)))
        (delete-directory (car item-files))))))

(defun youtube-dl-list-priority-modify (delta)
  "Change priority of item under point by DELTA."
  (cl-incf (youtube-dl-item-priority (youtube-dl--pointed-item)) delta)
  (youtube-dl--run))

(defun youtube-dl--pointed-thing ()
  "Returns an item or a playlist title under point wrapped
into a list suitable for use in `interactive' form."
  (list
   (or (get-text-property (point) 'youtube-dl-playlist-title)
       (youtube-dl--pointed-item))))

(defun youtube-dl-list-toggle-pause (thing)
  "Toggle pause on item under point or on the items of the playlist
which header is under point."
  (interactive (youtube-dl--pointed-thing))
  (if (youtube-dl-item-p thing)
      (let ((paused-p (youtube-dl-item-paused-p thing))
            (current (youtube-dl--current)))
        (setf (youtube-dl-item-paused-p thing) (not paused-p))
        (if (or (null current) (eq thing current))
            (youtube-dl--run)
          (youtube-dl-redisplay)))
    (youtube-dl-list-toggle-pause-all (youtube-dl--playlist-items thing))))

(defun youtube-dl-list-toggle-pause-all (items)
  "Toggle pause on items list. When paused items are in the minority,
all other items are paused, and vice versa. Being called
interactively operates on all items."
  (interactive (list youtube-dl-items))
  (let* ((count (length items))
         (paused-count (cl-count-if #'youtube-dl-item-paused-p items))
         (target (< paused-count (- count paused-count))))
    (dolist (item items)
      (unless (eq target (youtube-dl-item-paused-p item))
        (youtube-dl-list-toggle-pause item)))))

(defun youtube-dl-list-toggle-slow (thing)
  "Toggle slow mode on item under point or on the item of the playlist
which header is under point."
  (interactive (youtube-dl--pointed-thing))
  (if (youtube-dl-item-p thing)
      (let ((slow-p (youtube-dl-item-slow-p thing)))
        (setf (youtube-dl-item-slow-p thing) (not slow-p))
        (if (not (eq thing (youtube-dl--current)))
            (youtube-dl-redisplay)
          ;; Offset error count and restart the process.
          (cl-decf (youtube-dl-item-failures thing))
          (kill-process youtube-dl-process)))
    (youtube-dl-list-toggle-slow-all (youtube-dl--playlist-items thing))))

(defun youtube-dl-list-toggle-slow-all (items)
  "Toggle slow mode on items list. When slow items are in the minority,
all other items are made slow, and vice versa. Being called
interactively operates on all items."
  (interactive youtube-dl-items)
  (let* ((count (length items))
         (slow-count (cl-count-if #'youtube-dl-item-slow-p items))
         (target (< slow-count (- count slow-count))))
    (dolist (item items)
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

(defun youtube-dl-quit ()
  "Clear downloads queue and quit."
  (interactive)
  (youtube-dl-list-kill youtube-dl-items)
  (quit-window))

(defvar youtube-dl-list-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map "a" #'youtube-dl)
      (define-key map "l" #'youtube-dl-list-log)
      (define-key map "L" #'youtube-dl-list-kill-log)
      (define-key map "y" #'youtube-dl-list-yank)
      (define-key map " " #'youtube-dl-play)
      (define-key map "\r" #'youtube-dl-view)
      (define-key map "Q" #'youtube-dl-quit)
      (define-key map [down] #'youtube-dl-list-next-item)
      (define-key map [up] #'youtube-dl-list-prev-item)
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
        (format "%s%-6.6s %-10.10s %s"
                (propertize " " 'display '((space :align-to 0)))
                "done" "size" "title")))

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

(defconst youtube-dl-list-title-start-position 18
  "Title start position in the youtube-dl list buffer.")

(defun youtube-dl--fill-listing (&optional position)
  "Erase and redraw the queue in the queue listing buffer.
Optional argument specifies what to do with the cursor position
after filling. If it is `nil', cursor is leaved at the buffer end.
Integer value is treated as an item index. Cursor will be positioned
on this item or on playlist header if the item is the first one
in the playlist. Any other non-nil value implies positioning on the
active item or at the beginning of buffer if no active item exists."
  (with-current-buffer (youtube-dl--buffer)
    (let* ((inhibit-read-only t)
           (window (get-buffer-window (current-buffer)))
           (index 0)
           (target-point nil)
           (current-playlist nil)
           (active (youtube-dl--current))
           (string-audio (propertize "A" 'face 'youtube-dl-audio-content))
           (string-slow (propertize "S" 'face 'youtube-dl-slow))
           (string-paused (propertize "P" 'face 'youtube-dl-pause)))
      (erase-buffer)
      (dolist (item youtube-dl-items)
        (let* ((id (youtube-dl-item-id item))
               (failures (youtube-dl-item-failures item))
               (priority (youtube-dl-item-priority item))
               (progress (youtube-dl-item-progress item))
               (audio-p (youtube-dl-item-audio-p item))
               (paused-p (youtube-dl-item-paused-p item))
               (slow-p (youtube-dl-item-slow-p item))
               (total (youtube-dl-item-total item))
               (title
                (or (youtube-dl-item-dest-name item)
                    (youtube-dl-item-title item)
                    id))
               (playlist (youtube-dl-item-playlist item))
               (playlist-url (youtube-dl-item-playlist-url item))
               (start (point)))
          (unless (equal playlist current-playlist)
            (let ((indent-tabs-mode nil))
              (when current-playlist
                (indent-to youtube-dl-list-title-start-position)
                (insert "--- End of playlist ---\n"))
              (when playlist
                (setq start (point))
                (indent-to youtube-dl-list-title-start-position)
                (when (and (numberp position) (= index position))
                  (setq target-point (point)))
                (insert "*** " (propertize playlist 'face 'youtube-dl-playlist-title) " ***")
                (put-text-property start (point) 'youtube-dl-playlist-title playlist)
                (put-text-property start (point) 'youtube-dl-playlist-url playlist-url)
                (insert "\n")))
            (setq start (point)
                  current-playlist playlist))
          (insert
           (format "%-6.6s %-10.10s %s%s%s%s"
                   (or progress "0.0%")
                   (or total "???")
                   (if (zerop failures)
                       ""
                     (propertize (format "[%d] " failures)
                                 'face 'youtube-dl-failure))
                   (if (zerop priority)
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
                   (if (eq active item)
                       (propertize title 'face 'youtube-dl-active)
                     title)))
          (put-text-property start (point) 'youtube-dl-item-index index)
          (when (and (null target-point)
                     (or (and (numberp position) (= index position))
                         (and position (eq active item))))
            (setq target-point (+ start youtube-dl-list-title-start-position)))
          (setq index (1+ index))
          (insert "\n")))
      (when current-playlist
        (let ((indent-tabs-mode nil))
          (indent-to youtube-dl-list-title-start-position)
          (insert "--- End of playlist ---\n")))
      (if target-point
          (goto-char target-point)
        (when position
          (goto-char (+ (point-min) youtube-dl-list-title-start-position))))
      (when window
        (set-window-point window (point))))))

;;;###autoload
(defun youtube-dl-list (&optional position)
  "Display a list of all videos queued for download.
Optional argument specifies what to do with the cursor position
afterwards. Integer value is treated as an item index. Cursor
will be positioned on this item or on playlist header if the item
is the first one in the playlist. Any non-number value implies
positioning on the active item or at the beginning of buffer if
no active item exists."
  (interactive)
  (youtube-dl--fill-listing (or (and (numberp position) position) t))
  (pop-to-buffer (youtube-dl--buffer)))

(defun youtube-dl-list-next-item ()
  "Move to the next item."
  (interactive)
  (unless (zerop (forward-line 1))
    (error "End of buffer"))
  (unless (eobp)
    (forward-char youtube-dl-list-title-start-position)))

(defun youtube-dl-list-prev-item ()
  "Move to the previous item."
  (interactive)
  (when (<= (line-number-at-pos) 1)
    (error "Beginning of buffer"))
  (forward-line -1)
  (forward-char youtube-dl-list-title-start-position))

;;;###autoload
(defun youtube-dl-customize ()
  "Customize youtube-dl options."
  (interactive)
  (customize-group 'youtube-dl))

(provide 'youtube-dl)

;;; youtube-dl.el ends here
