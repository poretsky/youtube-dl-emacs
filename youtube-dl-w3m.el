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
(require 'advice)
(require 'youtube-dl)
(cl-eval-when (load)
  (require 'w3m))

(declare-function w3m-view-this-url "w3m")
(declare-function w3m-goto-url "w3m" (url))

;;;###autoload
(defgroup youtube-dl-w3m ()
  "W3m browser integration related options."
  :group 'youtube-dl)

(defcustom youtube-dl-w3m-downloadable-urls
  '("https?://\\(?:\\(?:www\\|music\\)\\.\\)?youtube\\.com/.+"
    "https?://music.yandex.ru/album/[0-9]\\{8\\}"
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

(defconst youtube-dl-w3m-native-youtube-base-url
  "https?://\\(?:\\(?:\\(?:www\\|music\\)\\.\\)?youtube\\.com\\|youtu\\.be\\)"
  "A regexp that matches all variants of the Youtube URl base
that can be replaced by the Invidious URL stub.")

(defcustom youtube-dl-w3m-invidious-url "https://yewtu.be/"
  "Base URL of the Invidious mirror in use.
All Youtube requests will be redirected to it.
If it is `nil', the redirection is disabled."
  :group 'youtube-dl-w3m
  :type '(choice (const nil) string))

(defun youtube-dl-w3m--downloadable-p (url)
  "Test given URL if it is downloadable."
  (let ((youtube-dl-playable-urls
         (nconc (cl-copy-list youtube-dl-playable-urls)
                youtube-dl-w3m-downloadable-urls)))
    (youtube-dl-playable-p url)))

(defun youtube-dl-w3m--current-anchor ()
  "Get an URL from current anchor if any."
  (get-text-property (point) 'w3m-href-anchor))

(defun youtube-dl-w3m--guess-url ()
  "Guess an URL for operation or request it from user."
  (cl-declare (special w3m-current-url))
  (or (youtube-dl-w3m--current-anchor)
      (youtube-dl-request-url w3m-current-url)))

(defun youtube-dl-w3m--request-args ()
  "Interactively request download arguments from user."
  (list (youtube-dl-w3m--guess-url)
        (youtube-dl-request-immediate)
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
  (youtube-dl url immediate reverse
              :extract-audio audio-only
              :display t))

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

(defun youtube-dl-w3m-view (url)
  "Shows video description for specified URL. Being called interactively
uses an anchor at point. If no anchor exists at point, requests an URL
from user suggesting reasonable default."
  (interactive (list (youtube-dl-w3m--guess-url)))
  (youtube-dl-view url))

(defvar youtube-dl-w3m-menu
  (let ((menu (make-sparse-keymap "YouTube download and playback")))
    (define-key menu [schedule]
      '(menu-item "Submit download" youtube-dl-w3m))
    (define-key menu [schedule-audio]
      '(menu-item "Submit download audio" youtube-dl-w3m-audio))
    (define-key menu [play]
      '(menu-item "Play video clip" youtube-dl-w3m-play))
    (define-key menu [view]
      '(menu-item "View video clip info" youtube-dl-w3m-view))
    (define-key menu [list]
      '(menu-item "Show download queue" youtube-dl-list))
    menu)
  "Actions menu for use in w3m.")

(defun youtube-dl-w3m-menu-popup ()
  "Pops up the actions menu."
  (interactive)
  (tmm-prompt youtube-dl-w3m-menu))

(defun youtube-dl-w3m-dispatch ()
  "Dispatches link visiting operation depending on the link nature.
Uses `w3m-view-this-url' as a fallback."
  (interactive)
  (let ((url (youtube-dl-w3m--current-anchor))
        (on-invidious-page
         (and youtube-dl-w3m-invidious-url
              (string-match (youtube-dl-w3m--invidious-url-pattern) w3m-current-url)))
        (youtube-url (concat "^" youtube-dl-w3m-native-youtube-base-url "\\(?:/\\|$\\)")))
    (cond
     ((or current-prefix-arg
          (not (stringp url))
          (and youtube-dl-w3m-invidious-url
               (not on-invidious-page)
               (not (string-match youtube-url w3m-current-url))))
      (call-interactively 'w3m-view-this-url))
     ((and on-invidious-page
           (string-prefix-p (substring w3m-current-url 0 (string-match "\\(?:&\\|$\\)" w3m-current-url)) url)
           (string-match "&t=\\([0-9]+\\)" url))
      (youtube-dl-play
       (replace-match "" nil t url)
       (match-string 1 url)))
     ((and on-invidious-page
           (string-match youtube-url url))
      (youtube-dl-w3m-menu-popup))
     ((and youtube-dl-w3m-auto-play
           (youtube-dl-playable-p url)
           (or (eq youtube-dl-w3m-auto-play 'always)
               (eq youtube-dl-w3m-auto-play 'preview)
               (y-or-n-p "Start playback? ")))
      (if (eq youtube-dl-w3m-auto-play 'preview)
          (youtube-dl-view url)
        (youtube-dl-play url)))
     ((and youtube-dl-w3m-auto-download
           (youtube-dl-w3m--downloadable-p url)
           (or (eq youtube-dl-w3m-auto-download 'always)
               (y-or-n-p "Schedule download? ")))
      (youtube-dl-w3m url))
     (t (call-interactively 'w3m-view-this-url)))))

(cl-loop
 for f in
 '(w3m-process-stop w3m-quit)
 do
 (eval
  `(defadvice ,f (before youtube-dl pre act comp)
     "Stop youtube video playback if any."
     (when (and (featurep 'youtube-dl-play)
                (called-interactively-p 'any))
       (youtube-dl-play-stop)))))

;; Invidious integration:

(defun youtube-dl-w3m--invidious-url-pattern ()
  "Construct regexp matching Invidious URL stub."
  (concat "^"
          (if (string-suffix-p "/" youtube-dl-w3m-invidious-url)
              (substring youtube-dl-w3m-invidious-url 0 -1)
            youtube-dl-w3m-invidious-url)
          "\\(?:/\\|$\\)"))

(defun youtube-dl-w3m-invidious-filter (_url)
  "Improve invidious pages display."
  ;; Split multiparagraph links
  (goto-char (point-min))
  (while (re-search-forward
          "<a[ \t\r\n\f]+\\(?:[^>]*[ \t\r\n\f]\\)?href=[^>]+>"
          nil t)
    (let ((anchor (match-string 0))
          (balance nil))
      (while (and anchor
                  (re-search-forward
                   "<\\(?:\\(p\\(?:[ \t\r\n\f][^>]+\\)?\\)\\|/\\(?:\\(p\\)\\|a\\)\\)>"
                   nil t))
        (cond
         ((match-string 1)
          (if balance
              (setq balance nil)
            (replace-match "</a>\\&"))
          (insert anchor))
         ((match-string 2)
          (unless balance
            (replace-match "</a>\\&")
            (setq balance t)))
         (t (setq anchor nil)
            (when balance
              (delete-region (match-beginning 0) (match-end 0))))))))
  ;;Get anchor title or id attribute visible
  (goto-char (point-min))
  (while (re-search-forward
          "<a[ \t\r\n\f]+\\(?:[^>]*[ \t\r\n\f]\\)?href=[^>]+>"
          nil t)
    (let ((anchor (match-string 0)))
      (when (and (looking-at "[ \t\r\n\f]*<")
                 (or (string-match "[ \t\r\n\f]title=\"\\([^\"]*\\)\"[ \t\r\n\f]" anchor)
                     (string-match "[ \t\r\n\f]title=\'\\([^\']*\\)\'[ \t\r\n\f]" anchor)
                     (string-match "[ \t\r\n\f]id=\"\\([^\"]*\\)\"[ \t\r\n\f]" anchor)
                     (string-match "[ \t\r\n\f]id=\'\\([^\']*\\)\'[ \t\r\n\f]" anchor)))
        (insert "\n")
        (insert (match-string 1 anchor))
        (insert "\n"))))
  (goto-char (point-min))
  ;; Correct clip links
  (let ((clip-url
         (concat "\""
                 youtube-dl-w3m-native-youtube-base-url
                 "/watch[^\"]+\\(&list=[^&\"]+\\)[&\"]")))
    (while (re-search-forward clip-url nil t)
      (replace-match "" nil t nil 1)))
  (goto-char (point-min))
  ;; Correct description view
  (when (re-search-forward "<div id=\"description-box\">" nil t)
    ;; Remove unneeded checkbox
    (let ((box-start (point)))
      (when (re-search-forward "<input id=\"descexpansionbutton\".*/>" nil t)
        (delete-region (match-beginning 0) (match-end 0)))
      (goto-char box-start)))
  (when (re-search-forward "<div id=\"descriptionWrapper\">" nil t)
    ;; Strings and paragraphs
    (let ((description-start (point)))
      (while (and (re-search-forward "\\($\\)\\|</div>" nil t)
                  (match-string 1))
        (if (not (looking-at "\n *$"))
            (insert "<br/>")
          (goto-char (match-end 0))
          (insert "<p/>"))
        (forward-char))
      (goto-char description-start)))
  ;; Time marks list
  (let ((time-mark "\\(\\(.*<a \\(?:[^>]* \\)data-jump-time=.+\\)\\(?:<br/>\\)?\\)\\($\\|</div>\\)"))
    (when (re-search-forward time-mark nil t)
      (replace-match "<ul>\n<li>\\2" nil nil nil 1)
      (while (re-search-forward time-mark nil t)
        (replace-match "<li>\\2" nil nil nil 1))
      (insert "\n</ul>"))))

(defadvice w3m-filter (around youtube-dl pre act comp)
  "Apply invidious filters."
  (if youtube-dl-w3m-invidious-url
      (let ((w3m-filter-configuration (cl-copy-list w3m-filter-configuration))
            (invidious-url (youtube-dl-w3m--invidious-url-pattern)))
        (cl-pushnew
         `(t "Invidious pages view improvement" ,invidious-url youtube-dl-w3m-invidious-filter)
         w3m-filter-configuration)
        ad-do-it)
    ad-do-it))

(defadvice w3m-uri-replace (around youtube-dl pre act comp)
  "Redirect Youtube requests to Invidious."
  (if youtube-dl-w3m-invidious-url
      (let* ((w3m-uri-replace-alist (cl-copy-list w3m-uri-replace-alist))
             (invidious-url
              (if (string-suffix-p "/" youtube-dl-w3m-invidious-url)
                  youtube-dl-w3m-invidious-url
                (concat youtube-dl-w3m-invidious-url "/")))
             (youtube-url (concat "^" youtube-dl-w3m-native-youtube-base-url "\\(?:/\\|$\\)"))
             (youtube-clip "^https?://youtu\\.be/\\([-_a-zA-Z0-9]\\{11\\}\\)")
             (youtube-clip-xt (concat youtube-clip "\\?"))
             (invidious-clip (concat invidious-url "watch?v=\\1"))
             (invidious-clip-xt (concat invidious-clip "&")))
        (cl-pushnew
         `(,youtube-url w3m-pattern-uri-replace ,invidious-url)
         w3m-uri-replace-alist)
        (cl-pushnew
         `(,youtube-clip w3m-pattern-uri-replace ,invidious-clip)
         w3m-uri-replace-alist)
        (cl-pushnew
         `(,youtube-clip-xt w3m-pattern-uri-replace ,invidious-clip-xt)
         w3m-uri-replace-alist)
        ad-do-it)
    ad-do-it)
  ad-return-value)

(defadvice w3m-bookmark-add (around youtube-dl pre act comp)
  "Replace Invidious URL and title with the corresponding Youtube ones."
  (let ((url (ad-get-arg 0))
        (title (ad-get-arg 1)))
    (when (and youtube-dl-w3m-invidious-url
               (string-match (youtube-dl-w3m--invidious-url-pattern) url))
      (ad-set-arg 0 (replace-match "https://www.youtube.com/" nil t url))
      (when (and title (string-match " +- +Invidious$" title))
        (ad-set-arg 1 (replace-match "" nil t title))))
    ad-do-it))

(defun youtube-dl-w3m--invidious-check ()
  "Signal error when Invidious address is not specified."
  (unless youtube-dl-w3m-invidious-url
    (error "Invidious address is not specified")))

(defun youtube-dl-w3m-invidious ()
  "Visit Invidious."
  (interactive)
  (youtube-dl-w3m--invidious-check)
  (w3m-goto-url youtube-dl-w3m-invidious-url))

(defun youtube-dl-w3m-invidious-search ()
  "Search Youtube via Invidious."
  (interactive)
  (cl-declare (special w3m-search-default-engine w3m-search-engine-alist))
  (youtube-dl-w3m--invidious-check)
  (require 'w3m-search)
  (let* ((w3m-search-default-engine "youtube")
         (url
          (concat youtube-dl-w3m-invidious-url
                  (if (string-suffix-p "/" youtube-dl-w3m-invidious-url) "" "/")
                  "search?q=%s"))
         (w3m-search-engine-alist `((,w3m-search-default-engine ,url utf-8)))
         (current-prefix-arg nil))
    (call-interactively 'w3m-search)))

;; Bindings:

;;;###autoload
(defun youtube-dl-w3m-setup ()
  "Set up extra key bindings."
  (cl-declare (special w3m-mode-map))
  (define-key w3m-mode-map "y" #'youtube-dl-w3m-menu-popup)
  (define-key w3m-mode-map "Y" #'youtube-dl-w3m-invidious-search)
  (define-key w3m-mode-map "i" #'youtube-dl-w3m-invidious)
  (define-key w3m-mode-map "\r" #'youtube-dl-w3m-dispatch))

(provide 'youtube-dl-w3m)

;;; youtube-dl-w3m.el ends here
