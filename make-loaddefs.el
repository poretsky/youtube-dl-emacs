(let ((make-backup-files nil)
      (generated-autoload-file (expand-file-name (car command-line-args-left)))
      (src-dir (file-name-directory load-file-name)))
  (if (locate-library "loaddefs-gen")
      (loaddefs-generate src-dir (car command-line-args-left) nil nil nil t)
    (require 'autoload)
    (update-directory-autoloads src-dir)))

;; Local Variables:
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; End:
