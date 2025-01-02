(let ((make-backup-files nil)
      (generated-autoload-file (expand-file-name (car command-line-args-left))))
  (mapc
   #'(lambda (source-file)
       (update-file-autoloads source-file t))
   (cdr command-line-args-left)))
