;; simple function to toggle display of dotfiles in dired
(defun d-dired-dotfiles-toggle ()
  "Show/hide dot-files"
  (interactive)
  (when (equal major-mode 'dired-mode)
    (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p) ; if currently showing
        (progn
          (set (make-local-variable 'dired-dotfiles-show-p) nil)
          (dired-mark-files-regexp "^\\\.")
          (dired-do-kill-lines))
      (progn (revert-buffer) ; otherwise just revert to re-show
             (set (make-local-variable 'dired-dotfiles-show-p) t)))))


(provide 'd-dired)


;; TODO compress on a separate thread

;; (defvar dired-compress-file-suffixes
;;   '(
;;     ;; "tar -zxf" isn't used because it's not available on the
;;     ;; Solaris10 version of tar. Solaris10 becomes obsolete in 2021.
;;     ;; Same thing on AIX 7.1.
;;     ("\\.tar\\.gz\\'" "" "gzip -dc %i | tar -xf -")
;;     ("\\.tgz\\'" "" "gzip -dc %i | tar -xf -")
;;     ("\\.gz\\'" "" "gunzip")
;;     ("\\.Z\\'" "" "uncompress")
;;     ;; For .z, try gunzip.  It might be an old gzip file,
;;     ;; or it might be from compact? pack? (which?) but gunzip handles both.
;;     ("\\.z\\'" "" "gunzip")
;;     ("\\.dz\\'" "" "dictunzip")
;;     ("\\.tbz\\'" ".tar" "bunzip2")
;;     ("\\.bz2\\'" "" "bunzip2")
;;     ("\\.xz\\'" "" "unxz")
;;     ("\\.zip\\'" "" "unzip -o -d %o %i")
;;     ("\\.7z\\'" "" "7z x -aoa -o%o %i")
;;     ;; This item controls naming for compression.
;;     ("\\.tar\\'" ".tgz" nil)
;;     ;; This item controls the compression of directories
;;     (":" ".tar.gz" "tar -cf - %i | gzip -c9 > %o"))
;;   "Control changes in file name suffixes for compression and uncompression.
;; Each element specifies one transformation rule, and has the form:
;;   (REGEXP NEW-SUFFIX PROGRAM)
;; The rule applies when the old file name matches REGEXP.
;; The new file name is computed by deleting the part that matches REGEXP
;;  (as well as anything after that), then adding NEW-SUFFIX in its place.
;; If PROGRAM is non-nil, the rule is an uncompression rule,
;; and uncompression is done by running PROGRAM.

;; Within PROGRAM, %i denotes the input file, and %o denotes the
;; output file.

;; Otherwise, the rule is a compression rule, and compression is done with gzip.
;; ARGS are command switches passed to PROGRAM.")

;; (defvar dired-compress-files-alist
;;   '(("\\.tar\\.gz\\'" . "tar -cf - %i | gzip -c9 > %o")
;;     ("\\.tar\\.bz2\\'" . "tar -cf - %i | bzip2 -c9 > %o")
;;     ("\\.tar\\.xz\\'" . "tar -cf - %i | xz -c9 > %o")
;;     ("\\.zip\\'" . "zip %o -r --filesync %i"))
;;   "Control the compression shell command for `dired-do-compress-to'.

;; Each element is (REGEXP . CMD), where REGEXP is the name of the
;; archive to which you want to compress, and CMD is the
;; corresponding command.

;; Within CMD, %i denotes the input file(s), and %o denotes the
;; output file. %i path(s) are relative, while %o is absolute.")



;; ;;;; ===========================================================================
;; ;;;;              redefine dired compresssion funcs to use async proc 

;; (defun dired-compress ()
;;   ;; Compress or uncompress the current file.
;;   ;; Return nil for success, offending filename else.
;;   (let* (buffer-read-only
;; 	 (from-file (dired-get-filename))
;; 	 (new-file (dired-compress-file from-file)))
;;     (if new-file
;; 	(let ((start (point)))
;; 	  ;; Remove any preexisting entry for the name NEW-FILE.
;; 	  (ignore-errors (dired-remove-entry new-file))
;; 	  (goto-char start)
;; 	  ;; Now replace the current line with an entry for NEW-FILE.
;; 	  (dired-update-file-line new-file) nil)
;;       (dired-log (concat "Failed to compress" from-file))
;;       from-file)))

;; (defun dired-do-compress-to ()
;;   "Compress selected files and directories to an archive.
;; Prompt for the archive file name.
;; Choose the archiving command based on the archive file-name extension
;; and `dired-compress-files-alist'."
;;   (interactive)
;;   (let* ((in-files (dired-get-marked-files))
;;          (out-file (expand-file-name (read-file-name "Compress to: ")))
;;          (rule (cl-find-if
;;                 (lambda (x)
;;                   (string-match (car x) out-file))
;;                 dired-compress-files-alist)))
;;     (cond ((not rule)
;;            (error
;;             "No compression rule found for %s, see `dired-compress-files-alist'"
;;             out-file))
;;           ((and (file-exists-p out-file)
;;                 (not (y-or-n-p
;;                       (format "%s exists, overwrite?"
;;                               (abbreviate-file-name out-file)))))
;;            (message "Compression aborted"))
;;           (t
;;            (when (zerop
;;                   (dired-shell-command
;;                    (format-spec (cdr rule)
;;                                 `((?\o . ,(shell-quote-argument out-file))
;;                                   (?\i . ,(mapconcat
;;                                            (lambda (file-desc)
;;                                              (shell-quote-argument (file-name-nondirectory
;;                                                                     file-desc)))
;;                                            in-files " "))))))
;;              (message "Compressed %d file(s) to %s"
;;                       (length in-files)
;;                       (file-name-nondirectory out-file)))))))


;; ;; (defun dired-shell-command (cmd)
;; ;;   "Run CMD, and check for output.
;; ;; On error, pop up the log buffer.
;; ;; Return the result of `process-file' - zero for success."
;; ;;   (let ((out-buffer " *dired-check-process output*")
;; ;;         (dir default-directory))
;; ;;     (with-current-buffer (get-buffer-create out-buffer)
;; ;;       (erase-buffer)
;; ;;       (let* ((default-directory dir)
;; ;;              (res (process-file
;; ;;                    shell-file-name
;; ;;                    nil
;; ;;                    t
;; ;;                    nil
;; ;;                    shell-command-switch
;; ;;                    cmd)))
;; ;;         (unless (zerop res)
;; ;;           (pop-to-buffer out-buffer))
;; ;;         res))))

;; (defun dired-do-compress (&optional arg)
;;   "Compress or uncompress marked (or next ARG) files.
;; If invoked on a directory, compress all of the files in
;; the directory and all of its subdirectories, recursively,
;; into a .tar.gz archive.
;; If invoked on a .tar.gz or a .tgz or a .zip or a .7z archive,
;; uncompress and unpack all the files in the archive."
;;   (interactive "P")
;;   (dired-map-over-marks-check #'dired-compress arg 'compress t))

;; (defun dired-compress-file (file)
;;   "Compress or uncompress FILE.
;; Return the name of the compressed or uncompressed file.
;; Return nil if no change in files."
;;   (let ((handler (find-file-name-handler file 'dired-compress-file))
;;         suffix newname
;;         (suffixes dired-compress-file-suffixes)
;;         command)
;;     ;; See if any suffix rule matches this file name.
;;     (while suffixes
;;       (let (case-fold-search)
;;         (if (string-match (car (car suffixes)) file)
;;             (setq suffix (car suffixes) suffixes nil))
;;         (setq suffixes (cdr suffixes))))
;;     ;; If so, compute desired new name.
;;     (if suffix
;;         (setq newname (concat (substring file 0 (match-beginning 0))
;;                               (nth 1 suffix))))
;;     (cond (handler
;;            (funcall handler 'dired-compress-file file))
;;           ((file-symlink-p file)
;;            nil)
;;           ((and suffix (setq command (nth 2 suffix)))
;;            (if (string-match "%[io]" command)
;;                (prog1 (setq newname (file-name-as-directory newname))
;;                  (dired-shell-command
;;                   (replace-regexp-in-string
;;                    "%o" (shell-quote-argument newname)
;;                    (replace-regexp-in-string
;;                     "%i" (shell-quote-argument file)
;;                     command
;;                     nil t)
;;                    nil t)))
;;              ;; We found an uncompression rule.
;;              (when (not
;;                     (dired-check-process
;;                      (concat "Uncompressing " file)
;;                      command
;;                      file))
;;                newname)))
;;           (t
;;            ;; We don't recognize the file as compressed, so compress it.
;;            ;; Try gzip; if we don't have that, use compress.
;;            (condition-case nil
;;                (if (file-directory-p file)
;;                    (progn
;;                      (setq suffix (cdr (assoc ":" dired-compress-file-suffixes)))
;;                      (when suffix
;;                        (let ((out-name (concat file (car suffix)))
;;                              (default-directory (file-name-directory file)))
;;                          (dired-shell-command
;;                           (replace-regexp-in-string
;;                            "%o" (shell-quote-argument out-name)
;;                            (replace-regexp-in-string
;;                             "%i" (shell-quote-argument (file-name-nondirectory file))
;;                             (cadr suffix)
;;                             nil t)
;;                            nil t))
;;                          out-name)))
;;                  (let ((out-name (concat file ".gz")))
;;                    (and (or (not (file-exists-p out-name))
;;                             (y-or-n-p
;;                              (format "File %s already exists.  Really compress? "
;;                                      out-name)))
;;                         (not
;;                          (dired-check-process (concat "Compressing " file)
;;                                               "gzip" "-f" file))
;;                         (or (file-exists-p out-name)
;;                             (setq out-name (concat file ".z")))
;;                         ;; Rename the compressed file to NEWNAME
;;                         ;; if it hasn't got that name already.
;;                         (if (and newname (not (equal newname out-name)))
;;                             (progn
;;                               (rename-file out-name newname t)
;;                               newname)
;;                           out-name))))
;;              (file-error
;;               (if (not (dired-check-process (concat "Compressing " file)
;;                                             "compress" "-f" file))
;;                   ;; Don't use NEWNAME with `compress'.
;;                   (concat file ".Z"))))))))
