
(defun d/ensure-dir-exists (path)
  (when (not (file-exists-p path))
    (mkdir path)))

(defun d/increment-char-at-point ()
  "Increment number or character at point."
  (interactive)
  (condition-case nil
      (save-excursion
        (let ((chr  (1+ (char-after))))
          (unless (characterp chr) (error "Cannot increment char by one"))
          (delete-char 1)
          (insert chr)))
    (error (error "No character at point"))))

(defun d/copy-file:line ()
  "Copy /full/path/file:line to clipboard"
  (interactive)
  (kill-new (format "%s:%d" (buffer-file-name) (line-number-at-pos))))

(defun d/switch-to-scratch-and-back (&optional arg)
  "Toggle between *scratch-MODE* buffer and the current buffer.
If a scratch buffer does not exist, create it with the major mode set to that
of the buffer from where this function is called.

        COMMAND -> Open/switch to a scratch buffer in the current buffer's major mode
    C-0 COMMAND -> Open/switch to a scratch buffer in `fundamental-mode'
    C-u COMMAND -> Open/switch to a scratch buffer in `org-mode'
C-u C-u COMMAND -> Open/switch to a scratch buffer in `emacs-elisp-mode'

Even if the current major mode is a read-only mode (derived from `special-mode'
or `dired-mode'), we would want to be able to write in the scratch buffer. So
the scratch major mode is set to `org-mode' for such cases.

Return the scratch buffer opened."
  (interactive "p")
  (if (and (or (null arg)               ; no prefix
               (= arg 1))
           (string-match-p "\\*scratch" (buffer-name)))
      (switch-to-buffer (other-buffer))
    (let* ((mode-str (cl-case arg
                       (0  "fundamental-mode") ; C-0
                       (4  "org-mode") ; C-u
                       (16 "emacs-lisp-mode") ; C-u C-u
                       ;; If the major mode turns out to be a `special-mode'
                       ;; derived mode, a read-only mode like `help-mode', open
                       ;; an `org-mode' scratch buffer instead.
                       (t (if (or (derived-mode-p 'special-mode) ; no prefix
                                  (derived-mode-p 'dired-mode))
                              "org-mode"
                            (format "%s" major-mode)))))
           (buf (get-buffer-create (concat "*scratch-" mode-str "*"))))
      (switch-to-buffer buf)
      (funcall (intern mode-str))   ; http://stackoverflow.com/a/7539787/1219634
      buf)))

;; Makes an independent buffer (yanking text of current and putting it in a new one)
;; applies the current mode to the new scratch buffer
(defun d:make-scratch-buffer-from-current ()
  "Copied the current buffer, open scratch, paste it there."
  (interactive)
  (kill-ring-save (point-min) (point-max))
  (d-switch-to-scratch-and-back)
  (yank))

(provide 'd:misc)
