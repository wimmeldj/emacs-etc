(require 'em-alias)

(defun read-shell-lines (path)
  "Returns contents of file at PATH as a list of lines. Any lines
broken into multiple using a backslash are coalesced into one"
  (with-temp-buffer
    (insert-file-contents path)
    (beginning-of-buffer)
    ;; join all lines that contain '\' at the end
    (while (not (eobp))
      (move-end-of-line nil)
      (while (equal (char-before) ?\\)
            (delete-backward-char 1)
            (join-line t) (delete-forward-char 1) ;join line with no space
            (move-end-of-line nil))
      (next-line))
    ;; with all backslashed lines coalesced, we can just split by newline
    (split-string (buffer-string) "\n" t "\s")))

(defun eshell-parse-aliases (path disallowed &rest eshell-aliases)
  "reads lines from PATH where a standard shell rc script is
expected. Lines defining aliases will be read from the file and
parsed into eshell syntax. Misbehaved --options in eshell are
excluded from the parsed commands by providing an alist in
DISALLOWED.

DISALLOWED is an alist where each car is the command to associate
the options with and each cdr is a list of regexeps matching
disallowed options. For instance:

((\"ls\" '(\"-l\" \"--human-readable\" \"--block-size=[[:word:]]+\")
  \"ln\" '(\"-s [[:word:]]+ [[:word:]]+\")))

The above alist tells the parser that any aliases for ls should
not include the following options:
-l
--human-readable
--blocksize=BLOCKSIZE

and for ln:
-s TARGET LINK_NAME

Which would mean you can't symbolically link with an alias
for ln.

Additional strings in eshell syntax can be provided through
ESHELL-ALIASES."
  (cl-flet ((exclude? (which)
                      (and (assoc which disallowed)
                           (not (cdr (assoc which disallowed)))))
            (filter-opts (which options)
                         (let ((pair (assoc which disallowed))
                               (filtered-options options))
                           (if (not pair)
                               options ;if not found, all options are fine, otherwise filter
                             (progn
                               (loop for bad-opt in (cdr pair) do
                                     (setf filtered-options
                                           (s-replace-regexp bad-opt "" filtered-options t)))
                               (s-trim (s-replace-regexp "\s+" " " filtered-options t)))))))
    (let ((lines (read-shell-lines path))
          cmd alias which options)
      (with-temp-buffer
        (dolist (line lines)
          (setf line (s-trim (s-replace-regexp "\s\s*" " " line t)))
          (when (string-match "^\\([[:word:]]+\\) \\([^=][^=]*\\)=\"\\([[:word:]]+\\)[ ]?\\(.*\\)\"$"
                              line)
            (setf cmd (match-string 1 line)
                  alias (match-string 2 line)
                  which (match-string 3 line)
                  options (match-string 4 line))
            (when (equalp "alias" cmd)
              (when (not (exclude? which))
                (insert (format "alias %s %s %s $*\n"
                                alias which (filter-opts which options)))))))
        (dolist (alias eshell-aliases)
          (insert (format "%s\n" alias)))
        (buffer-string)))))

;; USAGE:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The above means: when parsing the rc file, we want to ignore the ;;
;; options "--classify" and "--color=SOMEWORD" for all aliases of   ;;
;; ls. Also, we want to completely ignore all aliases for top (cdr  ;;
;; is nil instead of a list of regexps matching disallowed          ;;
;; options). Lastly, we define an alias for top in eshell syntax    ;;
;; which will try to call the function helm-top.                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (add-hook 'eshell-alias-load-hook 'write-eshell-aliases)

(provide 'eshell-mods)
