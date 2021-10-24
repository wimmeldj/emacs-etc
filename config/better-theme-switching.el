(require 'ring)

(defvar d/theme-ring (make-ring 1)
  "Members are of form (THEME-NAME BEFORE AFTER)")

(defvar d/current-theme nil
  "Tracks current theme.")

(defun d/add-theme (theme before after)
  (ring-insert-at-beginning d/theme-ring (list theme before after))) ;last shall be last

(defmacro d-defthemes (&rest themes)
  "a THEME is
(THEME-NAME [:before SIDE-EFFECTS] [:after SIDE-EFFECTS])

THEME-NAME is a name given to `deftheme', eg. leuven

A theme can optionally contain properties after THEME-NAME which
are lists whose members will be `eval'ed before and after the
theme is loaded."
  `(progn
     (setq d/theme-ring (make-ring (length ',themes)))
     (mapc (lambda (theme)
             (let ((theme (car theme))
                   (side-effects (cdr theme)))
               (d/add-theme theme
                            (plist-get side-effects :before)
                            (plist-get side-effects :after))))
           ',themes)))

(defun d-load-next-theme ()
  "Loads the next theme on `d/theme-ring' and evals the before
and after side-effects. Loads the first member the first time
it's called."
  (interactive)
  (let* ((theme (if d/current-theme
                    (ring-next d/theme-ring d/current-theme)
                  (ring-ref d/theme-ring 0)))
         (theme-name (car theme))
         (before (cadr theme))
         (after (caddr theme)))
    (mapc #'eval before)
    (load-theme theme-name t)
    (mapc #'eval after)
    (setq d/current-theme theme)
    t))

(provide 'better-theme-switching)
