(defmacro d/make-theme (name theme &rest side-effects)
  "Defines a variable NAME of form: (THEME . SIDE-EFFECTS). This
represents a theme and side-effects to call when loading it."
  `(defvar ,name (cons ,theme (quote ,side-effects))
 "Themes are of form (THEME . SIDE-EFFECTS) where SIDE-EFFECTS is
a list of functions to execute /before/ the theme is loaded. For
instance, you might want to adjust some font or disable all other
themes."))

(defun d/load-theme (name)
  "Load a theme defined with `d/make-theme'"
  (let ((side-effects (cdr name))
	(theme (car name)))
    (mapc #'eval side-effects)
    (load-theme theme t)))

(defun d/flip-theme ()
  "If theme is dark, switched to light and vice versa."
  (interactive)
  (let ((dark-or-light-p (or (eq theme-state 'dark) (eq theme-state 'light))))
    (if dark-or-light-p
	(if (eq theme-state 'dark)
	    (d/load-theme light-theme)
	  (d/load-theme dark-theme))
      (message "==I don't know whether the theme is light or dark!"))))


(provide 'better-theme-switching)
