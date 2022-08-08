(require 'better-theme-switching "better-theme-switching")

;; get rid of greeting screen
(setq inhibit-startup-message t)
(setq inhibit-startup-screen 1)

;; declutter view
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode 1)
;; (display-battery-mode 1)
;; (display-time-mode 1)

;; scrolling
(setq mouse-wheel-scroll-amount '(1)    ;1 line at a time
      inhibit-compacting-font-caches t  ;some gc thing
      scroll-conservatively 101         ;
      mouse-wheel-progressive-speed t   ;faster mouse wheel -> faster text scrolling
      )

;; show column numbers
(column-number-mode)

;; enable cursorline
(global-hl-line-mode t)

;; globally preffiy symbols e.g. <=, or, lambda, ...
(global-prettify-symbols-mode t) 

;; highlight matching paren when point is on top of one. Applies to braces, brackets, etc.
(show-paren-mode 1)
(setq show-paren-style 'expression)

;; wrap lines somewhat intelligently. Would still like some sort of indentation of wrapped line
(setq-default word-wrap t)

;; default window dimensions 130x150 and 200 pixels down. X unspecified. This
;; works for emacsclient as well
(when (display-graphic-p)
  (setq default-frame-alist
        '((width . 130)
          (height . 100)
          (top . 200))))

(require 'org)
;; terminus doens't have italics or bold
;; (add-to-list 'org-emphasis-alist
;;             '("*" . ((t (:background "yellow" :foreground "black"))))) ;org bold
;; (add-to-list 'org-emphasis-alist
;;             '("~" . ((t (:background "black" :foreground "green"))))) ;org code

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (diminish 'which-key-mode)
  :after
  (diminish))

;; easily see cursor
(use-package beacon
  :ensure t
  :config
  (beacon-mode 1)
  (diminish 'beacon-mode)
  :after (diminish))

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; this is a dumb mode that makes things like #FFFFFF colored in particular modes
;; (use-package rainbow-mode
;;   :ensure t)

;; (use-package gruvbox-theme :ensure t)
;; (use-package modus-operandi-theme :ensure t)
;; (use-package modus-vivendi-theme :ensure t)
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/gruvbox")

(d-defthemes
 (leuven
  :before
  ((mapc #'disable-theme custom-enabled-themes) ;disable all themes currently enabled
   )

  :after
  ((custom-set-faces '(ivy-current-match ((((class color) (background light))
                                           (:background "#1a4b77" :foreground "white" :extend t))
                                          (((class color) (background dark))
                                           (:background "#65a7e2" :foreground "black" :extend t)))))
   (message "==loaded light theme")
   ))

 (spacemacs-dark
  :before
  ((mapc #'disable-theme custom-enabled-themes)
   )

  :after
  ((custom-set-faces
    ;; bright green ivy selection
    '(ivy-current-match ((t (:foreground "chartreuse3" :underline t :weight bold)))))

   (message "==loaded dark theme"))
  ))

;; loads first theme. Subsequent calls load the next
(d-load-next-theme)


(provide 'visual)
