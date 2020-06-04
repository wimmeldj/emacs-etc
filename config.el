;; two alternative keymaps, C-; and C-/
(define-prefix-command 'semicolon-map)
(define-prefix-command 'fs-map)
(global-set-key (kbd "C-;") semicolon-map)
;; (global-set-key (kbd "C-/") fs-map)

;; don't allow execution of (save-buffers-kill-terminal) so easily
(global-unset-key (kbd "C-x C-c"))

(global-set-key (kbd "C-x C-b") #'ibuffer)

(global-set-key (kbd "C-x o") #'ace-window)

(global-set-key (kbd "s-SPC") #'golden-ratio)

(global-set-key (kbd "M-<f12>") #'d/flip-theme)

;; (global-set-key (kbd "M-s") #'avy-goto-char)

(define-key semicolon-map
  (kbd "C-u") #'browse-url)

(defun modi/switch-to-scratch-and-back (&optional arg)
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
(defun make-scratch-buffer-from-current ()
  "Copied the current buffer, open scratch, paste it there."
  (interactive)
  (kill-ring-save (point-min) (point-max))
  (modi/switch-to-scratch-and-back)
  (yank))

(defun kill-help ()
  "Kill help, wherever it is, so you don't have to jump to it."
  (interactive)
  (let ((help-buff (get-buffer "*Help*")))
    (when (buffer-live-p help-buff)
      (kill-buffer help-buff))))

(use-package csound-mode
  :ensure t)

(use-package dired-rsync
  :ensure t)

;; these both have essential nice functions for elisp
(use-package s :ensure t)		;string manip
(use-package f :ensure t)		;file manip

(require 'f)				;used several times in init

(require 'server)
(unless (server-running-p)
  (server-start))

;; get rid of greeting screen
;;(setq inhibit-startup-message t)

;; declutter view
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
;; (display-battery-mode 1)
;; (display-time-mode 1)
(setq inhibit-startup-screen 1)

;; show column numbers
(column-number-mode)

;; enable cursorline
(global-hl-line-mode t)

;; globally preffiy symbols e.g. <=, or, lambda, ...
(global-prettify-symbols-mode t) 

;; highlight matching paren when point is on top of one. Applies to braces, brackets, etc.
(show-paren-mode 1)

;; x clipboard support
(setq select-enable-clipboard t)
(setq x-select-enable-clipboard-manager t)

;; C-; C-q open recent files
(recentf-mode)
(define-key semicolon-map (kbd "C-q") 'recentf-open-files)

(setq ring-bell-function 'ignore)

;; alias yes-or-no-p function to y-or-n-p function
(defalias 'yes-or-no-p 'y-or-n-p)

;; leave off unless and locally set to t dependent on language.
;; (setq-default indent-tabs-mode nil)

;; make 80 the horizontal char limit
(setq-default fill-column 80)

;; default window dimensions 130x150 and 200 pixels down. X unspecified. This
;; works for emacsclient as well
(when (display-graphic-p)
  (setq default-frame-alist
	'((width . 130)
	  (height . 150)
	  (top . 200))))

;; make firefox-developer-edition default browser
(setq browse-url-generic-program "firefox-developer-edition"
      browse-url-browser-function #'browse-url-generic)

(setq enable-recursive-minibuffers t)

(add-to-list 'auto-mode-alist '("\\.service\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.rules\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.bashrc\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.zshrc\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . conf-mode))

;; not autosave and no lock files. But do backup to specific dir
(setq make-backup-files t
      auto-save-default nil
      create-lockfiles nil)

(when (not (f-exists? "~/.emacs.d/backups"))
      (mkdir "~/.emacs.d/backups"))

(setq backup-by-copying t
      backup-directory-alist
      '(("." . "~/.emacs.d/backups"))
      delete-old-versions t
      kept-new-versions 3
      kept-old-versions 2
      version-control t)

(winner-mode)

;; vim style bindings
(define-key winner-mode-map (kbd "C-w C-h") 'winner-undo)
(define-key winner-mode-map (kbd "C-w C-l") 'winner-redo)

;; avy for faster navigation inside and outside buffers
(use-package avy
  :ensure t)

;; a non-directional way to switch windows
(use-package ace-window
  :ensure t
  :init
  (setq aw-keys '(?h ?j ?k ?l ?a ?s ?d ?f)))

;resizes windows to the golden ratio
(use-package golden-ratio
  :ensure t)

;; (use-package markdown-mode
;;   :ensure t
;;   :mode (("README\\.md\\'" . gfm-mode)
;;          ("\\.md\\'" . markdown-mode)
;;          ("\\.markdown\\'" . markdown-mode))
;;   :init
;;   (setq markdown-command "pandoc -s --quiet"))

(use-package diminish
  :ensure t)

;; reminds you of common commands formable from key prefixes when you type
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

(use-package rainbow-mode
  :ensure t)

;; (use-package gruvbox-theme :ensure t)
;; (use-package modus-operandi-theme :ensure t :config (load-theme 'modus-operandi t))
;; (use-package modus-vivendi-theme :ensure t)

;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/gruvbox")


(add-to-list 'load-path "~/.emacs.d/small-mods/")
(require 'better-theme-switching)
(d/make-theme light-theme 'leuven
	      ;; blue and white ivy selection
	      (custom-set-faces '(ivy-current-match ((((class color) (background light))
						      (:background "#1a4b77" :foreground "white" :extend t))
						     (((class color) (background dark))
						      (:background "#65a7e2" :foreground "black" :extend t)))))
	      (mapc #'disable-theme custom-enabled-themes) ;disable all themes currently enabled
	      (setq theme-state 'light)			   ;for use by `d/flip-theme'
	      (message "==loaded light theme")
	      )

(d/make-theme dark-theme 'spacemacs-dark
	      (custom-set-faces
	       ;; bright green ivy selection
	       '(ivy-current-match ((t (:foreground "chartreuse3" :underline t :weight bold)))))

	      (mapc #'disable-theme custom-enabled-themes)
	      (setq theme-state 'dark)
	      (message "==loaded dark theme")
	      )
(d/load-theme dark-theme)

;; C, C++, Objective-C completion
;; this takes care of loading the irony server as well. It integrates with
;; company
(use-package company-irony
  :ensure t
  :after (company)
  :config
  (require 'irony)
  (add-hook 'c++-mode-hook #'irony-mode)
  ;; (add-hook 'c-mode-hook #'irony-mode)
  (add-hook 'objc-mode-hook #'irony-mode)
  (add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options)
  (add-to-list 'company-backends #'company-irony))

;; eldoc support in c modes
(use-package irony-eldoc
  :ensure t
  :after (company-irony)
  :config (add-hook 'irony-mode-hook #'irony-eldoc))

;; (add-hook 'c-mode-hook
	  ;; #'irony-mode)


(defvar fs-c-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map fs-map)
    map)
  "Overrides `fs-map' in c-mode buffers")
(define-prefix-command 'fs-c-mode-map)

;; look up man page at point
(define-key fs-c-mode-map (kbd "C-h") #'man-follow) ; C-/ C-h for man follow


(add-hook 'c-mode-hook
	  #'(lambda ()
	      (local-set-key (kbd "C-x u") nil) ;don't overwrite this
	      (local-set-key (kbd "C-/") fs-c-mode-map)
	      ))

;; (setq c-default-style "k&r")
(require 'cc-vars)
(push '(c-mode . "k&r") c-default-style)

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(use-package company-web
  :ensure t
  :after (company web-mode)
  :config
  (add-to-list 'company-backends #'company-web-html)
  ;; (add-hook 'html-mode #'company-web-html)
  (define-key web-mode-map (kbd "C-c C-.") 'company-web-html)
  (add-to-list 'company-backends #'company-css)
  )


;; (add-hook 'html-mode-hook
;;           #'(lambda ()
;;               (define-key html-mode-map
;;                 (kbd "C-c C-.")
;;                 #'company-web-html)))

;; (use-package slime-company
;;   :ensure t)
(add-hook 'slime-mode-hook
          #'(lambda ()
              (setq-local fill-column 100)))

;; in buffer completion framework
(use-package company
  :ensure t
  :config
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  ;; C-; C-/ - force company mode file path completion
  (define-key semicolon-map
    (kbd "C-/")
    #'company-files)

  (global-company-mode 1)
  (diminish 'company-mode)
  :after (diminish))

(setq company-minimum-prefix-length 3)
(setq company-tooltip-limit 15)
;; if idle delay is non-nil, tramp will hang a lot.
(setq company-default-idle-delay 0.05)
(setq company-idle-delay company-default-idle-delay)

(defun toggle-company-idle-delay ()
  (interactive)
  (message "Company Idle Delay %s"
           (propertize (format "%s"
                               (if company-idle-delay
                                   (setq company-idle-delay nil)
                                 (setq company-idle-delay company-default-idle-delay)))
                       'face '(:foreground "#00FFFF"))))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-height 32)                  ;32 candidates
  (global-set-key (kbd "C-x b") #'ivy-switch-buffer)
  (global-set-key (kbd "C-c C-r") #'ivy-resume)

  (setq ivy-use-virtual-buffers t)

  (diminish 'ivy-mode)
  :after (diminish))

(use-package swiper
  :ensure t
  :config
  (setq ivy-use-group-face-if-no-groups nil) ;weird error if you don't do this
  :after (ivy))

(use-package counsel
  :ensure t
  :config
  (setq counsel-find-file-at-point t)
  (global-set-key (kbd "C-x C-f") #'counsel-find-file)
  (global-set-key (kbd "M-x") #'counsel-M-x)
  (global-set-key (kbd "C-h v") #'counsel-describe-variable)
  (global-set-key (kbd "C-h f") #'counsel-describe-function)
  (global-set-key (kbd "C-h l") #'counsel-find-library)
  (global-set-key (kbd "C-h S") #'counsel-info-lookup-symbol)
  (global-set-key (kbd "C-s") #'counsel-grep-or-swiper)
  (global-set-key (kbd "C-x r b") #'counsel-bookmark)
  (global-set-key (kbd "C-x r i") #'counsel-register)

  (define-key semicolon-map
    (kbd "C-s") #'counsel-ag)
  (define-key semicolon-map
    (kbd "C-f") #'counsel-fzf)
  :after (ivy swiper))

(setq dired-listing-switches "-al --human-readable")

;; additional dired functionality. Comes with emacs
(require 'dired-x)

;; collapses dirs having only 1 item, but still displays the collapsed dir so
;; that you can see the full relative path
;; TODO: this breaks 'j' -> `dired-goto-file' functionality for collapsed dirs
;; (use-package dired-collapse
  ;; :ensure t
  ;; :config (add-hook 'dired-mode-hook #'dired-collapse-mode))

(use-package dired-quick-sort
  :ensure t
  :config (dired-quick-sort-setup))     ;binds S to hyrda sort dispatcher

(setq dired-dwim-target t)

;; (setq dired-omit-mode t)                ;this hides .elc among others

;; simple function to toggle display of dotfiles in dired
(defun dired-dotfiles-toggle ()
  "Show/hide dot-files"
  (interactive)
  (when (equal major-mode 'dired-mode)
    (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p) ; if currently showing
	(progn 
	  (set (make-local-variable 'dired-dotfiles-show-p) nil)
	  (message "h")
	  (dired-mark-files-regexp "^\\\.")
	  (dired-do-kill-lines))
      (progn (revert-buffer) ; otherwise just revert to re-show
	     (set (make-local-variable 'dired-dotfiles-show-p) t)))))

(defvar fs-dired-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map fs-map)
    map)
  "Overrides `fs-map' in dired-mode buffers")
(define-prefix-command 'fs-dired-mode-map)

(define-key fs-dired-mode-map (kbd "C-d") #'dired-dotfiles-toggle)

(add-hook 'dired-mode-hook
	  #'(lambda ()
	      (local-set-key (kbd "C-/") fs-dired-mode-map)))

(setq doc-view-resolution 300)

;; for viwing pdfs and other things. NOTE, pdf-tools-install only installs when
;; not already installed, so this is fine.
;; TODO: for some reason, pdf-tool-install was breaking (doc-view-toggle-display)
;; (use-package pdf-tools
  ;; :ensure t
  ;; :config
  ;; (pdf-tools-install)
  ;; )

(setq global-eldoc-mode t)
(setq eldoc-idle-delay 0.05)        ;reduce time it takes for eldoc to pop up
(setq eldoc-print-after-edit nil)   ;documentation is show even when not editing
(setq irony-eldoc-use-unicode nil)  ;OFF: use ∷ and ⇒ instead of :: and =>

(custom-set-faces
 '(eldoc-highlight-function-argument ((t (:inherit bold
						   :foreground "#98971a"
						   :height 1.3)))))

(add-hook 'eshell-mode-hook
	  #'(lambda ()
	      ;; don't auto complete with company
	      (setq-local company-idle-delay nil)
	      ;; use default completion instead of pcomplete
	      ;; (define-key eshell-mode-map (kbd "<tab>") #'completion-at-point)
	      ))
(setq eshell-prefer-lisp-functions nil)

; tab completion in eshell
(setq eshell-cmpl-cycle-completions nil)

;; more quickly traverse to parent directories with regexp match over ../../../../
(use-package eshell-up
  :ensure t)

(add-to-list 'load-path "~/.emacs.d/eshell-ring/")
(require 'eshell-ring)
(global-eshring-mode 1)

(require 'eshell-mods "~/.emacs.d/eshell-mods")

(setq eshell-aliases-source "~/.zshrc")

(defun write-eshell-aliases () 
  (interactive)
  (f-write (eshell-parse-aliases eshell-aliases-source
				 '(("ls" . ("--classify"
					    "--color=[[:word:]]+"))
				   ("top" . nil))
				 ;; "alias top (helm-top)"
				 "alias up eshell-up $1"
				 "alias pk eshell-up-peek $1")
	   'utf-8 eshell-aliases-file))

;;;; sudo completion
(defun pcomplete/sudo ()
  "Completion rules for the `sudo' command."
  (let ((pcomplete-ignore-case t))
    (pcomplete-here (funcall pcomplete-command-completion-function))
    (while (pcomplete-here (pcomplete-entries)))))

;;;; systemctl completion
(defcustom pcomplete-systemctl-commands
  '("disable" "enable" "status" "start" "restart" "stop" "reenable"
    "list-units" "list-unit-files")
  "p-completion candidates for `systemctl' main commands"
  :type '(repeat (string :tag "systemctl command"))
  :group 'pcomplete)

(defvar pcomplete-systemd-units
  (split-string
   (shell-command-to-string
    "(systemctl list-units --all --full --no-legend;systemctl list-unit-files --full --no-legend)|while read -r a b; do echo \" $a\";done;"))
  "p-completion candidates for all `systemd' units")

(defvar pcomplete-systemd-user-units
  (split-string
   (shell-command-to-string
    "(systemctl list-units --user --all --full --no-legend;systemctl list-unit-files --user --full --no-legend)|while read -r a b;do echo \" $a\";done;"))
  "p-completion candidates for all `systemd' user units")

(defun pcomplete/systemctl ()
  "Completion rules for the `systemctl' command."
  (pcomplete-here (append pcomplete-systemctl-commands '("--user")))
  (cond ((pcomplete-test "--user")
	 (pcomplete-here pcomplete-systemctl-commands)
	 (pcomplete-here pcomplete-systemd-user-units))
	(t (pcomplete-here pcomplete-systemd-units))))

;;;; man completion
(defvar pcomplete-man-user-commands
  (split-string
   (shell-command-to-string
    "apropos -s 1,3 .|while read -r a b; do echo \" $a\";done;"))
  "p-completion candidates for `man' command")

(defun pcomplete/man ()
  "Completion rules for the `man' command."
  (pcomplete-here pcomplete-man-user-commands))

(shell-command-to-string "man man")

(add-hook 'shell-mode-hook
          #'(lambda ()
              ;; don't use company for auto completion
              (setq-local company-idle-delay nil)))

(use-package undo-tree
  :load-path "~/.emacs.d/undo-tree/"
  :ensure t
  :init
  (when (not (f-exists? "~/.emacs.d/undo-tree-hist"))
    (mkdir "~/.emacs.d/undo-tree-hist"))
  :config
  (global-undo-tree-mode 1)

  (defalias #'redo #'undo-tree-redo)
  (defalias #'undo #'undo-tree-undo)

  ;; sets directory where persistent undo history is stored
  (setq undo-tree-history-directory-alist
        '(("." . "~/.emacs.d/undo-tree-hist")))
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-visualizer-timestamps t)

  ;; evil provides bindings for these, so they're unnecessary
  (define-key undo-tree-map (kbd "C-/") nil)
  (define-key undo-tree-map (kbd "C-_") nil)
  (define-key undo-tree-map (kbd "M-_") nil)
  (diminish 'undo-tree-mode)
  :after (diminish))

(add-to-list 'load-path "~/.emacs.d/evil-numbers/")
(require 'evil-numbers)

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t  ;set C-u to function as scroll up in evil mode
	evil-want-C-i-jump nil  ;this should fix issues with evil tabbing in org
	evil-want-fine-undo t   ;finer granularity for undo
	evil-want-Y-yank-to-eol t   ;Y yanks to eol instead of stupid whole line
	;; evil-want-minibuffer t
	)
  ;; Normally bound to `upcase-word', but evil provides gUaw and
  ;; `universal-argument' is more important
  (global-set-key (kbd "M-u") #'universal-argument)
  :config
  (evil-mode 1)
  (setq evil-echo-state nil) ;turn off -- INSERT --, -- VISUAL --, because it ruins eldoc.

  ;; INSERT STATE
  ;; escape from insert state with M-i
  (define-key evil-insert-state-map
    (kbd "M-i") #'evil-normal-state)
  ;; VISUAL STATE
  (define-key evil-visual-state-map
    (kbd "M-i") #'evil-normal-state)
  ;; NORMAL STATE
  ;; set vimish-fold-avy as default action for zf
  (define-key evil-normal-state-map
    (kbd "zf") #'vimish-fold-avy)
  (define-key evil-normal-state-map
    (kbd "M-a") #'evil-avy-goto-line)
  (define-key evil-normal-state-map
    (kbd "M-i") #'evil-normal-state-map)
  ;; don't overwrite `xref-find-definitions'
  (define-key evil-normal-state-map
    (kbd "M-.") nil)
  ;; evil-numbers increment and decrement functionality
  (define-key evil-normal-state-map
    (kbd "C-c C-=") #'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map
    (kbd "C-c +") #'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map
    (kbd "C-c C--") #'evil-numbers/dec-at-pt)
  ;; MOTION STATE
  ;; have already redefined C-u to `evil-scroll-page-up'
  (define-key evil-motion-state-map
    (kbd "C-b") nil)

  ;; tell evil to not run in these modes
  (nconc evil-emacs-state-modes
	 '(dired-mode)
	 '(image-mode)
	 '(ivy-occur-mode)
	 '(epa-key-list-mode epa-key-mode epa-info-mode) ;easy pgp
	 )

  ;; force nomral evil state in these modes
  (require 'ivy)
  (setq evil-normal-state-modes
	'(grep-mode			;so we can use evil to edit with `wgrep'
	  ivy-occur-grep-mode		;so the above works in counsel-ag too
	  ))

  :after (ivy))

;; vimish fold
(use-package vimish-fold
  :ensure t)

;; evil keybindings to vimish fold zf, za, zd, &c
(use-package evil-vimish-fold
  :ensure t
  :after (vimish-fold)
  :config
  (evil-vimish-fold-mode)
  (diminish 'evil-vimish-fold-mode)
  :after (diminish evil))

;; auto start flyspell on non programming buffers
(add-hook 'org-mode-hook #'flyspell-mode)
(add-hook 'text-mode-hook #'flyspell-mode)

(require 'flyspell)
(define-key flyspell-mode-map (kbd "C-;") nil)
(define-key flyspell-mode-map (kbd "C-M-i") nil)
(define-key flyspell-mode-map (kbd "C-,") nil)
(define-key flyspell-mode-map (kbd "C-c $") nil)

;; ;; so we need a hook
;; (add-hook 'flyspell-mode-hook
;;           #'(lambda ()
;;               (define-key flyspell-mode-map (kbd "C-;") nil)
;;               (define-key flyspell-mode-map (kbd "C-M-i") nil)
;;               (define-key flyspell-mode-map (kbd "C-,") nil)
;;               (define-key flyspell-mode-map (kbd "C-c $") nil)))

(use-package magit
  :ensure t)

(add-hook 'org-mode-hook
	  (lambda ()
	    ;; redefines org's definition of paragraph start and end to be compatible with
	    ;; evil mode's notion of "a paragraph"
	    ;; (setq paragraph-start "\\|[ 	]*$"
	    ;;       paragraph-separate "[ 	]*$")
	    
	    ;; (setq fill-column 100)
	    (auto-fill-mode 1) ;automatically break line at `current-fill-column'
	    (defun org-insert-today ()
	                  "Inserts todays date in the following form <1969-12-31 Wed>"
	                  (interactive)
	                  (insert (format-time-string "<%Y-%m-%d %a>" (current-time))))
	    
	    (define-skeleton org-mode-html-header
	      "Inserts skeleton fitting most org-mode files which export to HTML"
	      ""
	      (format "#+TITLE: %12s\n" (read-string "Title: "))
	      "#+AUTHOR:\n"
	      "#+EMAIL: nil\n"
	      (format "#+DATE: %s\n" (format-time-string "<%Y-%m-%d %a>" (current-time)))
	      "#+LANGUAGE: en"\n
	      (if (y-or-n-p "Custom stylesheet?")
	          (format "#+HTML_HEAD: <link rel='stylesheet' type='text/css' href='%s'\n%s\n"
	                  (file-relative-name (read-file-name "path: " ) default-directory)
	                  "#+OPTIONS: html-style:nil")
	        "#+OPTIONS: html-style:t\n")
	      "#+OPTIONS: toc:t"\n
	      "#+OPTIONS: tex:t"\n
	      "#+OPTIONS: html-postamble:nil"\n
	      (let ((todo-kwords ""))
	        (loop for kword in (cdar org-todo-keywords)
	              do (setq todo-kwords (s-concat todo-kwords " " kword)))
	        (format "#+TODO: %s\n" todo-kwords))
	      "#+PROPERTY: header-args :results output")))

(define-key org-mode-map
  (kbd "C-c C-'")
  'org-babel-expand-src-block)

(setq org-babel-python-command "python3")

(org-babel-do-load-languages 'org-babel-load-languages
 '((python . t)
   (C . t)
   ;; (R . t)
   ;; (dot . t)
   (lisp . t)
   (shell . t)
   ;; (awk . t)
   ;; (sed . t)
   (emacs-lisp . t)
   ;; (sql . t)
   ;; (js . t)
   ))

;; sets where org-edit-special takes you (C-c ')
(setq org-src-window-setup 'current-window)

;; function (org-show-block-all)
;; choose to hide or show blocks on startup
(setq org-hide-block-startup nil)

(setq org-todo-keywords
      '((sequence "TODO" "DOING" "DONE")))

(setq org-todo-keyword-faces
      '(("DOING" . "yellow")))

(setq org-startup-with-inline-images t)

;; don't confirm evaluation of src code block
(setq org-confirm-babel-evaluate nil)

;; used by org to produce html files
(use-package htmlize
  :ensure t)

;; emacs speaks statistics. Not exclusively for org, but this is where I'll use
;; it.
;; (use-package ess
  ;; :ensure t)

;; for drawing graphs. Will only really be used in org-mode
;; (use-package graphviz-dot-mode
  ;; :ensure t)

(use-package restclient
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode)))

;; man completion
(defadvice man (before my-woman-prompt activate)
  (interactive (progn
		 (require 'woman)
		 (list (woman-file-name nil)))))

(use-package slime
  :ensure t)

(defun set-inferior-lisp (name)
  (with-temp-buffer
    (let ((res (shell-command (format "which %s" name) (current-buffer)))
          (output (string-remove-suffix "\n" (buffer-string))))
      (if (= res 0)
          (message "SLIME: inferior lisp, %s, located at %s" name
                   (setq inferior-lisp-program output))
        (message "SLIME: tried to located inferior lisp, %s, but got the following error [%s]"
                 name output)))))

(set-inferior-lisp "sbcl")

;; load "almost all of the popular contribs"
;; (setq slime-contribs '(slime-fancy slime-company))

(slime-setup '(slime-fancy))

(add-hook 'lisp-mode-hook #'slime-mode)

(setq tramp-default-method "ssh")

;; writable `grep'. do a grep and edit it to apply those changes.
;; use with `rgrep' to modify multiple files recursively
(use-package wgrep
  :ensure t)

;; wrapper around `ag'
(use-package wgrep-ag
  :ensure t
  :after (wgrep))

(setq electric-pair-pairs '((?\( . ?\))
			    (?\[ . ?\])
			    (?\{ . ?\})
			    (?\" . ?\")))
(electric-pair-mode t)

(use-package yasnippet
  :ensure t
  :config
  (require 'yasnippet)
  (yas-global-mode 1)
  (diminish 'yas-minor-mode)
  :after (diminish))
