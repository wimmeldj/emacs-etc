(require 'use-package)

(use-package diminish
  :ensure t)


;;;; ===========================================================================
;;;;                                   trying out 

(use-package dired-rsync
  :ensure t)
(use-package rainbow-blocks
  :ensure t
  :config (rainbow-blocks-mode 1))
(use-package blimp :ensure t
  :config
  (add-hook 'image-minor-mode-hook 'blimp-mode))
(use-package gnuplot :ensure t)
(use-package restclient
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode)))



;;;; ===========================================================================
;;;;                                  file assocs 

(add-to-list 'auto-mode-alist '("\\.service\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.rules\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.bashrc\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.zshrc\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.ps1\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.csproj\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\PKGBUILD\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.conf\\'" . conf-mode))



;;;; ===========================================================================
;;;;                             general configuration 

;; (require 'server)
;; (unless (server-running-p)
;;   (server-start))

;; only warn about file local variables when considered unsafe
(setq enable-local-variables t)

;; x clipboard support
(setq select-enable-clipboard t)
(setq x-select-enable-clipboard-manager t)

(setq ring-bell-function 'ignore)

;; alias yes-or-no-p function to y-or-n-p function
(defalias 'yes-or-no-p 'y-or-n-p)

;; leave off unless and locally set to t dependent on language.
(setq-default indent-tabs-mode nil)

;; make 80 the horizontal char limit
(setq-default fill-column 80)

(setq browse-url-generic-program "firefox"
      browse-url-browser-function #'browse-url-generic)

(setq enable-recursive-minibuffers t)

;; hack in emacs 27.1 to make buffers with really long lines not cause ruin
(global-so-long-mode)

;; not autosave and no lock files. But do backup to specific dir
(setq make-backup-files t
      auto-save-default t
      create-lockfiles nil)

;; don't autosave and no lock files, but /do/ backup to /backups
(let ((backup-dir (concat user-emacs-directory "backups")))
  (when (not (file-exists-p backup-dir))
    (mkdir backup-dir))
  (setq backup-by-copying t
        backup-directory-alist
        `(("." . ,backup-dir))
        delete-old-versions t
        kept-new-versions 3
        kept-old-versions 2
        version-control t))

;; pdfs not fuzzy
(setq doc-view-resolution 300)

(require 'bookmark)
(setq bookmark-save-flag t
      bookmark-use-annotations t)

;; provides vim-like `forward-to-word' `backward-to-word'
(require 'misc)

(setq delete-by-moving-to-trash nil)


;;;; ===========================================================================
;;;;                             navigation and windows

(require 'winner)
(winner-mode)

;; avy for faster navigation inside and outside buffers
(let ((keys '(?a ?s ?d ?f ?g
                 ?h ?j ?k ?l ?\;
                 ?q ?w ?e ?r ?t
                 ?u ?i ?o ?p
                 ?m ?n)))
  (use-package avy
    :ensure t
    :config
    (setq avy-keys keys))

  ;; a non-directional way to switch windows
  (use-package ace-window
    :ensure t
    :config
    (setq aw-keys keys)))



;;;; ===========================================================================
;;;;                                    company 

;; in buffer completion framework
(use-package company
  :ensure t
  :config
  (require 'company)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)

  (setq company-dabbrev-downcase nil) ;otherwise completion is downcase for plaintext
  (setq company-minimum-prefix-length 3)
  (setq company-tooltip-limit 15)
  ;; if idle delay is non-nil, tramp will hang a lot.
  (setq company-default-idle-delay 0.0)
  (setq company-idle-delay company-default-idle-delay)

  (add-hook 'tramp--startup-hook
            (lambda ()
              ;; disable auto completion in tramp
              (setq-local company-idle-delay nil)))

  (global-company-mode 1)
  (diminish 'company-mode)
  :after (diminish))



;; ;;;; ===========================================================================
;; ;;;;                            ivy completion framework

(use-package ivy
  :ensure t
  :config
  ;; (ivy-mode 1)
  (setq ivy-height 32)                  ;32 candidates
  (setq ivy-use-virtual-buffers t)
  (diminish 'ivy-mode)
  :after (diminish))

;; (use-package swiper
;;   :ensure t
;;   :config
;;   (setq ivy-use-group-face-if-no-groups nil) ;weird error if you don't do this
;;   :after (ivy))

(use-package counsel
  :ensure t
  :config
  (setq counsel-find-file-at-point t)
  )

;; ;; (require 'ido)
;; ;; (require 'ido-hacks)
;; ;; (ido-mode 1)
;; ;; (ido-hacks-mode)

;; ;; (setq ido-enable-flex-matching t
;; ;;       ido-everywhere t
;; ;;       ido-use-filename-at-point 'guess
;; ;;       ido-create-new-buffer 'always
;; ;;       ido-virtual-buffers t
;; ;;       ido-max-window-height 0.50
;; ;;       ido-show-dot-for-dired t          ;first item is always dired
;; ;;       ido-use-url-at-point t
;; ;;       ido-enable-regexp t
;; ;;       ido-max-prospects 100
;; ;;       )

;; ;; (setf (elt ido-decorations 2) "\n"
;; ;;       (elt ido-decorations 3) "\n")

;; ;; ;; C-n/C-p for next/prev completion
;; ;; (define-key ido-common-completion-map (kbd "C-n") #'ido-next-match)
;; ;; (define-key ido-common-completion-map (kbd "C-p") #'ido-prev-match)

;; ;; (require 'smex)



;;;; ===========================================================================
;;;;                                      fzf 
;; set before requiring so as to trigger warning message when loaded and these
;; settings don't make sense

;; disabling. fzf needs to be updated for emacs28, but I don't really use it much.
;; (setq fzf/executable "fzf")
;; (setq fzf/wsl nil)                      ;set wsl flag
;; (setq fzf/args "-x --prompt='? ' --print-query --query='!^bin !^obj '")

;; ;; forked implementation of fzf
;; (require 'fzf)

;; no evil mode in fzf
;; (advice-add 'fzf :after 'turn-off-evil-mode)
;; (advice-add 'fzf-git :after 'turn-off-evil-mode)
;; (advice-add 'fzf-git-grep :after 'turn-off-evil-mode)
;; (advice-add 'fzf-git-files :after 'turn-off-evil-mode)
;; (advice-add 'fzf-hg :after 'turn-off-evil-mode)
;; (advice-add 'fzf-directory :after 'turn-off-evil-mode)
;; (advice-add 'fzf-projectile :after 'turn-off-evil-mode)



;;;; ===========================================================================
;;;;                                     dired 

(setq dired-listing-switches "-Al -v --human-readable")
(setq dired-dwim-target t)
;; (setq dired-omit-mode t)                ;this hides .elc among others

;; additional dired functionality. Comes with emacs
(require 'dired-x)

;; collapses dirs having only 1 item, but still displays the collapsed dir so
;; that you can see the full relative path
;; TODO: this breaks 'j' -> `dired-goto-file' functionality for collapsed dirs
;; (use-package dired-collapse
  ;; :ensure t
  ;; :config (add-hook 'dired-mode-hook #'dired-collapse-mode))

(require 'd-dired)



;;;; ===========================================================================
;;;;                                  ehsell/shell 

(add-hook 'shell-mode-hook
          #'(lambda ()
              ;; don't use company for auto completion
              (setq-local company-idle-delay nil)))

(add-hook 'eshell-mode-hook
          #'(lambda ()
              ;; don't auto complete with company
              (setq-local company-idle-delay nil)
              ;; use default completion instead of pcomplete
              ;; (define-key eshell-mode-map (kbd "<tab>") #'completion-at-point)
              ))
(setq eshell-prefer-lisp-functions nil)

;; cycle through completions with TAB
(setq eshell-cmpl-cycle-completions t)

;; (require 'eshell-ring)
;; (global-eshring-mode 1)

;; *SLOW* This adds about 3 seconds to startup because of all the shell commands. Maybe we should
;; cache? From https://www.emacswiki.org/emacs/EshellCompletion. Fixes eshell's noncompletion of
;; sub-commands

;; (defun pcomplete/sudo ()
;;   "Completion rules for the `sudo' command."
;;   (let ((pcomplete-ignore-case t))
;;     (pcomplete-here (funcall pcomplete-command-completion-function))
;;     (while (pcomplete-here (pcomplete-entries)))))

;;   ;;;; systemctl completion
;; (defcustom pcomplete-systemctl-commands
;;   '("disable" "enable" "status" "start" "restart" "stop" "reenable"
;;     "list-units" "list-unit-files")
;;   "p-completion candidates for `systemctl' main commands"
;;   :type '(repeat (string :tag "systemctl command"))
;;   :group 'pcomplete)

;; (defvar pcomplete-systemd-units
;;   (split-string
;;    (shell-command-to-string
;;     "(systemctl list-units --all --full --no-legend;systemctl list-unit-files --full --no-legend)|while read -r a b; do echo \" $a\";done;"))
;;   "p-completion candidates for all `systemd' units")

;; (defvar pcomplete-systemd-user-units
;;   (split-string
;;    (shell-command-to-string
;;     "(systemctl list-units --user --all --full --no-legend;systemctl list-unit-files --user --full --no-legend)|while read -r a b;do echo \" $a\";done;"))
;;   "p-completion candidates for all `systemd' user units")

;; (defun pcomplete/systemctl ()
;;   "Completion rules for the `systemctl' command."
;;   (pcomplete-here (append pcomplete-systemctl-commands '("--user")))
;;   (cond ((pcomplete-test "--user")
;;          (pcomplete-here pcomplete-systemctl-commands)
;;          (pcomplete-here pcomplete-systemd-user-units))
;;         (t (pcomplete-here pcomplete-systemd-units))))

;;   ;;;; man completion
;; (defvar pcomplete-man-user-commands
;;   (split-string
;;    (shell-command-to-string
;;     "apropos -s 1,3 .|while read -r a b; do echo \" $a\";done;"))
;;   "p-completion candidates for `man' command")

;; (defun pcomplete/man ()
;;   "Completion rules for the `man' command."
;;   (pcomplete-here pcomplete-man-user-commands))

;; (shell-command-to-string "man man")



;;;; ===========================================================================
;;;;                                   undo-tree 

;; qdwimmel TODO restore undo tree settings
;; (use-package undo-tree
;;   :load-path "~/.emacs.d/undo-tree/"
;;   :ensure t
;;   :init
;;   (when (not (file-exists-p "~/.emacs.d/undo-tree-hist"))
;;     (mkdir "~/.emacs.d/undo-tree-hist"))
;;   :config
;;   (global-undo-tree-mode 1)

;;   (defalias #'redo #'undo-tree-redo)
;;   (defalias #'undo #'undo-tree-undo)

;;   ;; sets directory where persistent undo history is stored
;;   (setq undo-tree-history-directory-alist
;;         '(("." . "~/.emacs.d/undo-tree-hist")))
;;   (setq undo-tree-auto-save-history t)
;;   (setq undo-tree-visualizer-diff t)
;;   (setq undo-tree-visualizer-timestamps t)

;;   ;; evil provides bindings for these, so they're unnecessary
;;   ;; (define-key undo-tree-map (kbd "C-/") nil)
;;   ;; (define-key undo-tree-map (kbd "C-_") nil)
;;   ;; (define-key undo-tree-map (kbd "M-_") nil)
;;   (diminish 'undo-tree-mode)
;;   :after (diminish))



;;;; ===========================================================================
;;;;                                      evil 

(require 'evil-numbers)

;; (use-package evil
;;   :ensure t
;;   :init
;;   (setq evil-want-C-u-scroll t  ;set C-u to function as scroll up in evil mode
;;         evil-want-C-i-jump nil  ;this should fix issues with evil tabbing in org
;;         evil-want-fine-undo t   ;finer granularity for undo
;;         evil-want-Y-yank-to-eol t   ;Y yanks to eol instead of stupid whole line
;;         ;; evil-want-minibuffer t
;;         )
;;   :config
;;   ;; (evil-mode 1)
;;   (setq evil-echo-state nil) ;turn off -- INSERT --, -- VISUAL --, because it ruins eldoc.

;;   ;; INSERT STATE
;;   ;; escape from insert state with M-i
;;   (define-key evil-insert-state-map
;;     (kbd "M-i") #'evil-normal-state)
;;   ;; VISUAL STATE
;;   (define-key evil-visual-state-map
;;     (kbd "M-i") #'evil-normal-state)
;;   ;; NORMAL STATE
;;   ;; set vimish-fold-avy as default action for zf
;;   (define-key evil-normal-state-map
;;     (kbd "zf") #'vimish-fold-avy)
;;   (define-key evil-normal-state-map
;;     (kbd "M-i") #'evil-normal-state-map)
;;   ;; don't overwrite `xref-find-definitions'
;;   (define-key evil-normal-state-map
;;     (kbd "M-.") nil)
;;   ;; evil-numbers increment and decrement functionality
;;   (define-key evil-normal-state-map
;;     (kbd "C-c C-=") #'evil-numbers/inc-at-pt)
;;   (define-key evil-normal-state-map
;;     (kbd "C-c +") #'evil-numbers/inc-at-pt)
;;   (define-key evil-normal-state-map
;;     (kbd "C-c C--") #'evil-numbers/dec-at-pt)
;;   ;; MOTION STATE
;;   ;; have already redefined C-u to `evil-scroll-page-up'
;;   (define-key evil-motion-state-map
;;     (kbd "C-b") nil)

;;   ;; tell evil to not run in these modes
;;   (nconc evil-emacs-state-modes
;;          '(dired-mode)
;;          '(image-mode)
;;          '(ivy-occur-mode)
;;          '(epa-key-list-mode epa-key-mode epa-info-mode) ;easy pgp
;;          )

;;   ;; force nomral evil state in these modes
;;   (require 'ivy)
;;   (setq evil-normal-state-modes
;;         '(
;;           grep-mode                   ;so we can use evil to edit with `wgrep'
;;           ivy-occur-grep-mode         ;so the above works in counsel-ag too
;;           ))

;;   :after (ivy))

;; vimish fold
(use-package vimish-fold
  :ensure t)

;; evil keybindings to vimish fold zf, za, zd, &c
;; (use-package evil-vimish-fold
;;   :ensure t
;;   :after (vimish-fold)
;;   :config
;;   (evil-vimish-fold-mode)
;;   (diminish 'evil-vimish-fold-mode)
;;   :after (diminish evil))



;;;; ===========================================================================
;;;;                                    flyspell 

;; [s -> goto previous flyspell error
;; ]s -> goto next flyspell error
;; z= -> ispell-word
;; auto start flyspell on non programming buffers
(add-hook 'org-mode-hook #'flyspell-mode)
(add-hook 'text-mode-hook #'flyspell-mode)

(require 'flyspell)
(define-key flyspell-mode-map (kbd "C-;") nil)
(define-key flyspell-mode-map (kbd "C-M-i") nil)
(define-key flyspell-mode-map (kbd "C-,") nil)
(define-key flyspell-mode-map (kbd "C-c $") nil)



;;;; ===========================================================================
;;;;                                      man 

(defadvice man (before my-woman-prompt activate)
  (interactive (progn
                 (require 'woman)
                 (list (woman-file-name nil)))))

(setq Man-notify-method 'pushy)         ;man opens selected page in current buffer


;;;; ===========================================================================
;;;;                                     tramp 

(setq tramp-default-method "ssh")



;;;; ===========================================================================
;;;;                                      vlf 


(use-package vlf :ensure t
  :config
  (require 'vlf-setup) ;when opening large file, gives a 'v' option for opening with vlf
  )



;;;; ===========================================================================
;;;;                                      grep 

;; writable `grep'. do a grep and edit it to apply those changes.
;; use with `rgrep' to modify multiple files recursively
(use-package wgrep
  :ensure t)

;; wrapper around `ag'
(use-package wgrep-ag
  :ensure t
  :after (wgrep))


;;;; ===========================================================================
;;;;                                  epa easy pgp
(setq epg-key-id "David Wimmel")



;;;; ===========================================================================
;;;;                             libraries and packages 


(require 'visual)
(require 'langs)
(require 'd:org)
(require 'interactive)

(provide 'main)


;;;; ===========================================================================
;;;;                                    ibuffer
;; c-h v 'ibuffer-formats



