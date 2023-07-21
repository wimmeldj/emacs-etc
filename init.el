(setq source-directory "/home/d/.emacs.d/emacs-27.1/")


;;;; ===========================================================================
;;;;                               package management

(package-initialize)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
;;			 ("marmalade" . "http://marmalade-repo.org/packages/")
                         ))
(let* ((packages '(
                   modus-themes
                   spacemacs-theme
                   gruvbox-theme
                   diminish

                   ace-window
                   avy
                   avy
                   beacon
                   company
                   counsel
                   flyspell
                   ivy
                   magit
                   nix-mode
                   rainbow-delimiters
                   solidity-mode
                   undo-tree
                   wgrep
                   wgrep-ag
                   which-key
                   winner
                   yasnippet
                   markdown-mode
                   edit-indirect        ;required by markdown-mode to edit codeblocks

                   rust-mode
                   forge                ;magit interface for "forges", e.g. github
                   github-review
                    ))
       (notinstalled (seq-filter #'(lambda (pkg) (not (package-installed-p pkg)))
                                 packages)))
    (when notinstalled
    (message "==The following packages will be installed: %s" (format "%S" notinstalled))
    (package-refresh-contents)
    (mapc #'package-install notinstalled)))


;;;; ===========================================================================
;;;;                        include local files in load-path

(let ((paths '(                         ;with respect to ~/.emacs.d/
              "ezkeys"
              "config"
              "hoon-mode.el"
              )))
  (setq paths (mapcar #'(lambda (path) (file-truename
                                   (concat user-emacs-directory path)))
                      paths))
  (setq load-path (append paths load-path)))


;;;; ===========================================================================
;;;;                                    requires

(require 'd:misc)
(require 'd:org)
(require 'd:better-theme-switching)


(require 'abbrev)
(require 'ace-window)
(require 'avy)
(require 'beacon)
(require 'bookmark)
(require 'cc-vars)
(require 'company)
(require 'counsel)
(require 'diminish)
(require 'dired-x)                      ;additional dired functionality
(require 'ezkeys)
(require 'flyspell)
(require 'ivy)
(require 'magit)
(require 'misc)
(require 'package)
(require 'rainbow-delimiters)
(require 'seq)
(require 'undo-tree)
(require 'which-key)
(require 'winner)
(require 'yasnippet)
(require 'hoon-mode)
(require 'forge)
(require 'github-review)


;;;; ===========================================================================
;;;;                                    desktop
;; (set-face-attribute 'default nil :font "fixedsys" :height 120)
;; (set-face-attribute 'default nil :font "Terminus (TTF)" :height 120)
(set-face-attribute 'default nil :font "IBM Plex Mono" :height 120)

;;;; ===========================================================================
;;;;                                     mobile
;; (set-face-attribute 'default nil :font "fixedsys" :height 110)
;; (set-face-attribute 'default nil :font "Terminus (TTF)" :height 110)
;; (set-face-attribute 'default nil :font "IBM Plex Mono" :height 100)



;;;; ===========================================================================
;;;;                                     ezkeys

(setq ezk-keymap-path (concat user-emacs-directory "init.el"))
(diminish 'ezk-minor-mode)
(ezk-defkeymaps
 ;; precedence
 (c-mode
  scheme-mode)
 ;; groups
 ((G GLOBAL)
  (CC c-mode c++-mode)
  (LISP emacs-lisp-mode scheme-mode-hook lisp-mode)
  (HOON hoon-mode)
  (GUD gud-mode))

 ;; overrides `subword-mode' :(
 ;; ("M-f" (forward-to-word G))
 ;; ("M-b" (backward-to-word G))

 ;; map
 ("M-<f12>" (d/load-next-theme G))

 ;; ("M-x" (counsel-M-x G))
 ;; ("M-X" (smex-major-mode-commands G))


 ("C-x"
    ("o" (ace-window G))
    ("C-b" (ibuffer G))
    ;; ("C-f" (counsel-find-file G))
    ;; ("u" (undo-tree-visualize G))
    ;; ("b" (ivy-switch-buffer G))
    ("x g" (revert-buffer G))
    )

 ("C-c"
    ;; GLOBAL
    ("C-r" (ivy-resume G))
    ;; GUD
    ("C-r" (gud-cont GUD))                     ;override ivy-resume
    ;; ("C-w" (gud-watch GUD))                    ;override `c-subword-mode'
    ("C-g" (gdb-frame-disassembly-buffer GUD)) ;show disasm in new frame
    )

 ;; ("C-h"
 ;;    ("v" (counsel-describe-variable G))
 ;;    ("f" (counsel-describe-function G))
 ;;    ("l" (counsel-find-library G))
 ;;    ("S" (counsel-info-lookup-symbol G))
 ;;     )

 ("C-<tab>" (magit-section-cycle-diffs magit-status-mode))
 ("C-;"
    ("m"
       ("m" (magit-status G))
       ("f" (magit-find-file G))
       ("c" (magit-file-checkout G))
       ("l" (magit-log-buffer-file G)))
    ("a" (avy-goto-line G))
    ("f" (find-dired G))
    ("C-f" (counsel-fzf G))
    ;; ("C-/" (company-files G))
    ("C-s" (counsel-ag G))
    ("u" (browse-url G))

    ("C-h" (man-follow CC))

    ("C-d" (d-dired-dotfiles-toggle dired-mode))
    ("0 w" (d/copy-file:line G))
    ("c" (compile G))
    )

 ("C-S-s" (occur G))

;;;; ===========================================================================
;;;;                                   temporary

 ;; HOON
 ;; useful if we're planning on adding indent and/or alignment to `hoon
 ("C-X v" ((lambda () (interactive) (message (format "%s" (parse-partial-sexp 786 832)))) HOON))

 ;; for whatever reason, the current definition of `make-frame-command'
 ;;
 ;; (if (display-graphic-p)
 ;;     (make-frame)
 ;;   (select-frame (make-frame)))
 ;;
 ;; Doesn't actually select the newly created frame. Redefining as such works though
 ;; has the effect of only allowing two though.
 ;;
 ;; see google: emacs make-frame gnome "is ready"
 ;;     google: gnome allow focus stealing
 ;;     https://major.io/2015/07/06/allow-new-windows-to-steal-focus-in-gnome-3/#:~:text=Open%20dconf%2Deditor%20and%20navigate,and%20you%20can%20select%20strict.
 ;;
 ;; ("C-x 5 2" ((lambda () (interactive) (switch-to-buffer-other-frame (current-buffer)))
 ;;             G))

 )

(define-key org-mode-map (kbd "C-<tab>") 'org-global-cycle)

(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir t)))))


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
(add-to-list 'auto-mode-alist '("\\.hoon\\'" . hoon-mode))
(add-to-list 'auto-mode-alist '("\\.BUILD\\'" . bazel-mode))



;;;; ===========================================================================
;;;;                             general configuration

(when (display-graphic-p)
  (setq default-frame-alist
        '((width . 130)
          (height . 100)
          (top . 200))))

;; only warn about file local variables when considered unsafe
(setq enable-local-variables t)

;; x clipboard support
(setq select-enable-clipboard t)
(setq x-select-enable-clipboard-manager t)

;; STOP RINGING
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

;; pdfs not fuzzy
(setq doc-view-resolution 300)

(setq bookmark-save-flag t
      bookmark-use-annotations t)

(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)


;;;; ===========================================================================
;;;;                              filewrite operations

(setq delete-by-moving-to-trash nil)

(setq make-backup-files t
      auto-save-default t
      create-lockfiles nil)

(let ((dir (concat user-emacs-directory "backups")))
  (d/ensure-dir-exists dir)
  (setq backup-by-copying t
        backup-directory-alist `(("." . ,dir))
        delete-old-versions t
        kept-new-versions 5
        kept-old-versions 5
        version-control t))



;;;; ===========================================================================
;;;;                                   undo-tree

(let ((dir (concat user-emacs-directory "undo-tree-hist")))
  (d/ensure-dir-exists dir)

  ;; sets directory where persistent undo history is stored
  (setq undo-tree-history-directory-alist
        `(                              ; "For the common case of all backups going into one
          ("." . ,dir)                  ; directory, the alist should contain a single element
                                        ; pairing "." with the appropriate directory name."
          )))

(defalias #'redo #'undo-tree-redo)
(defalias #'undo #'undo-tree-undo)
(setq undo-tree-auto-save-history t)
(setq undo-tree-visualizer-diff t)
(setq undo-tree-visualizer-timestamps t)
(global-undo-tree-mode 1)
(diminish 'undo-tree-mode)



;;;; ===========================================================================
;;;;                             navigation and windows

(winner-mode)

;; avy for faster navigation inside and outside buffers
(let ((keys '(?a ?s ?d ?f ?g
                 ?h ?j ?k ?l ?\;
                 ?q ?w ?e ?r ?t
                 ?u ?i ?o ?p
                 ?m ?n)))
  (setq avy-keys keys)
  (setq aw-keys keys))


;;;; ===========================================================================
;;;;                            company can stay for now


;; package company-ctags ?

;; (define-key company-active-map (kbd "C-n") 'company-select-next)
;; (define-key company-active-map (kbd "C-p") 'company-select-previous)

(setq company-dabbrev-downcase nil)     ;otherwise completion is downcase for plaintext
(setq company-minimum-prefix-length 3)
(setq company-tooltip-limit 15)
(setq company-default-idle-delay 0.0)
(setq company-idle-delay company-default-idle-delay)

;; if idle delay is non-nil, tramp will hang a lot.
(add-hook 'tramp--startup-hook
          (lambda ()
            ;; disable auto completion in tramp
            (setq-local company-idle-delay nil)))

(global-company-mode 1)


;;;; ===========================================================================
;;;;                                  ivy/counsel

(setq ivy-height 32)                  ;32 candidates
(setq ivy-use-virtual-buffers t)

(setq counsel-find-file-at-point t)



;;;; ===========================================================================
;;;;                                     dired

(setq dired-listing-switches "-Al -v --human-readable")
(setq dired-dwim-target t)
;; (setq dired-omit-mode t)                ;this hides .elc among others

(add-hook 'org-mode-hook #'flyspell-mode)
(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)

(setq tramp-default-method "ssh")


;;;; ===========================================================================
;;;;                                  epa easy pgp
(setq epg-key-id "David Wimmel")


;;;; ===========================================================================
;;;;                                   calc-mode

;; so that binary operations can be performed on 64 bits
(setq calc-word-size 128)



;;;; ===========================================================================
;;;;                                     visual

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

(which-key-mode)
(diminish 'which-key-mode)

;; easily see cursor
(beacon-mode 1)
(diminish 'beacon-mode)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; (use-package gruvbox-theme :ensure t)
;; (use-package modus-operandi-theme :ensure t)
;; (use-package modus-vivendi-theme :ensure t)
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/gruvbox")

(d/defthemes
 ;; (leuven
 ;;  :before
 ;;  ((mapc #'disable-theme custom-enabled-themes) ;disable all themes currently enabled
 ;;   )

 ;;  :after
 ;;  ((custom-set-faces '(ivy-current-match ((((class color) (background light))
 ;;                                           (:background "#1a4b77" :foreground "white" :extend t))
 ;;                                          (((class color) (background dark))
 ;;                                           (:background "#65a7e2" :foreground "black" :extend t)))))
 ;;   (message "==loaded light theme")
 ;;   ))

 ;; (spacemacs-dark
 ;;  :before
 ;;  ((mapc #'disable-theme custom-enabled-themes)
 ;;   )

 ;;  :after
 ;;  ((custom-set-faces
 ;;    ;; bright green ivy selection
 ;;    '(ivy-current-match ((t (:foreground "chartreuse3" :underline t :weight bold)))))

 ;;   (message "==loaded dark theme"))
 ;;  )

 (modus-vivendi
  :before
  ((mapc #'disable-theme custom-enabled-themes)
   )
  )
 (modus-operandi
  :before
  ((mapc #'disable-theme custom-enabled-themes)
   )
  )

 (gruvbox-dark-hard
  :before
  ((mapc #'disable-theme custom-enabled-themes)
   )
  )

 (gruvbox-light-hard
  :before
  ((mapc #'disable-theme custom-enabled-themes)
   )
  )
 )

;; loads first theme. Subsequent calls load the next
(d/load-next-theme)



;;;; ===========================================================================
;;;;                     text(formatting|nav)/programming langs


(global-subword-mode 1)                 ;word defn is finer grained. e.g. camelCase - 2 words
(diminish 'subword-mode)
(delete-selection-mode 1)               ;highlighted region gets deleted on input

(setq global-eldoc-mode t
      eldoc-idle-delay 0.00             ;reduce time it takes for eldoc to pop up
      eldoc-print-after-edit nil        ;documentation is show even when not editing
      irony-eldoc-use-unicode nil)      ;use ∷ and ⇒ instead of :: and =>

(setq electric-pair-pairs '((?\( . ?\))
                            (?\[ . ?\])
                            (?\{ . ?\})
                            (?\" . ?\")))
(electric-pair-mode t)

;; yasnippet
(yas-global-mode 1)
(diminish 'yas-minor-mode)

(read-abbrev-file "~/.emacs.d/abbrevs.el")
(abbrev-mode 1)

;; magit
(setq magit-diff-refine-hunk 'all     ;word-level diff in magit-status buffers
      magit-diff-paint-whitespace t
      magit-diff-highlight-trailing t)


;; gdb
(setq gdb-many-windows t
      gdb-show-main t                   ;on start, src code window shows main
      gdb-speedbar-auto-raise t         ;raise watch frame to foreground upon var change
      gdb-show-changed-values t         ;speedbar watch frame
      gdb-use-colon-colon-notation t    ;FUN::VAR format for vars in watch frame
      )


;;;; ===========================================================================
;;;;                                   emacs-lisp

(add-hook 'emacs-lisp-mode-hook
          (lambda () (setq-local fill-column 100)))


;;;; ===========================================================================
;;;;                                       sh

;; use this for bash completion:
;; https://github.com/szermatt/emacs-bash-completion
(add-hook 'sh-mode-hook
          (lambda () (sh-electric-here-document-mode -1)) ;poorly implemented
          )


;;;; ===========================================================================
;;;;                                     eshell

(add-hook 'eshell-mode-hook
          (lambda ()
            (company-mode -1)))



;;;; ===========================================================================
;;;;                                     forge


(setq auth-sources '("~/.authinfo"))

;; forge miscellany
;;
;; storing secrets in plaintext in ~/.authinfo. See https://magit.vc/manual/ghub/Storing-a-Token.html
;;
;; for ssh subdomain stuff: https://github.com/magit/forge/issues/280 - remember, ~/.ssh/config
;; defines a host entry "Urbit" that allows us to use a different ssh identity for git urls of the
;; form: git@urbit:urbit/SUBREPO.git
;;
;; we need to inform forge of that like so:
;;

(setq forge-alist (append forge-alist
                          '(("urbit"
                            "api.github.com"
                            "github.com"
                            forge-github-repository))))


;; additionally, remember that forge requires configuring the following git local variable:
;; "github.user". This is different from the typical user.name configured in git.
;;
;; git config --local github.user <GITHUB_USER_NAME>
;;

;; github-review also requires a token to be stored in .authinfo (or .authinfo.gpg, etc)
;; https://github.com/charignon/github-review
(setq github-review-view-comments-in-code-lines t
      github-review-view-comments-in-code-lines-outdated nil)




;;;; ===========================================================================
;;;;                                     custom

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(github-review edit-indirect forge rust-mode markdown-mode bazel diminish gruvbox-theme modus-themes nix-mode undo-tree which-key wgrep-ag use-package spacemacs-theme rainbow-delimiters counsel beacon ace-window))
 '(safe-local-variable-values
   '((major-mode . gdb-script-mode)
     (explicit-shell-file-name . /bin/bash))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ivy-current-match ((t (:foreground "chartreuse3" :underline t :weight bold)))))
(put 'dired-find-alternate-file 'disabled nil)
