;; emacs c-src
(setq source-directory "/home/d/.emacs.d/emacs-27.1/")
;; below is unnecessary so long as `find-func' hasn't been loaded yet.
;; (setq find-function-C-source-directory
;;   (let ((dir (expand-file-name "src" source-directory)))
;;     (if (file-accessible-directory-p dir) dir)))

(package-initialize)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")
			 ("melpa-stable" . "http://stable.melpa.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")
;;			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ))


;;;; ===========================================================================
;;;;                                    desktop
;; (set-face-attribute 'default nil :font "fixedsys" :height 120)
;; (set-face-attribute 'default nil :font "Terminus (TTF)" :height 120)
(set-face-attribute 'default nil :font "IBM Plex Mono" :height 90)

;;;; ===========================================================================
;;;;                                     mobile
;; (set-face-attribute 'default nil :font "fixedsys" :height 110)
;; (set-face-attribute 'default nil :font "Terminus (TTF)" :height 110)
;; (set-face-attribute 'default nil :font "IBM Plex Mono" :height 100)

;;;; ===========================================================================
;;;;                    pacakges installed without `use-package' 
(require 'package)
(require 'seq)
(let* ((packages '(use-package
		    helm
		    spacemacs-theme
                    ;; from https://emacs-lsp.github.io/lsp-mode/tutorials/CPP-guide/
                    ;; figure out what you really want to keep later
                    lsp-mode
                    lsp-treemacs
                    helm-lsp       ;`helm-lsp-workspace-symbol' `helm-lsp-global-workspace-symbol' and `helm-lsp-code-actions'
                    lsp-ivy        ;`lsp-ivy-workspace-symbol' and `lsp-ivy-global-workspace-symbol'
                    projectile
                    hydra
                    flycheck
                    company
                    avy
                    which-key
                    helm-xref
                    dap-mode
		    ))
       (notinstalled (seq-filter #'(lambda (pkg) (not (package-installed-p pkg)))
				 packages)))
  (when notinstalled
    (message "==The following packages will be installed: %s" (format "%S" notinstalled))
    (package-refresh-contents)
    (mapc #'package-install notinstalled)))

;;;; ===========================================================================
;;;;                                   load-path 

(add-to-list 'load-path (concat user-emacs-directory "config/"))
(add-to-list 'load-path (concat user-emacs-directory "config/hacks/"))
(add-to-list 'load-path (concat user-emacs-directory "eshell-ring/"))
(add-to-list 'load-path (concat user-emacs-directory "ezkeys/"))
(add-to-list 'load-path (concat user-emacs-directory "evil-numbers/"))
(add-to-list 'load-path (concat user-emacs-directory "fzf/"))


(require 'main)

(setq ezk-keymap-path (concat user-emacs-directory "init.el"))
(require 'ezkeys)

(ezk-defkeymaps
 ;; precedence
 (c-mode
  scheme-mode)
 ;; groups
 ((G GLOBAL)
  (CC c-mode c++-mode)
  (LISP emacs-lisp-mode scheme-mode-hook lisp-mode))

 ;; map
 ("M-<f12>" (d-load-next-theme G))

 ;; ("M-u" (universal-argument G))
 ("M-x" (counsel-M-x G))
 ("M-X" (smex-major-mode-commands G))
 ;; ("M-x" (smex G))


 ("C-x"
    ("o" (ace-window G))
    ("C-b" (ibuffer G))
    ("C-f" (counsel-find-file G))
    ("u" (undo-tree-visualize G))
    ("b" (ivy-switch-buffer G))
    ;; ("r i" (counsel-register G))
    )

 ("C-c"
    ("C-r" (ivy-resume G))
    )

 ("C-h"
    ("v" (counsel-describe-variable G))
    ("f" (counsel-describe-function G))
    ("l" (counsel-find-library G))
    ("S" (counsel-info-lookup-symbol G))
    ("l" (find-library G))
    )

 ("C-;"
    ("m"
       ("m" (magit-status G))
       ("f" (magit-find-file G))
       ("c" (magit-file-checkout G))
       ("l" (magit-log-buffer-file G)))
    ("a" (avy-goto-line G))
    ("C-f" (fzf G))
    ;; ("C-f" (counsel-fzf G))
    ("C-/" (company-files G))
    ("C-s" (counsel-ag G))
    ("u" (browse-url G))

    ("C-h" (man-follow CC))

    ("C-d" (d-dired-dotfiles-toggle dired-mode))
    )

 ;; ("C-w" ("C-h" (winner-undo G))
 ;;        ("C-l" (winner-redo G)))

 ("C-S-s" (occur G))
 )




;;;; ===========================================================================
;;;;                        evil hack to work with `ezkeys' 
;; (define-minor-mode evil-local-mode
;;   "Minor mode for setting up Evil in a single buffer."
;;   :init-value nil
;;   (cond
;;    ((evil-disabled-buffer-p)
;;     ;; Don't leave the mode variable on in buffers where evil disabled, because
;;     ;; functions that check this variable will get an incorrect result (e.g.,
;;     ;; evil-refresh-cursor).
;;     (setq evil-local-mode nil))
;;    (evil-local-mode
;;     (setq emulation-mode-map-alists
;;           (if (cdr emulation-mode-map-alists)
;;               (evil-concat-lists (list (car emulation-mode-map-alists)) '(evil-mode-map-alist) emulation-mode-map-alists)
;;             (evil-concat-lists '(evil-mode-map-alist) emulation-mode-map-alists)))
;;     (evil-initialize-local-keymaps)
;;     ;; restore the proper value of `major-mode' in Fundamental buffers
;;     (when (eq major-mode 'turn-on-evil-mode)
;;       (setq major-mode 'fundamental-mode))
;;     (when (minibufferp)
;;       (setq-local evil-default-state 'insert)
;;       (setq-local evil-echo-state nil))
;;     ;; The initial state is usually setup by `evil-initialize' when
;;     ;; the major-mode in a buffer changes. This preliminary
;;     ;; initialization is only for the case when `evil-local-mode' is
;;     ;; called directly for the first time in a buffer.
;;     (unless evil-state (evil-initialize-state))
;;     (add-hook 'input-method-activate-hook 'evil-activate-input-method t t)
;;     (add-hook 'input-method-deactivate-hook 'evil-deactivate-input-method t t)
;;     (add-hook 'activate-mark-hook 'evil-visual-activate-hook nil t)
;;     (add-hook 'pre-command-hook 'evil-repeat-pre-hook)
;;     (add-hook 'post-command-hook 'evil-repeat-post-hook))
;;    (t
;;     (evil-refresh-mode-line)
;;     (remove-hook 'activate-mark-hook 'evil-visual-activate-hook t)
;;     (remove-hook 'input-method-activate-hook 'evil-activate-input-method t)
;;     (remove-hook 'input-method-deactivate-hook 'evil-deactivate-input-method t)
;;     (evil-change-state nil))))





















































(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-safe-themes
   '("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "4cf9ed30ea575fb0ca3cff6ef34b1b87192965245776afa9e9e20c17d115f3fb" "d0fd069415ef23ccc21ccb0e54d93bdbb996a6cce48ffce7f810826bb243502c" "ffba0482d3548c9494e84c1324d527f73ea4e43fff8dfd0e48faa8fc6d5c2bc7" "123a8dabd1a0eff6e0c48a03dc6fb2c5e03ebc7062ba531543dfbce587e86f2a" "a06658a45f043cd95549d6845454ad1c1d6e24a99271676ae56157619952394a" "939ea070fb0141cd035608b2baabc4bd50d8ecc86af8528df9d41f4d83664c6a" "4c0739c6ad6fd91ebd737f8f40527d279cc5f85bc286a7c0d7467b4a6ba53166" "d97baf5a34c87b05508739505cad03438cde8efa2a0d350c7773f2a8bc26a50d" "8f5b54bf6a36fe1c138219960dd324aad8ab1f62f543bed73ef5ad60956e36ae" "cbd85ab34afb47003fa7f814a462c24affb1de81ebf172b78cb4e65186ba59d2" "2b9dc43b786e36f68a9fd4b36dd050509a0e32fe3b0a803310661edb7402b8b6" "8e797edd9fa9afec181efbfeeebf96aeafbd11b69c4c85fa229bb5b9f7f7e66c" "585942bb24cab2d4b2f74977ac3ba6ddbd888e3776b9d2f993c5704aa8bb4739" "8f97d5ec8a774485296e366fdde6ff5589cf9e319a584b845b6f7fa788c9fa9a" "a22f40b63f9bc0a69ebc8ba4fbc6b452a4e3f84b80590ba0a92b4ff599e53ad0" "b583823b9ee1573074e7cbfd63623fe844030d911e9279a7c8a5d16de7df0ed0" default))
 '(flymake-error-bitmap '(flymake-double-exclamation-mark modus-theme-fringe-red))
 '(flymake-note-bitmap '(exclamation-mark modus-theme-fringe-cyan))
 '(flymake-warning-bitmap '(exclamation-mark modus-theme-fringe-yellow))
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f")))
 '(ibuffer-deletion-face 'dired-flagged)
 '(ibuffer-filter-group-name-face 'dired-mark)
 '(ibuffer-marked-face 'dired-marked)
 '(ibuffer-title-face 'dired-header)
 '(package-selected-packages
   '(lsp-ivy ivy-lsp dap-mode helm-xref projectile helm-lsp lsp-treemacs lsp-mode gruvbox-theme rainbow-blocks geiser omnisharp blimp gnuplot vlf htmlize not-a-package ace-window-mode dired-rsync wgrep-ag wgrep yasippet-snippets yasnippet js2-mode restclient smex web-mode evil-numbers pydoc counsel magit company-jedi slime-company company-web irony-eldoc company-irony which-key use-package tide slime rainbow-mode rainbow-delimiters pdf-tools org-bullets markdown-mode ivy helm golden-ratio evil-vimish-fold eshell-up editorconfig dired-quick-sort dired-collapse diminish company beacon ace-window))
 '(pdf-view-midnight-colors '("#fdf4c1" . "#1d2021"))
 '(vc-annotate-background "#ffffff")
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#ab4642")
     (50 . "#dc9656")
     (80 . "#f7ca88")
     (110 . "#a1b56c")
     (140 . "#86c1b9")
     (170 . "#7cafc2")
     (200 . "#ba8baf")
     (230 . "#a16046")
     (260 . "#181818")
     (290 . "#282828")
     (320 . "#383838")
     (350 . "#585858")))
 '(vc-annotate-very-old-color "#585858")
 '(which-key-mode t)
 '(xterm-color-names
   ["#000000" "#ff8059" "#44bc44" "#eecc00" "#33beff" "#feacd0" "#00d3d0" "#a8a8a8"])
 '(xterm-color-names-bright
   ["#181a20" "#f4923b" "#58dd13" "#e5f040" "#72a4ff" "#f78fe7" "#4ae8fc" "#ffffff"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eldoc-highlight-function-argument ((t (:inherit bold :foreground "#98971a" :height 1.3))))
 '(ivy-current-match ((((class color) (background light)) (:background "#1a4b77" :foreground "white" :extend t)) (((class color) (background dark)) (:background "#65a7e2" :foreground "black" :extend t))))
 '(mode-line-inactive ((((class color) (min-colors 89)) (:foreground "#655370" :background "#fbf8ef" :box (:color "#b3b9be" :line-width 1))))))
(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
