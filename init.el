;; emacs c src
(setq source-directory "/home/d/.emacs.d/emacs-26.3/")
;; below is unnecessary so long as `find-func' hasn't been loaded yet.
;; (setq find-function-C-source-directory
;;   (let ((dir (expand-file-name "src" source-directory)))
;;     (if (file-accessible-directory-p dir) dir)))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'package)
(require 'seq)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")
			 ("melpa-stable" . "http://stable.melpa.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")
;;			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ))

(let* ((packages '(use-package
		    helm
		    spacemacs-theme
		    ))
       (notinstalled (seq-filter #'(lambda (pkg) (not (package-installed-p pkg)))
				 packages)))
  (when notinstalled
    (message "==The following packages will be installed: %s" (format "%S" notinstalled))
    (package-refresh-contents)
    (mapc #'package-install
	  notinstalled)))


(add-to-list 'load-path "~/.emacs.d/small-mods/")
(add-to-list 'load-path "~/.emacs.d/eshell-ring/")
(add-to-list 'load-path "~/.emacs.d/evil-numbers/")
(add-to-list 'load-path "~/.emacs.d/fzf/")


;;; load org config file if it exists.
(when (file-readable-p "~/.emacs.d/config.org")
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))
;; (load "~/.emacs.d/config")          ;byte compiling seems to have no effect on startup time


;;;; ===========================================================================
;;;;                                    desktop
;; (set-face-attribute 'default nil :font "fixedsys" :height 80)
(set-face-attribute 'default nil :font "Terminus (TTF)" :height 70)

;;;; ===========================================================================
;;;;                                     mobile
;; (set-face-attribute 'default nil :font "fixedsys" :height 110)
;; (set-face-attribute 'default nil :font "Terminus (TTF)" :height 110)





























(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "4cf9ed30ea575fb0ca3cff6ef34b1b87192965245776afa9e9e20c17d115f3fb" "d0fd069415ef23ccc21ccb0e54d93bdbb996a6cce48ffce7f810826bb243502c" "ffba0482d3548c9494e84c1324d527f73ea4e43fff8dfd0e48faa8fc6d5c2bc7" "123a8dabd1a0eff6e0c48a03dc6fb2c5e03ebc7062ba531543dfbce587e86f2a" "a06658a45f043cd95549d6845454ad1c1d6e24a99271676ae56157619952394a" "939ea070fb0141cd035608b2baabc4bd50d8ecc86af8528df9d41f4d83664c6a" "4c0739c6ad6fd91ebd737f8f40527d279cc5f85bc286a7c0d7467b4a6ba53166" "d97baf5a34c87b05508739505cad03438cde8efa2a0d350c7773f2a8bc26a50d" "8f5b54bf6a36fe1c138219960dd324aad8ab1f62f543bed73ef5ad60956e36ae" "cbd85ab34afb47003fa7f814a462c24affb1de81ebf172b78cb4e65186ba59d2" "2b9dc43b786e36f68a9fd4b36dd050509a0e32fe3b0a803310661edb7402b8b6" "8e797edd9fa9afec181efbfeeebf96aeafbd11b69c4c85fa229bb5b9f7f7e66c" "585942bb24cab2d4b2f74977ac3ba6ddbd888e3776b9d2f993c5704aa8bb4739" "8f97d5ec8a774485296e366fdde6ff5589cf9e319a584b845b6f7fa788c9fa9a" "a22f40b63f9bc0a69ebc8ba4fbc6b452a4e3f84b80590ba0a92b4ff599e53ad0" "b583823b9ee1573074e7cbfd63623fe844030d911e9279a7c8a5d16de7df0ed0" default)))
 '(flymake-error-bitmap
   (quote
    (flymake-double-exclamation-mark modus-theme-fringe-red)))
 '(flymake-note-bitmap (quote (exclamation-mark modus-theme-fringe-cyan)))
 '(flymake-warning-bitmap (quote (exclamation-mark modus-theme-fringe-yellow)))
 '(ibuffer-deletion-face (quote dired-flagged))
 '(ibuffer-filter-group-name-face (quote dired-mark))
 '(ibuffer-marked-face (quote dired-marked))
 '(ibuffer-title-face (quote dired-header))
 '(package-selected-packages
   (quote
    (omnisharp blimp gnuplot vlf htmlize not-a-package ace-window-mode dired-rsync wgrep-ag wgrep yasippet-snippets yasnippet js2-mode restclient smex web-mode evil-numbers pydoc counsel magit company-jedi slime-company company-web irony-eldoc company-irony which-key use-package tide slime rainbow-mode rainbow-delimiters pdf-tools org-bullets markdown-mode ivy helm golden-ratio evil-vimish-fold eshell-up editorconfig dired-quick-sort dired-collapse diminish company beacon ace-window)))
 '(pdf-view-midnight-colors (quote ("#fdf4c1" . "#1d2021")))
 '(vc-annotate-background "#ffffff")
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#ab4642")
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
     (350 . "#585858"))))
 '(vc-annotate-very-old-color "#585858")
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
 '(ivy-current-match ((t (:foreground "chartreuse3" :underline t :weight bold))))
 '(mode-line-inactive ((((class color) (min-colors 89)) (:foreground "#655370" :background "#fbf8ef" :box (:color "#b3b9be" :line-width 1))))))
(put 'dired-find-alternate-file 'disabled nil)
