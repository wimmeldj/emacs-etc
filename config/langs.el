;;;; ===========================================================================
;;;;                                    general

(global-subword-mode 1)                 ;word defn is finer grained. e.g. camelCase - 2 words
(delete-selection-mode 1)               ;highlighted region gets deleted on input

(setq global-eldoc-mode t
      eldoc-idle-delay 0.00        ;reduce time it takes for eldoc to pop up
      eldoc-print-after-edit nil   ;documentation is show even when not editing
      irony-eldoc-use-unicode nil)  ;use ∷ and ⇒ instead of :: and =>

(custom-set-faces
 '(eldoc-highlight-function-argument ((t (:inherit bold
                                                   :foreground "#98971a"
                                                   :height 1.3)))))

(use-package magit
  :ensure t
  :config
  (setq magit-diff-refine-hunk 'all     ;word-level diff in magit-status buffers
        magit-diff-paint-whitespace t
        magit-diff-highlight-trailing t
        ) )

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

(read-abbrev-file (concat user-emacs-directory "abbrevs.el"))
(abbrev-mode 1)



;;;; ===========================================================================
;;;;                                     clang 


(require 'cc-vars)
(push '(c-mode . "k&r") c-default-style)
(push '(c++-mode . "k&r") c-default-style)

(let ((clangd-exit (call-process "which" nil nil nil "clangd"))
      (bear-exit (call-process "which" nil nil nil "bear"))
      (cmake-exit (call-process "which" nil nil nil "cmake")))
  (when (not (= clangd-exit 0))
    (message "==LSP: clang not found in PATH. Need this for lsp-mode completion of c/c++"))
  (when (not (= bear-exit 0))
    (message "==LSP: bear not found in PATH. Need this for clangd project setup of projects built with make. See https://clangd.llvm.org/installation.html"))
  (when (not (= cmake-exit 0))
    (message "==LSP: cmake not found in PATH. Need this for clangd project setup of projects built with cmake. See https://clangd.llvm.org/installation.html")))

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1
      lsp-headerline-breadcrumb-enable t
      lsp-keymap-prefix "M-l")
(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools))




;;;; ===========================================================================
;;;;                                       c# 

;; (use-package omnisharp :ensure t
;;   :config
;;   (add-hook 'csharp-mode-hook #'omnisharp)
;;   (require 'company)
;;   (add-to-list 'company-backends #'company-omnisharp)
;;   (add-hook 'csharp-mode-hook #'flycheck-mode)
;;   (add-hook 'csharp-mode-hook #'eldoc-mode)

;;   (require 'csharp-mode)
;;   (define-key csharp-mode-map (kbd "M-.") #'omnisharp-go-to-definition)
;;   :after (company))


;;;; ===========================================================================
;;;;                                      web 

;; (use-package web-mode
;;   :ensure t
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;;   (setq web-mode-markup-indent-offset 2)
;;   (setq web-mode-css-indent-offset 2)
;;   (setq web-mode-code-indent-offset 2))

;; (use-package company-web
;;   :ensure t
;;   :after (company web-mode)
;;   :config
;;   (add-to-list 'company-backends #'company-web-html)
;;   ;; (add-hook 'html-mode #'company-web-html)
;;   (define-key web-mode-map (kbd "C-c C-.") 'company-web-html)
;;   (add-to-list 'company-backends #'company-css))



;;;; ===========================================================================
;;;;                                       cl 

(defun set-inferior-lisp (name)
  (with-temp-buffer
    (let ((res (call-process "which" nil (current-buffer) nil name))
          (output (string-remove-suffix "\n" (buffer-string))))
      (if (= res 0)
          (message "==inferior lisp, %s, located at %s" name
                   (setq inferior-lisp-program output))
        (message "==SLIME: tried to located inferior lisp, %s, but got the following error [%s]"
                 name output)))))

(use-package slime
  :ensure t
  :config
  
  (add-hook 'slime-mode-hook
            (lambda () (setq-local fill-column 100)))

  (set-inferior-lisp "sbcl")

  ;; load "almost all of the popular contribs"
  ;; (setq slime-contribs '(slime-fancy slime-company))

  (slime-setup '(slime-fancy))

  (add-hook 'lisp-mode-hook #'slime-mode)
  )




;;;; ===========================================================================
;;;;                                   emacs-lisp 
(add-hook 'emacs-lisp-mode-hook
          (lambda () (setq-local fill-column 100)))

(require 'pp)                           ;printy print
(define-key emacs-lisp-mode-map (kbd "C-x C-e") #'eval-last-sexp)
(define-key emacs-lisp-mode-map
  (kbd "C-x e") #'pp-eval-last-sexp) ;with universal arg, inserts results at point


;;;; ===========================================================================
;;;;                                     scheme 

;; get a repl up with C-C C-z
(use-package geiser
  :ensure t
  :config
  (setq geiser-active-implementations '(chez))
  (setq geiser-default-implementation 'chez)
  (add-hook 'scheme-mode-hook #'geiser-mode))



;;;; ===========================================================================
;;;;                                       js 

(use-package js2-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'interpreter-mode-alist '("node" . js2-mode)))



;;;; ===========================================================================
;;;;                                       sh

(add-hook 'sh-mode
          (lambda () (sh-electric-here-document-mode -1)) ;poorly implemented
          )

(provide 'langs)
