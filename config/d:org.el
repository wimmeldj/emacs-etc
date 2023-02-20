(require 'org)

;; used by org to produce html files
;; (use-package htmlize
;;   :ensure t)

(add-hook 'org-mode-hook
          (lambda ()
            (setq fill-column 100
                  ;; redefines org's definition of paragraph start and end to be compatible with
                  ;; evil mode's notion of "a paragraph"
                  ;; paragraph-start "\\|[         ]*$"
                  ;; paragraph-separate "[   ]*$"
                  )
            (auto-fill-mode 1)
            (org-indent-mode 1) ;soft indentation.
            (org-hide-block-all)
            ))

(setq fill-column 100
      org-src-window-setup 'current-window ; sets where org-edit-special takes you (C-c ')
      org-hide-block-startup nil           ; don't hide blocks on startup
      org-startup-with-inline-images t
      org-todo-keywords '((sequence "TODO" "DOING" "DONE"))
      org-todo-keyword-faces '(("DOING" . "yellow"))
      org-confirm-babel-evaluate nil    ;don't confirm eval of src code block
      org-adapt-indentation nil         ;disable hard indentation
      org-hide-leading-stars nil
      )

(setq d/org-default-setup-file (concat user-emacs-directory "org-default-setup.org/"))

(setq org-babel-python-command "python3")
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (C . t)
   ;; (R . t)
   ;; (dot . t)
   (lisp . t)
   (shell . t)
   (scheme . t)
   ;; (awk . t)
   ;; (sed . t)
   (emacs-lisp . t)
   ;; (sql . t)
   ;; (js . t)
   ))

(define-key org-mode-map
  (kbd "C-c C-'")
  'org-babel-expand-src-block)


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
  (format "#+SETUPFILE: %s\n"
          (if (y-or-n-p "Custom setup file?")
              (file-relative-name (read-file-name "path: ") default-directory)
            d/org-default-setup-file))
  "#+OPTIONS: html-style:t"\n
  "#+OPTIONS: toc:t"\n
  "#+OPTIONS: tex:t"\n
  "#+OPTIONS: html-postamble:nil"\n
  (let ((todo-kwords ""))
    (cl-loop for kword in (cdar org-todo-keywords)
          do (setq todo-kwords (concat todo-kwords " " kword)))
    (format "#+TODO: %s\n" todo-kwords))
  "#+PROPERTY: header-args :results output"\n
  "#+FILETAGS: :ex1:ex2:"\n
  "#+STARTUP: overview")

(provide 'd:org)


