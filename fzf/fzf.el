(setq fzf/executable "fzf")
(setq fzf/wsl nil)

(setq fzf/executable "fzf.exe")
(setq fzf/wsl t)

(setq fzf/args "-x --print-query --print0")

;;; fzf.el --- A front-end for fzf.
;;
;; Copyright (C) 2015 by Bailey Ling
;; Author: Bailey Ling
;; URL: https://github.com/bling/fzf.el
;; Package-Version: 20180619.145
;; Package-Commit: 521d18933cb586337c4e34281bdc71ac07202c98
;; Filename: fzf.el
;; Description: A front-end for fzf
;; Created: 2015-09-18
;; Version: 0.0.2
;; Package-Requires: ((emacs "24.4"))
;; Keywords: fzf fuzzy search
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;; 
;; 
;; Forked to work reasonably well with fzf as a window binary run from
;; wsl. There are still issues when you backspace an initial search, but good
;; enough. This seems to be a problem with `term' running in wsl1
;;
;;
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Install:
;;
;; Autoloads will be set up automatically if you use package.el.
;;
;; Usage:
;;
;; M-x fzf
;; M-x fzf-directory
;; M-x fzf-git
;; M-x fzf-git-files
;; M-x fzf-hg
;; M-x fzf-projectile
;; M-x fzf-git-grep
;;
;;; Code:

(require 'subr-x)

(defvar fzf/wsl nil)

(defvar fzf/success-msg "Process fzf finished")

(defgroup fzf nil
  "Configuration options for fzf.el"
  :group 'convenience)

(defcustom fzf/window-height 15
  "The window height of the fzf buffer"
  :type 'integer
  :group 'fzf)

(defcustom fzf/executable "fzf"
  "The path to the fzf executable."
  :type 'string
  :group 'fzf)

(defcustom fzf/args "-x --color bw --print-query"
  "Additional arguments to pass into fzf."
  :type 'string
  :group 'fzf)

(defcustom fzf/git-grep-args "-i --line-number %s"
  "Arguments to pass into git grep. %s is the search term placeholder"
  :type 'string
  :group 'fzf)

(defcustom fzf/position-bottom t
  "Set the position of the fzf window. Set to nil to position on top."
  :type 'bool
  :group 'fzf)

(defcustom fzf/directory-start nil
  "The path of the default start directory for fzf-directory."
  :type 'string
  :group 'fzf)

(defun fzf/grep-cmd (cmd args)
  (format (concat cmd " " args)
          (shell-quote-argument
           (if (region-active-p)
               (buffer-substring-no-properties (region-beginning) (region-end))
             (read-from-minibuffer (concat cmd ": "))))))

(defun fzf/after-term-handle-exit (process-name msg)
  (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
         (lines (split-string text "\n" t "\s*>\s+"))
         (line (if fzf/wsl
                   (string-trim (second lines))
                 (car (last (butlast lines 1)))))
         (selected (split-string line ":"))
         (file (expand-file-name (first selected)))
         (linenumber (second selected)))
    (kill-buffer "*fzf*")
    (jump-to-register :fzf-windows)
    (when (file-exists-p file)
      (find-file file))
    (when linenumber
      (goto-char (point-min))
      (forward-line (- (string-to-number linenumber) 1))
      (back-to-indentation)))
  (advice-remove 'term-handle-exit #'fzf/after-term-handle-exit))

;; original implementation relied on fzf printing the selected file after
;; exit. When run in `term', fzf.exe does that. But for some reason, the
;; selected file is /not/ included in the *fzf* buffer which is running a `term'
;; process. I couldn't figure out why, so instead we'll just find the match from
;; the *fzf* buffer by selecting the first line starting with ">" (the default
;; fzf cursor) So, this doesn't work if for some reason you modified the
;; cursor. This one doesn't have the linenumber functionality, becuase I don't
;; understand how it's working and don't use it.
(defun fzf/after-term-handle-exit-wsl (process-name msg)
  (require 'seq)
  (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
         (lines (split-string text "\n" t " *"))
         (starts-with-> (seq-filter (lambda (x) (and (> (length x) 1)
                                                (equal ?> (elt x 0))))
                                    lines))
         (selected (string-trim (substring (car starts-with->) 1 nil)))
         (file (expand-file-name (replace-regexp-in-string "\\\\" "/" selected)))
         (success-p (catch 'found
                      (dotimes (i (length lines))
                        (when (equal (elt lines i) fzf/success-msg)
                          (throw 'found t))))))
    (kill-buffer "*fzf*")
    (jump-to-register :fzf-windows)
    (when (and success-p (file-exists-p file))
      (find-file file)))
  (advice-remove 'term-handle-exit #'fzf/after-term-handle-exit-wsl))

(defun fzf/start (directory &optional cmd-stream)
  (require 'term)
  (window-configuration-to-register :fzf-windows)
  (if fzf/wsl
      (advice-add 'term-handle-exit :after #'fzf/after-term-handle-exit-wsl)
    (advice-add 'term-handle-exit :after #'fzf/after-term-handle-exit))
  (let* ((buf (get-buffer-create "*fzf*"))
         (min-height (min fzf/window-height (/ (window-height) 2)))
         (window-height (if fzf/position-bottom (- min-height) min-height))
         (window-system-args (when window-system " --margin=1,0"))
         (fzf-args (concat fzf/args window-system-args))
         (sh-cmd (if cmd-stream (concat cmd-stream " | " fzf/executable " " fzf-args)
                   (concat fzf/executable " " fzf-args))))
    (with-current-buffer buf
      (setq default-directory directory))
    (split-window-vertically window-height)
    (when fzf/position-bottom (other-window 1))
    (make-term "fzf" "sh" nil "-c" sh-cmd)
    ;; (make-term "fzf" "fzf.exe" nil "-x --print-query")
    (switch-to-buffer buf)
    (linum-mode 0)
    (visual-line-mode 0)

    ;; disable various settings known to cause artifacts, see #1 for more details
    (setq-local scroll-margin 0)
    (setq-local scroll-conservatively 0)
    (setq-local term-suppress-hard-newline t) ;for paths wider than the window
    (setq-local show-trailing-whitespace nil)
    (setq-local display-line-numbers nil)
    (face-remap-add-relative 'mode-line '(:box nil))

    (term-char-mode)
    (setq mode-line-format (format "   FZF  %s" directory))))

(defun fzf/vcs (match)
  (let ((path (locate-dominating-file default-directory match)))
    (if path
        (fzf/start path)
      (fzf-directory))))

(defun fzf/git-files ()
  (let ((process-environment
         (cons (concat "FZF_DEFAULT_COMMAND=git ls-files")
               process-environment))
        (path (locate-dominating-file default-directory ".git")))
    (if path
        (fzf/start path)
      (user-error "Not inside a Git repository"))))

;;;###autoload
(defun fzf ()
  "Starts a fzf session."
  (interactive)
  (if (fboundp #'projectile-project-root)
      (fzf/start (condition-case err
                     (projectile-project-root)
                   (error
                    default-directory)))
    (fzf/start default-directory)))

;;;###autoload
(defun fzf-directory ()
  "Starts a fzf session at the specified directory."
  (interactive)
  (fzf/start (ido-read-directory-name "Directory: " fzf/directory-start)))

;;;###autoload
(defun fzf-git ()
  "Starts a fzf session at the root of the current git."
  (interactive)
  (fzf/vcs ".git"))

;;;###autoload
(defun fzf-git-files ()
  "Starts a fzf session only searching for git tracked files."
  (interactive)
  (fzf/git-files))

;;;###autoload
(defun fzf-hg ()
  "Starts a fzf session at the root of the curreng hg."
  (interactive)
  (fzf/vcs ".hg"))

;;;###autoload
(defun fzf-projectile ()
  "Starts a fzf session at the root of the projectile project."
  (interactive)
  (require 'projectile)
  (fzf/start (projectile-project-root)))

;;;###autoload
(defun fzf-git-grep ()
  "Starts a fzf session based on git grep result. The input comes
   from the prompt or the selected region"
  (interactive)
  (fzf/start (locate-dominating-file default-directory ".git")
             (fzf/grep-cmd "git grep" fzf/git-grep-args)))

(provide 'fzf)
;;; fzf.el ends here
