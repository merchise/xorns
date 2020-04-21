;;; xorns-bots.el --- Latest `xorns' development versions tools

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module contains tools to be used when using `xorns' in standalone mode
;; to facilitate staying up-to-date on the latest development versions.

;;; Code:

(require 'xorns-tools)


(defconst >>=!pkg-dir (bound-and-true-p >>=init-mode/standalone)
  "Package directory if `xorns' is initialized in standalone mode.")


(defun >>-bot-git-remote-url ()
  "Internal function to get GIT remote URL of `xorns' standalone repository."
  (if >>=!pkg-dir
    (let ((default-directory user-emacs-directory))
      (string-trim (shell-command-to-string "git remote get-url origin")))))


(defun >>=bot/dired+git-status ()
  "Open `dired' and `magit-status' buffers in `user-emacs-directory'.
This assumes that `user-emacs-directory' is a valid cloned `xorns' GIT
repository."
  (interactive)
  (if >>=!pkg-dir
    (let ((default-directory user-emacs-directory))
      (dired default-directory)
      (magit-status-setup-buffer default-directory))
    ;; else
    (warn ">>= only allowed in standalone-mode.")))


(defun >>=bot/dired-working-folder (&optional base)
  "Open a `dired' buffer in `xorns' working-folder Lisp library.
If BASE argument is non-nil, open project directory instead."
  (interactive "P")
  (let ((dir (>>-bot-git-remote-url))
	ok)
    (if dir
      (let ((path (replace-regexp-in-string "^file://" "" dir)))
	(when (file-exists-p path)
	  (setq ok (if base path (expand-file-name "horns" path)))
	  (dired ok))))
    (if (not ok)
      (warn ">>= xorns working-folder not found."))))


(defun >>=bot/git-pull ()
  "Pull `xorns' standalone repository from GIT origin remote.
This assumes that `user-emacs-directory' is a valid cloned `xorns' GIT
repository."
  (interactive)
  (if >>=!pkg-dir
    (let ((default-directory user-emacs-directory))
      (shell-command "git pull --rebase origin"))
    ;; else
    (warn ">>= only allowed in standalone-mode.")))


(defun >>=bot/byte-recompile (&optional force)
  "Recompile `.el' files in `xorns' standalone directory.
If FORCE argument is non-nil, recompile every ‘.el’ file even if it already
has an ‘.elc’ file; otherwise only those that needs recompilation."
  (interactive "P")
  (if >>=!pkg-dir
    (let ((path (file-name-as-directory >>=!pkg-dir)))
      (save-some-buffers nil
	(lambda ()
	  (string-match (regexp-quote path) buffer-file-name)))
      (byte-recompile-directory path 0 force))
    ;; else
    (warn ">>= only allowed in standalone-mode.")))


(define-transient-command >>=bot/menu ()
  "Local menu standalone mode developer tools."
  ["Arguments"
    ("-f" "Force byte recompile all `.el' files"      ("-f" "--force"))
    ("-b" "Base working-folder instead Lisp library"  ("-b" "--base"))]
  [["Production"
     ("p" "Pull from source"    >>=bot/git-pull)
     ("d" "Open dired+magit"    >>=bot/dired+git-status)
     ("c" "Byte recompile"      >>=bot/byte-recompile)]
   ["Development"
     ("w" "Open dired"          >>=bot/dired-working-folder)]]
  (interactive)
  (transient-setup '>>=bot/menu))


(provide 'xorns-bots)
;;; xorns-bots.el ends here
