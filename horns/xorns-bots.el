;;; xorns-bots.el --- Latest `xorns' development versions tools  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module contains tools to be used when using `xorns' in standalone mode
;; to facilitate staying up-to-date on the latest development versions.

;;; Code:

(eval-and-compile
  (require 'recentf)
  (require 'transient nil 'noerror)
  (require 'magit-status nil 'noerror)
  (require 'xorns-tools))


(defconst >>=!pkg-dir (bound-and-true-p >>=!xorns/standalone-dir)
  "Package directory if `xorns' is initialized in standalone mode.")


(defvar >>=|bots/activation-key "<C-s-backspace>"
  "Key-binding to use with `>>=bots/menu'.")


(defun >>-bots-menu-arguments (prefix)
  "Internal function to get transient or interactive PREFIX argument."
  (list
    (or
      current-prefix-arg
      (member prefix (transient-args '>>=bots/menu)))))


(defun >>-bots-git-remote-url ()
  "Internal function to get GIT remote URL of `xorns' standalone repository."
  (if >>=!pkg-dir
    (let ((default-directory user-emacs-directory))
      (replace-regexp-in-string "^file://" ""
        (>>=shell-command-to-string "git remote get-url origin")))))


(defun >>=bots/dired+git-status ()
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


(defun >>=bots/dired-working-folder (&optional base)
  "Open a `dired' buffer in `xorns' working-folder Lisp library.
If BASE argument is non-nil, open project directory instead."
  (interactive (>>-bots-menu-arguments "base"))
  (let ((path (>>-bots-git-remote-url)))
    (if (file-exists-p path)
      (dired (if base path (expand-file-name "horns" path)))
      ;; else
      (warn ">>= xorns working-folder not found."))))


(defun >>=bots/recent-working-file ()
  "Switch to a buffer visiting recent file in `xorns' working-folder."
  (interactive)
  (let ((path (>>-bots-git-remote-url)))
    (if (file-exists-p path)
      (let ((res (>>=file-in-dir-tree recentf-list path)))
        (if res
          (find-file res)
          ;; else
          (>>=bots/dired-working-folder)))
      ;; else
      (warn ">>= xorns working-folder not found."))))


(defun >>=bots/git-pull ()
  "Pull `xorns' standalone repository from GIT origin remote.
This assumes that `user-emacs-directory' is a valid cloned `xorns' GIT
repository."
  (interactive)
  (if >>=!pkg-dir
    (let ((default-directory user-emacs-directory))
      (shell-command "git pull --rebase origin"))
    ;; else
    (warn ">>= only allowed in standalone-mode.")))


(defun >>=bots/remove-compiled ()
  "Remove `.elc' compiled files in `xorns' standalone directory."
  (interactive)
  (if >>=!pkg-dir
    (mapc
      (lambda (file)
        (when (string-equal (>>=suffix file 4) ".elc")
          (delete-file (>>=path/join >>=!pkg-dir file) 'trash)
          ))
      (directory-files >>=!pkg-dir))
    ;; else
    (warn ">>= only allowed in standalone-mode.")))


(defun >>=bots/byte-recompile (&optional force)
  "Recompile `.el' files in `xorns' standalone directory.
If FORCE argument is non-nil, recompile every ‘.el’ file even if it already
has an ‘.elc’ file; otherwise only those that needs recompilation."
  (interactive (>>-bots-menu-arguments "force"))
  (if >>=!pkg-dir
    (let ((path (file-name-as-directory >>=!pkg-dir)))
      (save-some-buffers nil
        (lambda ()
          (string-match (regexp-quote path) buffer-file-name)))
      (byte-recompile-directory path 0 force))
    ;; else
    (warn ">>= only allowed in standalone-mode.")))


(transient-define-prefix >>=bots/menu ()
  "Local menu standalone mode developer tools."
  ["Arguments"
    ("-f" "Force byte recompile all `.el' files"      ("-f" "force"))
    ("-b" "Base working-folder instead Lisp library"  ("-b" "base"))]
  [["Production"
     ("p" "Pull from source"    >>=bots/git-pull)
     ("d" "Open dired+magit"    >>=bots/dired+git-status)
     ("l" "Clean `.elc' files"  >>=bots/remove-compiled)
     ("c" "Byte recompile"      >>=bots/byte-recompile)]
   ["Development"
     ("w" "Open dired"          >>=bots/dired-working-folder)
     ("r" "Open recent-file"    >>=bots/recent-working-file)]]
  (interactive)
  (transient-setup '>>=bots/menu))


(if >>=|bots/activation-key
  (>>=global-set-keys (kbd >>=|bots/activation-key) '>>=bots/menu))


(provide 'xorns-bots)
;;; xorns-bots.el ends here
