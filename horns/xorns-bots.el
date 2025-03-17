;;; xorns-bots.el --- Development tools -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module contains tools to be used when using `xorns' in standalone mode
;; to facilitate staying up-to-date on the latest development versions.

;; TODO: migrate `transient' prefix to use a keymap (see
;;       `projectile-command-map').  That will be more flexible using
;;       `which-key'.

;;; Code:

(eval-and-compile
  (require 'recentf)
  (require 'transient)
  (require 'magit-status)
  (require 'xorns-tools)
  (require 'xorns-window))


(defconst >>-!lib-dir (bound-and-true-p >>=!xorns/lib-dir)
  "Package directory.")


(defvar >>=|bots/activation-key "<C-s-backspace>"
  "Key-binding to use with `>>=bots/menu'.")


(defun >>-bots/menu-arguments (prefix)
  "Internal function to get transient or interactive PREFIX argument."
  (list
    (or
      current-prefix-arg
      (member prefix (transient-args '>>=bots/menu)))))


(defun >>-bots/local-working-folder ()
  "Get the local working `xorns' repository."
  (let* ((default-directory user-emacs-directory)
         (res
           (file-name-as-directory
             (replace-regexp-in-string "^file://" ""
               (>>=shell-command-to-string "git remote get-url origin")))))
    (when (file-directory-p res)
      res)))


(defun >>-bots/get-current-release ()
  "Get the current version of the repository in the local working folder."
  (if-let ((path (>>-bots/local-working-folder)))
    (let ((default-directory path)
          (command "git describe --tags --abbrev=0 2> /dev/null"))
      (>>=shell-command-to-string command))
    ;; else
    (>>=warn "working-folder not found")))


(defun >>-bots/set-package-version (version)
  "Set package VERSION to a new release."
  (if-let ((wf (>>-bots/local-working-folder)))
    (let ((file-name (expand-file-name "xorns.el" (>>=path/join wf "horns"))))
      (with-temp-file file-name
        (insert-file-contents file-name)
        (re-search-forward "^;; Version: ")
        (delete-region (point) (line-end-position))
        (insert version)))
    ;; else
    (>>=warn "working-folder not found")))


(defun >>-bots/get-next-release-options (&optional current)
  "Get some options for the next release based on the CURRENT one."
  (unless current
    (setq current (>>-bots/get-current-release)))
  (let ((parts (split-string current "[.]")))
    (list
      (format "%s.%s.%s"
        (nth 0 parts) (nth 1 parts) (1+ (string-to-number (nth 2 parts))))
      (format "%s.%s.%s"
        (nth 0 parts) (1+ (string-to-number (nth 1 parts))) 0)
      (format "%s.%s.%s"
        (1+ (string-to-number (nth 0 parts))) 0 1))))


(defun >>=bots/dired+git-status ()
  "Open `dired' and `magit-status' buffers in `user-emacs-directory'.
This assumes that `user-emacs-directory' is a valid cloned `xorns' GIT
repository."
  (interactive)
  (let ((default-directory user-emacs-directory))
    (dired default-directory)
    (ignore-errors
      (magit-status-setup-buffer default-directory))))


(defun >>=bots/dired-working-folder (&optional base)
  "Open a `dired' buffer in `xorns' working-folder Lisp library.
If BASE argument is non-nil, open project directory instead."
  (interactive (>>-bots/menu-arguments "base"))
  (if-let ((path (>>-bots/local-working-folder)))
    (dired (if base path (expand-file-name "horns" path)))
    ;; else
    (>>=warn "working-folder not found")))


(defun >>=bots/recent-working-file ()
  "Switch to a buffer visiting recent file in `xorns' working-folder."
  (interactive)
  (if-let ((path (>>-bots/local-working-folder)))
    (let ((res (>>=file-in-dir-tree recentf-list path (buffer-file-name))))
      (if res
        (let* ((buf (find-file-noselect res))
               (win (get-buffer-window buf)))
          (if win
            (select-window win)
            ;; else
            (switch-to-buffer buf)))
        ;; else
        (>>=bots/dired-working-folder)))
    ;; else
    (>>=warn "working-folder not found")))


(defun >>=bots/make-release (version)
  "Set new release to VERSION in local folder repository."
  (interactive
    (list (completing-read ">>= " (>>-bots/get-next-release-options))))
  (let ((current (>>-bots/get-current-release)))
    (if (version< current version)
      (let ((tag-command "git tag %s")
            (commit-command "git commit -am \"Set new release to %s\""))
        (message "Setting new release '%s'" version)
        (>>-bots/set-package-version version)
        (shell-command (format tag-command version))
        (shell-command (format commit-command version)))
      ;; else
      (>>=warn
        "new version '%s' should be greater than current release '%s'"
        version current))))


(defun >>=bots/open-sketch ()
  "Switch to my sketch buffer."
  (interactive)
  (pop-to-buffer-same-window
    (find-file-noselect
      (>>=path/join
        >>=|preferred-default-directory
        "sketch" "emacs" "xorns-sketch.el"))))


(defun >>=bots/git-pull ()
  "Pull `xorns' standalone repository from GIT origin remote.
This assumes that `user-emacs-directory' is a valid cloned `xorns' GIT
repository."
  (interactive)
  (if (>>-bots/local-working-folder)
    (let ((default-directory user-emacs-directory))
      (shell-command "git pull --rebase origin"))
    ;; else
    (>>=warn "only allowed when there is a local working folder")))


(defun >>=bots/remove-compiled ()
  "Remove `.elc' compiled files in `xorns' standalone directory."
  (interactive)
  (when >>-!lib-dir
    (dolist (file (directory-files >>-!lib-dir 'full "\\.elc\\'" 'nosort))
      (when (file-regular-p file)
        (delete-file file)))))


(defun >>=bots/byte-recompile (&optional force)
  "Recompile `.el' files in `xorns' standalone directory.
If FORCE argument is non-nil, recompile every ‘.el’ file even if it already
has an ‘.elc’ file; otherwise only those that needs recompilation."
  (interactive (>>-bots/menu-arguments "force"))
  (let ((path (file-name-as-directory >>-!lib-dir)))
    (save-some-buffers nil
      (lambda ()
        (string-match-p (regexp-quote path) buffer-file-name)))
    (byte-recompile-directory path 0 force)))


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
     ("r" "Open recent-file"    >>=bots/recent-working-file)
     ("k" "Open sketch"         >>=bots/open-sketch)
     ("s" "Switch to scratch"   >>=toolbox/scratch-buffer)
     ("v" "Make new release"    >>=bots/make-release)
     ]]
  (interactive)
  (transient-setup '>>=bots/menu))


(if >>=|bots/activation-key
  (>>=bind-global-key >>=|bots/activation-key '>>=bots/menu))


(provide 'xorns-bots)
;;; xorns-bots.el ends here
