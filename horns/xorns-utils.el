;;; xorns-utils.el --- Common manipulation utility functions

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;;; Commentary:

;; Extensions functions that can be used in a plain Emacs (with no extensions
;; installed).  All mode specific functions must be placed in the specific
;; `xorns' sub-module.

;; Enjoy!


;;; Code:

(require 'xorns-tools)



;;; Symbols and variables

(defun >>=get-original-value (symbol)
  "Return SYMBOL's original value or nil if that is void."
  (if (boundp symbol)
    (eval (car (get symbol 'standard-value)))))




;;; Files


(defconst >>=|home-dir
  (purecopy (file-name-as-directory (or (getenv "HOME") "~")))
  "Home directory.")


(defvar >>=|preferred-default-directory
  (cl-some
    (lambda (item) (if (and item (file-directory-p item)) item))
    (list
      (getenv "WORKSPACE")
      (>>=dir-join >>=|home-dir "work" "src")
      (>>=dir-join >>=|home-dir "work")
      (>>=dir-join >>=|home-dir "src" "merchise")
      (>>=dir-join >>=|home-dir "src")
      >>=|home-dir))
  "Preferred default directory when start a new session.")


(defun >>=set-default-directory ()
  "Set the default directory to its original value."
  (if (equal (>>=default-directory) xorns-home-dir)
    (>>=set-value default-directory >>=|preferred-default-directory)))


(defun >>=executable-find (command &rest other-commands)
  "Search for COMMAND in `exec-path' and return the absolute file name.

If COMMAND is not found, looks for alternatives given in OTHER-COMMANDS.

This function is safe avoiding nil commands.  If none is found, nil
is returned."
  (cl-some
    (lambda (cmd) (if cmd (executable-find cmd)))
    (cons command other-commands)))


(defun >>=default-directory ()
  "Name of default directory of current buffer.
The result always ends with slash and it is in abbreviated format.  To
interactively change the default directory, use command `cd'."
  (file-name-as-directory (abbreviate-file-name default-directory)))


(provide 'xorns-utils)
;;; xorns-utils.el ends here
