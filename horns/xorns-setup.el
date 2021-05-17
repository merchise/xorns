;;; xorns-setup.el --- Basic tools to setup components  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module define tools to configure the components, or building blocks,
;; required in a `xorns' setup.

;;; Code:

(require 'xorns-tools)



;;; setup levels

(defvar >>=|setup/level nil
  "Desired setup level for `xorns' components.
There are three possible values defined in `>>=!setup/levels'.")


(defconst >>=!setup/levels '((basic . 0) (medium . 1) (high . 2))
  "Possible setup levels with its associated integer values.")


(defsubst >>-setup/int-level (level)
  "Convert a setup LEVEL into an integer value."
  (cond
    ((integerp level)
      level)
    ((null level)
      0)
    (t
      (or
        (if (null level) 0)
        (cdr (assq level >>=!setup/levels))
        (error ">>= invalid setup level '%s'" level)))))


(defsubst >>=setup/level-check (&optional level)
  "Check if LEVEL is OK to setup a feature."
  (>=
    (>>-setup/int-level >>=|setup/level)
    (>>-setup/int-level level)))


(defun >>=setup/command-check (command &optional level)
  "Check if LEVEL is OK to setup a feature that depends on a system COMMAND.
If COMMAND is nil, just check the LEVEL."
  (and
    (>>=setup/level-check level)
    (let ((res (executable-find command)))
      (if res
        res
        ;; else
        (warn ">>= '%s' command is not installed" command)
        nil))))


(defmacro >>=define-setup-checker (command)
  "Define a `>>=setup/command-check' for a given COMMAND.
The checker name will be '>>=setup/<command>-check'"
  (declare (indent 1) (debug t))
  `(defun ,(intern (format ">>=setup/%s-check" command)) (&optional level)
     ,(format ">>=setup/command-check for '%s'" command)
     (>>=setup/command-check ,command level)))


(provide 'xorns-setup)
;;; xorns-setup.el ends here
