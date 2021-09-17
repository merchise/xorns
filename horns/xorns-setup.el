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
An integer value between 0 and 10.  There are three symbols defining standard
levels in `>>=!setup/levels'.")


(defconst >>=!setup/levels '((basic . 0) (medium . 5) (high . 10))
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
        (cdr (assq level >>=!setup/levels))
        (error ">>= invalid setup level '%s'" level)))))


(defsubst >>=setup/level-check (&optional level)
  "Check if a given setup LEVEL is OK to configure a feature.
This is done by comparing the given LEVEL against defined `>>=|setup/level'."
  (>=
    (>>-setup/int-level >>=|setup/level)
    (>>-setup/int-level level)))


(defun >>=setup/command-check (command &optional level)
  "Check a COMMAND to find out if a feature can be configured.
The COMMAND argument must be a string to be checked against `executable-find',
In this case, a warning will be issued if the COMMAND is not found.  A boolean
value can also be used, in which case it is returned without further checking.
If a setup LEVEL is given, it is verified with the `>>=setup/level-check'
function."
  (and
    (>>=setup/level-check level)
    (if (booleanp command)
      command
      ;; else
      (let ((res (executable-find command)))
        (when (null res)
          (message ">>= warning: '%s' command is not installed" command))
        res))))


(provide 'xorns-setup)
;;; xorns-setup.el ends here
