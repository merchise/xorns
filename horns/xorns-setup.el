;;; xorns-setup.el --- Basic tools to setup components  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module define tools to configure the components, or building blocks,
;; required in a `xorns' setup.

;;; Code:

(require 'use-package)
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


(defun >>=setup/command-check (command)
  "Check if a system COMMAND is installed.
Intended to find out if a feature that depends on the given command can be
configured."
  ;; See `use-package-ensure-system-package' fo a more elaborated solution.
  (or
    (executable-find command)
    (progn
      (message ">>= warning: '%s' command is not installed" command)
      nil)))



;;; `customization' functions

(defsubst >>=customized? (symbol)
  "Return t if SYMBOL’s value is already customized.
This has the same protocol as the `boundp' function."
  (or
    (plist-member (symbol-plist symbol) 'customized-value)
    (plist-member (symbol-plist symbol) 'saved-value)))


(defmacro >>:custom (&rest args)
  "Conditionally set a collection of custom variables.

ARGS is a sequence of (SYMBOL VALUE [COMMENT])... definitions.  It uses the
same protocol as the `:custom' keyword in the `use-package' configuration
macro, but checking that each SYMBOL is only configured if it is the first
time.

Customized values using the `:custom' keyword of `use-package' are not saved
in the Emacs `custom-file'.  Use this macro within a `:config' block if you
need to be compatible with the standard tools related to `customize-option'.

\(fn (SYMBOL VALUE [COMMENT]) ...)"
  (let (exps)
    (dolist (arg args `(progn . ,(nreverse exps)))
      (push
        (let ((symbol (nth 0 arg))
              (value `(,(nth 1 arg)))
              (comment (nth 2 arg)))
          `(unless (>>=customized? ',symbol)
             (customize-set-variable ',symbol ,(car value) ,comment)))
        exps))))


(defmacro >>:custom-set (&rest args)
  "Conditionally set a collection of custom variables.

This macro is similar to `>>:custom', but using the same syntax as the `setq'
function.  ARGS is a sequence of pairs [SYMBOL VALUE]...

\(fn [SYMBOL VALUE] ...)"
  (declare (debug setq))
  (let (exps)
    (while args
      (push `(,(pop args) ,(pop args)) exps))
    `(>>:custom ,@(nreverse exps))))



;;; `use-package' extensions


(defalias 'use-package-normalize/:custom? 'use-package-normalize/:custom
  "Normalize use-package ARGS for `:custom?' KEYWORD.")


(defun use-package-handler/:custom? (name _keyword args rest state)
  "Generate use-package `:custom?' keyword code.
Arguments used: NAME, ARGS, REST, and STATE."
  (use-package-concat
    `(,(macroexpand `(>>:custom ,@args)))
    (use-package-process-keywords name rest state)))


(setq use-package-keywords
  ;; `:custom?' may cause problems if added after `:config'
  (cl-loop for item in use-package-keywords
    if (eq item :config)
      collect :custom? and collect :config
    else
      unless (eq item :custom?)    ; avoid duplicates
        collect item)
  )


(provide 'xorns-setup)
;;; xorns-setup.el ends here
