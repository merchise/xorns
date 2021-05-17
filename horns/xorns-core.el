;;; xorns-core.el --- Core lisp functions for xorns  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Core lisp-extension library.

;; Enjoy!


;;; Code:

(require 'xorns-tools)



;;; Preface

(when (version< emacs-version "27")
  ;; Disable useless GUI: menu, toolbar, scroll-bars, and tool-tips.
  ;; As of Emacs 27, this is done in the `early-init.el' file.
  (unless (eq system-type 'darwin)    ; No disable menu in MacOs
    (menu-bar-mode -1))
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))



;;; Multilingual Environment

(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)



;;; Lisp configuration files

(defun >>=read-lisp-config (file)
  "Read a Lisp configuration FILE."
  (if (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let (res)
        (ignore-errors
          (while (not (eobp))
            (setq res (cons (read (current-buffer)) res))))
        (nreverse res)))))


(defun >>=write-lisp-config (file form)
  "Write a Lisp configuration FORM to a FILE."
  (with-temp-buffer
    (mapc (lambda (line) (insert (format "%S\n" line))) form)
    (write-file file)))



;;; Buffers

(defun >>=scratch/get-buffer-create ()
  "Copied from `startup--get-buffer-create-scratch'."
  (or (get-buffer "*scratch*")
    (with-current-buffer (get-buffer-create "*scratch*")
      (set-buffer-major-mode (current-buffer))
      (current-buffer))))


(defun >>=scratch/force (&optional arg)
  "Switch to `*scratch*` buffer, creating a new one if needed.
An optional argument ARG could be given to delete other windows; if
`0' also reset `default-directory' to `xorns' default."
  (interactive "P")
  (switch-to-buffer-other-window (>>=scratch/get-buffer-create))
  (if (= (prefix-numeric-value arg) 0)
    (>>=set-default-directory))
  (if arg (delete-other-windows)))


(defmacro >>=get-buffer-value (buffer variable)
  "Return the value of symbol VARIABLE in BUFFER if it is bound, else nil."
  `(and (boundp ',variable) (buffer-local-value ',variable ,buffer)))


(defun >>=get-buffer-keyword (buffer keyword)
  "Return the value of KEYWORD in BUFFER.

Possible keywords are:

:name
    The value resulting of function `buffer-name'.

:mode
    The value of `major-mode' converted to string using `symbol-name'.

:default-directory
    The value of `default-directory'.

:process
    The live process associated with the buffer.

:class
    If running under Emacs X Window Manager (`exwm'), the value of variable
    `exwm-class-name'.

:instance
    If running under Emacs X Window Manager (`exwm'), the value of variable
    `exwm-instance-name'.

:title
    If running under Emacs X Window Manager (`exwm'), the value of variable
    `exwm-title'."
  (pcase keyword
    (:name (buffer-name buffer))
    (:mode (buffer-local-value 'major-mode buffer))
    (:file-name (buffer-local-value 'buffer-file-name buffer))
    (:default-directory (buffer-local-value 'default-directory buffer))
    (:process (get-buffer-process buffer))
    (:class (>>=get-buffer-value buffer exwm-class-name))
    (:instance (>>=get-buffer-value buffer exwm-instance-name))
    (:title (>>=get-buffer-value buffer exwm-title))
    (_ (error ">>= unknown keyword: %s" keyword))))


(defun >>-buffer-value-to-string (value)
  "Convert a buffer VALUE to string."
  (cond
    ((or (null value) (stringp value))
      value)
    ((symbolp value)
      (symbol-name value))
    ((processp value)
      (process-name value))
    (t
      (format "%s" value))))


(defun >>-buffer-value-match (exp value)
  "Return if a buffer keyword VALUE match a ruler expression EXP."
  (and exp value
    (string-match-p
      (>>-buffer-value-to-string exp)
      (>>-buffer-value-to-string value))))


(defun >>=buffer-match-p (ruler buffer)
  "Return non-nil if BUFFER is matched with RULER."
  (if ruler
    (let ((res buffer))
      (while (and res ruler)
        (let ((exp (nth 1 ruler))
              (value (>>=get-buffer-keyword buffer (car ruler))))
          (unless (>>-buffer-value-match exp value)
            (setq res nil))
          (setq ruler (cdr (cdr ruler)))))
      res)))


(defun >>=find-buffers (&rest ruler)
  "Find all buffers matching RULER.
RULER is a property-list, all keywords supported by `>>=get-buffer-keyword'
can be specified, each value must be a string, the comparison logic will an
`and' of all given keywords, each value could be a simple string or a regular
expression."
  (let ((aux (>>=fix-rest-list ruler)))
    (delq nil
      (mapcar
        (lambda (buffer) (>>=buffer-match-p aux buffer))
        (buffer-list)))))


(defun >>=find-buffer (&rest ruler)
  "Find best buffer matching RULER.
See `>>=find-buffers' for more information."
  (let* ((result (apply '>>=find-buffers ruler))
         (one (car result))
         (two (nth 1 result)))
    ;; if two or more buffers are found, do not select current.
    (if (and two (eq one (current-buffer)))
      two
      ;; else
      one)))


(provide 'xorns-core)
;;; xorns-core.el ends here
