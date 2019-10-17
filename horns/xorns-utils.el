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


(defgroup xorns nil
  "Merchise extensions for Emacs."
  :prefix "xorns-"
  :group 'extensions
  :group 'convenience)



;;; Symbols and variables

(defun xorns-get-original-value (symbol)
  "Return SYMBOL's original value or nil if that is void."
  (if (boundp symbol)
    (eval (car (get symbol 'standard-value)))))



;;; Strings

(defun xorns-str-trim (s)
  "Remove white-spaces at start and end of the string S."
  (let ((blanks split-string-default-separators))
    (replace-regexp-in-string
      (format "\\`%s\\|%s\\'" blanks blanks) "" s)))



;;; Files

(defconst xorns-home-dir
  (purecopy (file-name-as-directory "~"))
  "Home directory.")


(defun xorns-preferred-directory (&rest dirs)
  "Return name of preferred directory (the first that exists in DIRS.

If no item is given in DIRS, return $HOME."
  (file-name-as-directory
    (if dirs
      (cl-some
        (lambda (item)
          (if (and item (file-directory-p item)) item))
        dirs)
      ;; else
      "~")))


(defun xorns-preferred-default-directory ()
  "Return name of preferred default directory when start a new session."
  (xorns-preferred-directory
    (getenv "WORKSPACE")
    (dir-join "~" "work" "src")
    (dir-join "~" "work")
    (dir-join "~" "src" "merchise")
    (dir-join "~" "src")
    "~"))


(defun >>=set-default-directory ()
  "Set the default directory to its original value."
  (if (equal (>>=default-directory) xorns-home-dir)
    (>>=set-value default-directory (xorns-preferred-default-directory))))


(defun xorns-executable-find (command &rest other-commands)
  "Search for COMMAND in `exec-path' and return the absolute file name.

If COMMAND is not found, looks for alternatives given in OTHER-COMMANDS.

This function is safe avoiding nil commands.  If none is found, nil
is returned."
  (cl-some
    #'(lambda (cmd) (if cmd (executable-find cmd)))
    (cons command other-commands)))


(defun >>=default-directory ()
  "Name of default directory of current buffer.
The result always ends with slash and it is in abbreviated format.  To
interactively change the default directory, use command `cd'."
  (file-name-as-directory (abbreviate-file-name default-directory)))


;; TODO: This code must be removed when every body uses Emacs >= 24.3
(unless (functionp 'file-name-base)
  (defun file-name-base (&optional filename)
    "Return the base name of the FILENAME: no directory, no extension.
FILENAME defaults to `buffer-file-name'."
    (file-name-sans-extension
      (file-name-nondirectory (or filename (buffer-file-name))))))


;; The LOCAL arg to `add-hook' is interpreted differently in Emacs and
;; XEmacs.  In Emacs we don't need to call `make-local-hook' first.
;; It's harmless, though, so the main purpose of this alias is to shut
;; up the byte compiler.
;; This is for "apt-utils" to work.
(unless (functionp 'make-local-hook)
  (defun make-local-hook (hook)
    "Make the hook HOOK local to the current buffer.
The return value is HOOK.

This function is obsolete since 21.1; not necessary any more.

You never need to call this function now that `add-hook' does it for you
if its LOCAL argument is non-nil.

Do not use `make-local-variable' to make a hook variable buffer-local."
    (add-hook hook 'ignore nil 'local)
    hook))



;;; Configuration levels

(defun xorns-get-config-level (arg &optional strict)
  "Transform argument ARG in a valid configuration level.

Value semantics for ARG when STRICT is true are::

- `0', nil or `\'minimum': execute configurations defined as basic or
  implicit.

- `1` or `\'basic': execute minimum extra configurations (including lower
  levels).  These include project management and text modes.

- `1' or `\'general': execute general configurations (including lower
  levels).  These include programming modes.

- `2' or `\'maximum': execute all specific configurations (including lower
  levels).  These include email configuration, and other esoteric stuff.

If STRICT is nil::

- not configured or nil: don't execute specific configurations.

- any other value is synonym of `'maximum'."
  (let ((res
          (let ((options
                  '((experimental . 110) (110 . 110)
                     (maximum . 100) (100 . 100)
                     (general . 70) (70 . 70)
                     (basic . 10) (10 . 10)
                     (minimum . 0) (nil . 0) (0 . 0)))
                 (default '(t . t)))
            (cdr (or (assq arg options) default)))))
    (if strict
      (if (not (or (null res) (eq res t)))
        res
        ;; else
        (error "Invalid argument `%s' in strict mode!" arg))
      ;; else
      (if (eq res t) 2 res))))


(defun xorns-read (prompt &optional default-value keymap read hist)
  "Read a string from the minibuffer, prompting with string PROMPT.

If second optional arg DEFAULT-VALUE is given, should be a string to return
  this value for empty input.

Third arg is a KEYMAP to use whilst reading.

If fourth arg READ is given, interpret the result as a Lisp object and return
  that object.

Fifth arg HIST , specifies a history list and optionally the initial position
  in the list.

See `read-from-minibuffer' for more information on all arguments."
  (let* ((prompt*
           (concat
             (or prompt ">>>")
             (if default-value (format " (%s)" default-value))
             (if (or prompt default-value) ":") " "))
          (res
            (read-from-minibuffer
              prompt* nil keymap read hist default-value)))
    (or (unless (equal res "") res) default-value "")))



;;; Features

(defun xorns-missing-feature (feature)
  "Report a message about a missing recommended FEATURE."
  (message "Recommended feature `%s' is not installed." feature))


(provide 'xorns-utils)
;;; xorns-utils.el ends here
