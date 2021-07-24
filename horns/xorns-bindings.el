;;; xorns-bindings.el --- Key binding simple tools

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tools to manage key-bindings.  It will be taken into account if `exwm' is
;; active when assigning each command.

;; Enjoy!


;;; Code:

(require 'xorns-init)
(require 'xorns-tools)


(defvar >>-global-set-key '>>-global-set-key
  "Function to give a KEY a global binding as COMMAND.
This variable is intended to redefine how to set a global key sequence in some
specialized packages, like `exwm'.  The default value is the internal function
`>>-global-set-key'.")


(defun >>-global-set-key (key command)
  "Give KEY a global binding as COMMAND.
This is an internal function, it is intended to be replaced in specialized
packages like `exwm'."
  (define-key (current-global-map) key command))


(defsubst >>-kbd (key)
  "Convert KEY to its internal representation but first trying a string."
  (if (vectorp key) key (kbd key)))


(defsubst >>-command (command)
  "Check a COMMAND always recognizing the symbols as valid."
  (if (symbolp command) command (>>=cast-function command 'validate)))


(defsubst >>-key-pair (key command)
  "Validate a KEY/COMMAND and convert it to a `cons' tuple."
  (cons (>>-kbd key) (>>-command command)))


(defun >>=global-set-keys (&rest pairs)
  "Bind on the current global keymap [KEY COMMAND] PAIRS."
  (>>=plist-do (key command pairs)
    (funcall >>-global-set-key (>>-kbd key) (>>-command command))))


(defmacro >>=remap (key command alt-key)
  "Give KEY a global binding as COMMAND.
In this case, KEY is a standard `key-binding', whose original command will
receive the alternative ALT-KEY binding.  See also `>>=remap*'"
  `(let ((kbind (>>-kbd ,key)))
     (>>=global-set-keys
       kbind ',command
       ,alt-key (key-binding kbind))))


(defmacro >>=remap* (key command alt-key)
  "Give KEY a global binding as COMMAND.
In this case KEY is NOT a `key-binding' but a base command, which will receive
the alternative ALT-KEY binding.  See also `>>=remap'"
  `(progn
     (>>=global-set-keys
       [remap ,key] ',command
       ,alt-key ',key)))


(provide 'xorns-bindings)
;;; xorns-bindings.el ends here
