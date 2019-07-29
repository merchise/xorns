;;; xorns-tools.el --- Common Systems Tools

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This library defines several general utilities than can be used in any
;; context.

;;; Code:

(defmacro >>=on-debug-message (format-string &rest args)
  "Display a message only when `init-file-debug' is active.
Use the same parameters as `message' standard function: FORMAT-STRING and
ARGS."
  `(if init-file-debug
    (message (concat ">>= " ,format-string) ,@args)))


(defsubst >>=intern (string)
  "Return STRING\'s canonical symbol (safe if it is already a symbol)."
  (if (symbolp string) string (intern string)))


(defsubst >>=symbol-name (symbol)
  "Return SYMBOL\'s name, a string (safe if it is already a string)."
  (if (stringp symbol) symbol (symbol-name symbol)))


(defun plist-exclude (plist &rest props)
  "Return a copy of PLIST with all PROPS excluded.
PLIST is a property-list of the form (PROP1 VALUE1 PROP2 VALUE2 ...)."
  (let ((pivot plist) res)
    (while (consp pivot)
      (let ((key (pop pivot))
	    (value (pop pivot)))
	(unless (memq key props)
	  (push value res)
	  (push key res))))
    res))


(defun dir-join (&rest parts)
  "Join PARTS to a single path."
  (mapconcat 'file-name-as-directory parts ""))


(defun find-dir (&rest dirs)
  "Find first existing directory from a DIRS list."
  (let (res)
    (while (and (not res) (consp dirs))
      (let ((dir (pop dirs)))
	(if (and (stringp dir) (file-directory-p dir))
	  (setq res dir))))
    res))



(provide 'xorns-tools)
;;; xorns-tools.el ends here
