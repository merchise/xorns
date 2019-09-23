;;; xorns-tools.el --- Common Systems Tools

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This library defines several basic and general utilities that can be used
;; in any context.

;;; Code:


;; General

(defmacro ->? (func &rest args)
  "Call FUNC with our remaining ARGS, only if it is bound."
  `(when (fboundp ',func)
     (if init-file-debug
       (message ">>= calling: %s"
	 (or (documentation ',func) ,(symbol-name func))))
     (condition-case-unless-debug err
       (,func ,@args)
       (error
	 (message "Xorns Error in '%s': %s\n"
	   ',(symbol-name func) (error-message-string err))))))


(defmacro >>=on-debug-message (format-string &rest args)
  "Display a message only when `init-file-debug' is active.
Use the same parameters as `message' standard function: FORMAT-STRING and
ARGS."
  `(if init-file-debug
    (message (concat ">>= " ,format-string) ,@args)))


(defmacro >>=set-value (symbol value)
  "Initialize a SYMBOL (variable name) with an expression (VALUE)."
  `(progn
     (unless
       (or
	 (get ',symbol 'standard-value)
	 (memq (get ',symbol 'custom-autoload) '(nil noset)))
       (custom-load-symbol ',symbol))
     ;; set the variable.
     (set ',symbol ,value)))



;; String - symbol conversion

(defsubst >>=intern (string)
  "Return STRING\'s canonical symbol (safe if it is already a symbol)."
  (if (symbolp string) string (intern string)))


(defsubst >>=symbol-name (symbol)
  "Return SYMBOL\'s name, a string (safe if it is already a string)."
  (if (stringp symbol) symbol (symbol-name symbol)))



;; property list extension

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



;; Files and directories

(defun dir-join (&rest parts)
  "Join PARTS to a single path."
  (mapconcat 'file-name-as-directory parts ""))


(defun file-expand (file-name &rest dir-parts)
  "Convert FILE-NAME to absolute using `expand-file-name' function.
The rest of the arguments from the second (DIR-PARTS) are considered parts to
build the default directory using `dir-join' function."
(expand-file-name file-name (apply 'dir-join dir-parts)))


(defun find-dir (&rest dirs)
  "Find first existing directory from a DIRS list."
  (let (res)
    (while (and (not res) (consp dirs))
      (let ((dir (pop dirs)))
	(if (and (stringp dir) (file-directory-p dir))
	  (setq res dir))))
    res))


(defun >>=locate-user-emacs-file (&rest names)
  "Return first found in NAMES absolute per-user Emacs-specific file-name.
This function uses `locate-user-emacs-file' for each name until a proper value
is found.  Each given name is processed with `substitute-in-file-name' to
substitute used environment variables.  If no item is given, the name of
standard Emacs initialization file is returned."
  (let (res)
    (while (and (null res) names)
      (let ((item (locate-user-emacs-file
		    (substitute-in-file-name (car names)))))
	(if (file-exists-p item)
	  (setq res item)
	  ;; else
	  (setq names (cdr names)))))
    (or res (locate-user-emacs-file "init.el" ".emacs"))))


(defun >>=default-directory ()
  "Name of default directory of current buffer.
The result will be abbreviated and end with the directory separator slash."
  (file-name-as-directory (abbreviate-file-name default-directory)))



;; Buffers

(defun >>=current-buffer-remote? ()
  "Return non-nil if current buffer is remote."
  (require 'files)
  (let ((tests
	  (list
	    (buffer-file-name)
	    list-buffers-directory
	    default-directory))
	res)
    (while (and tests (not res))
      (let ((aux (car tests)))
	(if (and (stringp aux) (file-remote-p aux))
	  (setq res aux)
	  ; else: next item
	  (setq tests (cdr tests)))))
    res))


(provide 'xorns-tools)
;;; xorns-tools.el ends here
