;;; xorns-tools.el --- Common Systems Tools  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This library defines several basic and general utilities that can be used
;; in any context.

;; Enjoy!


;;; Code:

(require 'subr-x)    ; for `string-trim'


;;; general

(defmacro ->? (func &rest args)
  "Call FUNC with our remaining ARGS, only if it is bound."
  `(when (fboundp ',func)
     (if init-file-debug
       (message ">>= calling: %s"
	 (or (documentation ',func) ,(symbol-name func))))
     (condition-case-unless-debug err
       (,func ,@args)
       (error
	 (message ">>= error in '%s': %s\n"
	   ',(symbol-name func) (error-message-string err))))))


(defmacro >>=on-debug-message (format-string &rest args)
  "Display a message only when `init-file-debug' is active.
Use the same parameters as `message' standard function: FORMAT-STRING and
ARGS."
  `(if init-file-debug
     (message (concat ">>= " ,format-string) ,@args)))


(defmacro >>=progn (header &rest body)
  "Safe evaluate BODY forms sequentially and return value of last one.
Use a HEADER message when `init-file-debug' is t, or in case of error, to
report the identity of the enclosed body."
  `(condition-case-unless-debug err
     (progn
       (>>=on-debug-message ,header)
       ,@body)
     (error (message (concat ">>= error on (" ,header "): %s") err))))


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


(defmacro >>=get-original-value (symbol)
  "Return SYMBOL's original value or nil if that is void."
  `(if (boundp ',symbol)
     (eval (car (get ',symbol 'standard-value)))))


;; TODO: Check `make-obsolete', `define-obsolete-function-alias', ...
(defun >>=deprecate (name &rest options)
  "Issue a warning deprecating NAME.
Several keyword OPTIONS are supported to complement the base message:
':current' module name, ':new' feature name to use instead, a ':new-place'
where an equivalent behaviour is configured, a ':release' version number where
old feature will be not longer available.  All invalid options are ignored."
  (warn
    (concat
      (format ">>= '%s' is now DEPRECATED" name)
      (apply '>>=mapconcat-alist
	'((:current . " in '%s' module")
	  (:new . ", use new '%s' feature instead")
	  (:new-place . ", it is configured now in '%s' module")
	  (:release . ", it will not be longer available after '%s' release"))
	"" options))))



;;; string - symbol conversion

(defsubst >>=intern (string)
  "Return STRING\'s canonical symbol (safe if it is already a symbol)."
  (if (symbolp string) string (intern string)))


(defmacro >>=intern* (string &rest args)
  "Return canonical symbol by formating a STRING with ARGS."
  `(intern (format ,string ,@args)))


(defsubst >>=symbol-name (symbol)
  "Return SYMBOL\'s name, a string (safe if it is already a string)."
  (if (stringp symbol) symbol (symbol-name symbol)))


(defun >>=safe-replace (regexp rep source)
  "Replace all occurrences for REGEXP with REP in SOURCE.
Source could be either a string or a symbol, return a new value of the same
type containing the replacements.  See `replace-regexp-in-string' function."
  (let* ((is-symbol (symbolp source))
	 (value (if is-symbol (symbol-name source) source))
	 (res (replace-regexp-in-string regexp rep value)))
    (if is-symbol (intern res) res)))



;;; lists, property lists extensions

(defun >>=mapconcat-alist (alist separator &rest options)
  "Conditionally `mapconcat' an association-list.
The SEPARATOR is pasted in between each pair of results.  ALIST is formed by
cons pairs (:KEY . FORMAT-STRING); each :KEY is searched in the property-list
given in the argument OPTIONS, when a corresponding value is found, it is used
with FORMAT-STRING to obtain a sequence result.  See `>>=deprecate' function
implementation for an example."
  (mapconcat 'identity
    (delq nil
      (mapcar
	(lambda (pair)
	  (let ((value (plist-get options (car pair))))
	    (if value (format (cdr pair) value))))
	alist))
    separator))


(defun >>=plist-exclude (plist &rest props)
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


(defmacro >>=append (target &rest sequences)
  "Set TARGET to the result value from appending it with all the SEQUENCES."
  `(setq ,target (append ,target ,@sequences)))



;;; files and directories

(defun >>=dir-join (&rest parts)
  "Join PARTS to a single path."
  (mapconcat 'file-name-as-directory parts ""))


(defun >>=find-dir (&rest dirs)
  "Find the first existing directory in DIRS sequence."
  (let (res)
    (while (and (not res) (consp dirs))
      (let ((dir (pop dirs)))
	(if (and (stringp dir) (file-directory-p dir))
	  (setq res dir))))
    res))


(defun >>=ensure-dir (&rest options)
  "Select first valid directory among several OPTIONS ensuring it exists."
  (let ((res (apply '>>=find-dir options)))
    (unless res
      (while (and options (null (car options)))
	(setq options (cdr options)))
      (setq res (car options))
      (when res
	(message ">>= creating directory '%s'." res)
	(make-directory res 'parents)))
    res))


(defun >>=canonical-directory-name (name)
  "Convert directory NAME to absolute canonical form."
  (if name
    (expand-file-name (file-name-as-directory name))))


(defmacro >>=dir-set (symbol &rest options)
  "Set variable SYMBOL to the first directory can be ensured.
The value is selected among several OPTIONS including current variable value."
  `(let ((res (>>=ensure-dir ,@options ,symbol)))
     (if (and res (not (string-equal res ,symbol)))
       (setq-default ,symbol res))))


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


(defun >>=file-in-dir-tree (files base)
  "Return the first item in FILES that is part of the BASE directory tree."
  ;; Based on `dired-in-this-tree'
  (let ((base (concat "^" (regexp-quote (expand-file-name base))))
	(files (if (stringp files) (list files) files))
	case-fold-search)
    (seq-find
      (lambda (item) (string-match-p base (expand-file-name item)))
      files)))


(defun >>=executable-find (command &rest extra)
  "Return the first COMMAND that is part of the BASE directory tree.

If the main command is not found, a EXTRA set of alternatives will be used
until a valid one is found.

This function is safe avoiding nil commands.  If none is found, nil
is returned."
  (cl-some
    (lambda (cmd) (if cmd (executable-find cmd)))
    (cons command extra)))


(defun >>=file-string (file)
  "Return the trimmed contents of the given FILE as a string."
  (if (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (string-trim (buffer-string)))))



;;; workspace management

(defconst >>=!home-dir
  (purecopy (>>=canonical-directory-name (or (getenv "HOME") "~")))
  "Home directory.")


(defvar >>=|preferred-default-directory
  (>>=find-dir
    (>>=canonical-directory-name (getenv "WORKSPACE"))
    (>>=dir-join >>=!home-dir "work" "src")
    (>>=dir-join >>=!home-dir "work")
    (>>=dir-join >>=!home-dir "src" "merchise")
    (>>=dir-join >>=!home-dir "src")
    >>=!home-dir)
  "Preferred default directory when start a new session.")


(defun >>=set-default-directory ()
  "Set the default directory to its original value."
  (if (equal (>>=canonical-directory-name default-directory) >>=!home-dir)
    (>>=set-value default-directory >>=|preferred-default-directory)))


(defun >>=default-directory ()
  "Return a shortened version of `default-directory'."
  (file-name-as-directory (abbreviate-file-name default-directory)))



;;; buffers

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



;;; modes

(defvar >>-criteria-mode-cache nil
  "Internal cache used for `>>-criteria-mode-y-or-n-p'.")


(defun >>-criteria-mode-y-or-n-p (criteria mode)
  "Version of `y-or-n-p' caching first time for CRITERIA/MODE pair.
Used for `>>=check-major-mode' when CRITERIA is a semantic identity."
  (when (bound-and-true-p >>=xorns-initialized)
    ;; always nil if Emacs is not yet initialized
    (let* ((pair (format "%s/%s" mode criteria))
	   (cached (assoc-string pair >>-criteria-mode-cache)))
      (when (null cached)
	(setq cached (cons pair (y-or-n-p (format ">>= enable '%s'? " pair))))
	(push cached >>-criteria-mode-cache))
      (cdr cached))))


(defun >>=criteria-mode-cache/reset ()
  "Reset criteria/mode pairs cache for `y-or-n-p' answers."
  (interactive)
  (setq >>-criteria-mode-cache nil))


(defun >>=check-major-mode (criteria &optional mode)
  "Check if a CRITERIA is valid to trigger a condition for a major MODE.

The value of CRITERIA could be either a list of symbols, or a symbol, or one
of the two canonical boolean values.

A list contains valid major modes, without the suffix '-mode'; for example
`python-mode' will be represented as plain `python'.

A symbol represents the semantic identity of the condition kind, in which case
a version of the function `y-or-n-p' will be used, caching the result for
every pair 'mode/criteria'.

If optional argument MODE is not given, current `major-mode' is used by
default."
  (let ((mode (>>=safe-replace "-mode$" "" (or mode major-mode))))
    (cond
      ((listp criteria)
	(member mode criteria))
      ((booleanp criteria)
	criteria)
      ((symbolp criteria)
	(>>-criteria-mode-y-or-n-p criteria mode))
      (t
	(error ">>= invalid criteria: %s" criteria)))))


(defmacro >>=major-mode-trigger (id criteria function &rest args)
  "Trigger a FUNCTION if CRITERIA is met for current `major-mode'.
CRITERIA has a semantic ID used to cache answers in `y-or-n-p' kind of
conditions.  FUNCTION is called using our remaining ARGS."
  `(let ((aux (if (eq ,criteria 'ask) ',id ,criteria)))
     (if (>>=check-major-mode aux)
       (,function ,@args))))


(provide 'xorns-tools)
;;; xorns-tools.el ends here
