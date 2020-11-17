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


(defun >>=var-value (variable &optional default)
  "Return the value of a VARIABLE, or DEFAULT if it is void."
  (let ((symbol (intern-soft variable)))
    (if (and symbol (boundp symbol))
      (symbol-value symbol)
      ;; else
      default)))


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


(defsubst >>=non-nil-symbol (object)
  "Return if OBJECT is a true symbol."
  (and object (symbolp object)))


(defun >>=check-function (value &optional validate)
  "Check if VALUE is an existing function.
If VALIDATE is given and VALUE is not a function, an error is issued."
  (if (functionp value)    ; TODO: difference with `fboundp'
    value
    ;; else
    (if validate
      (error ">>= wrong function form '%s'" value))))


(defun >>=cast-function (value &optional validate)
  "Recognize a function in VALUE and check it is valid.
If VALIDATE is given and VALUE is not a function, an error is issued."
  (>>=check-function
    (cond
      ((symbolp value)
	value)
      ((and (listp value)
	 (memq (car value) '(quote function))
	 (>>=non-nil-symbol (cadr value)))
	(cadr value))
      ((and (consp value) (memq (car value) '(lambda closure)))
	value)
      ((and (listp value)
	 (memq (car value) '(quote function))
	 (memq (car (cadr value)) '(lambda closure)))
	(cadr value))
      ((and (consp value) (>>=non-nil-symbol (car value)))
	;; macro building a function?
	(condition-case nil
	  (eval value)
	  (error value)))
      (t
	value))
    validate))


(defun >>=function-repr (fun)
  "Return function FUN string representation.
For a lambda function, its documentation is returned if it exists."
  (when (functionp fun)
    (if (symbolp fun)
      (symbol-name fun)
      ;; else
      (if (consp fun)
	(let* ((kind (car fun))
	       (is-closure (eq kind 'closure))
	       (doc (nth (if is-closure 3 2) fun)))
	  (if (stringp doc)
	    doc
	    ;; else
	    (if (symbolp kind)
	      (format "(%s %s ...)" kind (nth (if is-closure 2 1) fun))
	      ;; else
	      (format "%s" fun))))
	;; else
	(format "%s:%s" (type-of fun) fun)))))


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

(defun >>=safe-replace (regexp rep source)
  "Replace all occurrences for REGEXP with REP in SOURCE.

SOURCE could be either a string or a symbol, return a new value of the same
type containing the replacements.  See `replace-regexp-in-string' function."
  (let* ((is-symbol (symbolp source))
	 (value (if is-symbol (symbol-name source) source))
	 (res (replace-regexp-in-string regexp rep value)))
    (if is-symbol (intern res) res)))



;;; lists, property lists extensions

(defsubst >>=length (arg)
  "Return the length of a strict list ARG."
  (if (and (listp arg) (listp (cdr arg)))
    (length arg)))


(defsubst >>=cast-list (value)
  "Force VALUE to be a strict list."
  (if (>>=length value) value (list value)))


(defsubst >>=list-value (value)
  "Extract a singleton VALUE from a list if it has only one value."
  (if (eq (>>=length value) 1) (car value) value))


(defun >>=fix-rest-list (value)
  "Normalize VALUE used as rest-list argument."
  (let ((len (>>=length value)))
    (if len
      (if (eq len 1)
	(let ((res (car value)))
	  (if (>>=length res) res value))
	;; else
	value)
      ;; else
      (list value))))


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


(defmacro >>=append (target &rest sequences)
  "Set TARGET to the result value from appending it with all the SEQUENCES."
  `(setq ,target (append ,target ,@sequences)))


(defmacro >>=plist-do (spec &rest body)
  "Loop over a property-list using SPEC.

Evaluate BODY with KEY-VAR and VALUE-VAR bound to each pair from PLIST, in
turn.  Then evaluate RESULT to get return value, default nil.

Based on `dolist' original macro.  Keys are not checked to be valid keywords,
so this macro can be used to iterate over tuples of two values in any list.

\(fn (KEY-VAR VALUE-VAR PLIST [RESULT]) BODY...)"
  (declare (indent 1) (debug ((symbolp form &optional form) body)))
  (unless (consp spec)
    (signal 'wrong-type-argument (list 'consp spec)))
  (unless (<= 3 (length spec) 4)
    (signal 'wrong-number-of-arguments (list '(3 . 4) (length spec))))
  (let ((temp '>>--plist-do-tail--))
    (if lexical-binding
      `(let ((,temp ,(nth 2 spec)))
         (while ,temp
           (let ((,(car spec) (car ,temp))
		 (,(nth 1 spec) (nth 1 ,temp)))
             ,@body
             (setq ,temp (cdr (cdr ,temp)))))
         ,@(cdr (cdr (cdr spec))))
      ;; else
      `(let ((,temp ,(nth 2 spec))
             ,(car spec)
	     ,(nth 1 spec))
         (while ,temp
           (setq
	     ,(car spec) (car ,temp)
	     ,(nth 1 spec) (nth 1 ,temp))
           ,@body
           (setq ,temp (cdr (cdr ,temp))))
         ,@(if (cdr (cdr (cdr spec)))
             `((setq
		 ,(car spec) nil
		 ,(nth 1 spec) nil)
		,@(cdr (cdr (cdr spec)))))))))


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


(defun >>=split-list (pred xs)
  "Split list XS into a `cons' of two lists '(HEAD . TAIL)'.

HEAD is all successive items of XS for which (PRED item) returns nil.  TAIL is
a list of all items remaining starting from the first for which (PRED item)
returns a non-nil value."
  (let ((ys (list nil)) (zs (list nil)) flip)
    (cl-dolist (x xs)
      (if flip
        (nconc zs (list x))
        (if (funcall pred x)
          (progn
            (setq flip t)
            (nconc zs (list x)))
          (nconc ys (list x)))))
    (cons (cdr ys) (cdr zs))))


(defun >>=plist-merge (target key &rest values)
  "Merge VALUES into a TARGET property-list KEY."
  (plist-put target key
    (if (plist-member target key)
      (append (>>=cast-list (plist-get target key)) values)
      ;; else
      (>>=list-value values))))


(defun >>=plist-fix (source)
  "Fix a pseudo SOURCE property-list into a regular one."
  (setq source (delq 'elisp--witness--lisp source))
  (let (target multi)
    (while source
      (let* ((xs (>>=split-list #'keywordp (cdr source)))
	     (key (car source))
	     (value (car xs)))
	;; value
	(setq target
	  (plist-put target key
	    (if (plist-member target key)
	      (let ((current (plist-get target key)))
		(append
		  (if (memq key multi)
		    current
		    ;; else
		    (setq multi (cons key multi))
		    (list current))
		  value))
	      ;; else
	      (if (eq (>>=length value) 1)
		(car value)
		;; else
		(setq multi (cons key multi))
		value)))
	  source (cdr xs))))
    target))


(defun >>=plist-normalize (class name keywords &rest defaults)
  "Normalize a sequence of KEYWORDS.

KEYWORDS must be a pseudo property-list, usually provided as final arguments
to a macro that defines a new concept.

Each normalization process is identified with a CLASS and an instance NAME.
For example, in '(use-package magit)' the equivalent would be `use-package'
being the CLASS and `magit' being the NAME.

KEYWORDS are first fixed by the `>>=plist-fix' function, then DEFAULTS are
updated.

Finally each VALUE is normalized as follows: function `eval' is applied when
the VALUE is defined using the form '(:eval <lisp-expression>)'; functions
'<CLASS>-normalize/<:KEY>' and '<NAME>-normalize/<:KEY>' are applied when
defined, these functions must be defined to get three arguments (KEY VALUE
KEYWORDS).  KEYWORDS will be passed as the lexical environment argument."
  (if (null keywords)
    (setq keywords defaults)
    ;; else
    (setq keywords (>>=plist-fix keywords))
    (>>=plist-do (key value defaults keywords)
      (if (keywordp key)
	(when (not (plist-member keywords key))
	  (plist-put keywords key value))
	;; else
	(error ">>= '%s' must be a keyword, not %s" key (type-of key)))))
  (>>=plist-do (key value keywords)
    (let (changed)
      (when (and (listp value) (eq :eval (car value)))
	(if (eq (length value) 2)
	  (setq
	    value (eval (cadr value) `((keywords . ,keywords)))
	    changed t)
	  ;; else
	  (error ">>= eval form '%s' must have two elements, not %s"
	    value (length value))))
      (dolist (prefix `(,class ,name))
	(let ((check (intern-soft (format "%s-normalize/%s" prefix key))))
	  (when (functionp check)
	    (setq
	      value (funcall check value keywords)
	      changed t))))
      (if changed
	(plist-put keywords key value))))
  keywords)


(defun >>=plist-rename-aliases (target &rest aliases)
  "Rename a set of ALIASES in a TARGET property-list.

ALIASES is given as an association-list of '(CURRENT . NEW)' pairs.  It could
result in a pseudo property-list that needs additional normalization with
`>>=plist-fix'."
  (mapc
    (lambda (pair)
      (let ((cur (car pair))
	    (new (cdr pair))
	    aux)
	(if (and (keywordp cur) (keywordp new) (not (eq cur new)))
	  (while (setq aux (memq cur target))
	    (setcar aux new))
	  ;; else
	  (error ">>= must be keywords, not '%s'" pair))))
    (>>=fix-rest-list aliases))
  target)



;;; files and directories

(defconst >>=!path-separator
  "Character used by the operating system to separate pathname components."
  (substring (file-name-as-directory "x") 1))


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


(defun >>=find-env-executable (format &rest options)
  "Find the first valid command from a set of OPTIONS.
This is different from `>>=executable-find' in that each option is first
formatted with the FORMAT string, upcased, and looked up in the environment
using `getenv'.  Another difference is that the result is a `cons' with the
form '(OPTION . COMMAND)'."
  (let ((aux (delq nil (>>=fix-rest-list options)))
	(fmt (or format "%s"))
	res)
    (while (and aux (not res))
      (let* ((var (car aux))
	     (env-var (getenv (upcase (format fmt var))))
	     (tmp (executable-find (or env-var var))))
	(if tmp
	  (setq res (cons var tmp))))
      (setq aux (cdr aux)))
    res))


(defun >>=executable-find (&rest options)
  "Search first valid command using the function `executable-find'.
A set of OPTIONS is searched until a valid one is found.  This function is
safe avoiding nil commands.  If none is found, nil is returned."
  (let ((aux (delq nil (>>=cast-list options)))
	res)
    (while (and aux (not (setq res (executable-find (car aux)))))
      (setq aux (cdr aux)))
    res))


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
