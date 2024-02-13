;;; xorns-tools.el --- Common Systems Tools  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This library defines several basic and general utilities that can be used
;; in any context.

;; The last section allows redefining the mechanism for setting global
;; keybindings.  This is useful for libraries that need to do that
;; (e.g. `exwm-input-set-key' if you use `exwm').

;; Enjoy!


;;; Code:

(require 'subr-x)    ; for `string-trim'
(eval-when-compile
  (require 'cl-lib)
  (require 'map))
(require 'files)
(require 'project)


;;; general

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


;; TODO: never used, maybe make obsolete
(defun >>=var-value (variable &optional default)
  "Return the value of a VARIABLE, or DEFAULT if it is void."
  (let ((symbol (intern-soft variable)))
    (if (and symbol (boundp symbol))
      (symbol-value symbol)
      ;; else
      default)))


;; TODO: set-default-toplevel-value
(defun >>=set (symbol value)
  "Set SYMBOL to the given VALUE.
Similar to `set' but calling `custom-load-symbol' if needed."
  ;; TODO: Check `setopt' macro in module "cus-edit.el"
  (unless
    (or
      (get symbol 'standard-value)
      (car (memq (get symbol 'custom-autoload) '(nil noset))))
    (custom-load-symbol symbol))
  (set symbol value))


(defalias '>>=value-of 'bound-and-true-p)


(defmacro >>=get-original-value (symbol)    ; TODO: check this
  "Return SYMBOL's original value or nil if that is void."
  `(if (boundp ',symbol)
     (or
       (eval (car (get ',symbol 'standard-value)))
       (default-value ',symbol))))


(defmacro >>=restore-original-value (symbol)
  "Restore SYMBOL's original value."
  `(setq ,symbol (>>=get-original-value ,symbol)))


(defsubst >>=real-symbol (object)
  "Return if OBJECT is a real symbol (not including a boolean)."
  (and (symbolp object) (not (eq object t)) object))


(defsubst >>=customized? (symbol)
  "Return t if SYMBOL’s value is already customized.
Same protocol as `boundp', but not the same as the `custom-variable-p'
function."
  (or
    (get symbol 'customized-value)
    (get symbol 'saved-value)))


(defsubst >>=load (file)
  "Load a FILE silently except if in debug mode."
  (let ((silent (not init-file-debug)))
    (load file silent silent)))


(defsubst >>=init-time ()
  "Initialization time in seconds for this session."
  (float-time (time-subtract after-init-time before-init-time)))




;;; object oriented programming

(defalias 'super 'cl-call-next-method)
(defalias 'defgeneric 'cl-defgeneric)
(defalias 'defmethod 'cl-defmethod)
(defalias 'typep 'cl-typep)
(defalias 'instance-of 'cl-typep)



;;; string - symbol conversion

(defun >>=prefix (string size)
  "Return the STRING prefix of SIZE characters."
  (if (> size (length string))
    string
    ;; else
    (substring string 0 size)))


(defun >>=suffix (string size)
  "Return the STRING prefix of SIZE characters."
  (if (> size (length string))
    string
    ;; else
    (substring string (- size))))


(defun >>=prefix-beffore (regexp string)
  "Return the STRING prefix before REGEXP is matched."
  (let ((space (string-match-p regexp string)))
    (if space
      (substring string 0 space)
      ;; else
      string)))


(defsubst >>=str (value &optional strict)
  "Return a string only if VALUE is a symbol or a string.
When STRICT is given, and this function fails, an error is issued, otherwise
nil is returned."
  (cond
    ((stringp value)
      value)
    ((>>=real-symbol value)
      (symbol-name value))
    (strict
      (error ">>= %s'%s' is not a string nor a symbol"
        (if (eq strict t) "" (format "%s " strict)) value))))


(defsubst >>=str-non-empty (string)
  "Return a STRING if it is not empty."
  (when (and string (not (string-empty-p string)))
    string))


(defsubst >>=str-trim (string &optional trim-left trim-right)
  "Trim a STRING using `string-trim' but returning nil on an empty result.
Arguments TRIM-LEFT and TRIM-RIGHT are used verbatim."
  (when string
    (>>=str-non-empty (string-trim string trim-left trim-right))))


(defsubst >>=str-first-line (string)
  "Return the first line of a STRING."
  (when string
    (car (split-string string "[\n\r]"))))


(defun >>=safe-replace (regexp rep source)
  "Replace all occurrences for REGEXP with REP in SOURCE.
Argument SOURCE could be either a string or a symbol, return a new value of
the same type containing the replacements.  See `replace-regexp-in-string'
function."
  (let* ((is-symbol (symbolp source))
         (value (if is-symbol (symbol-name source) source))
         (res (replace-regexp-in-string regexp rep value)))
    (if is-symbol (intern res) res)))



;;; functions

(define-error '>>=quit ">>= xorns quit" 'quit)


(defmacro ->? (function &rest arguments)
  "Call FUNCTION when it is bound, passing remaining ARGUMENTS.
This is a safe macro that prints a debug message if `init-file-debug' is not
nil."
  `(when (fboundp ',function)
     (if init-file-debug
       (message ">>= calling: %s"
         (or
           (>>=str-first-line (documentation ',function))
           ,(symbol-name function))))
     (condition-case err
       (,function ,@arguments)
       (error
         (message ">>= error in '%s': %s\n"
           ',(symbol-name function) (error-message-string err))
         (signal (car err) (cdr err))))))


(defsubst >>=call? (function &rest arguments)
  "Call FUNCTION if it is not void, passing remaining ARGUMENTS to it.
Return nil when FUNCTION is not defined."
  (when (fboundp function)
    (apply function arguments)))


(defsubst >>=safe-eval (form &optional lexical)
  "Evaluate FORM using LEXICAL scoping and return its value.
Similar to `eval' but returns nil on errors."
  (condition-case nil
    (eval form lexical)
    (error nil)))


(defun >>=cast-function (value)
  "Recognize a function in VALUE.
This function does not check if the result is valid, use `>>=check-function'
for that purpose."
  (cond
    ((>>=real-symbol value))
    ((memq (car-safe value) '(quote function))
      (>>=cast-function (cadr value)))
    ((memq (car-safe value) '(lambda closure))
      value)
    (t
      (condition-case nil
        (>>=cast-function (eval (macroexpand value) t))
        (error value)))))


(defsubst >>=funcall (function &rest arguments)
  "Return the value calling FUNCTION passing remaining ARGUMENTS to it."
  (let ((fn (>>=cast-function function)))
    (if (>>=real-symbol fn)
      (eval `(,fn ,@arguments))
      ;; else
      (apply fn arguments))))


(defsubst >>=check-function (value &optional strict)
  "Check if VALUE is an existing function.
When STRICT is not nil and VALUE is not a function, an error is issued."
  (when (stringp value)
    (setq value (intern-soft value)))
  (if (functionp value)
    value
    ;; else
    (when strict
      (error ">>= wrong%s function '%s'"
        (if (eq strict t) "" (format " %s" strict)) value))))


(defsubst >>=normalize-function (value)
  "Normalize VALUE as a function for macro expansion."
  (let ((res (>>=cast-function value)))
    (if (>>=real-symbol res) (list 'quote res) res)))


(defsubst >>=function-arglist (function)
  "Return the argument list for the FUNCTION formatted as a string."
  (let ((res (help-function-arglist function t)))
    (if res (format "%s" res) "()")))


(defun >>=function-repr (function)
  "Return FUNCTION string representation.
For a lambda function, its documentation is returned if it exists."
  (when (functionp function)
    (cond
      ((symbolp function)
        (symbol-name function))
      ((when-let ((doc (>>=str-first-line (documentation function 'raw))))
         (format "<%s>" (string-trim-right doc "[.]"))))
      ((when-let ((kind (>>=real-symbol (car-safe function))))
         (when (eq kind 'closure)
           (setq kind 'lambda))
         (format "(%s %s ...)" kind (>>=function-arglist function))))
      ((format "%s:%s" (type-of function) function)))))


(defmacro >>=breaker (function)
  "FUNCTION wrapper to signal a quit condition on any standard error.
Wrapper to signal a quit condition on any standard error on a FUNCTION.  Can
be used to mark a function as a breaker in a `>>=function-chain' argument."
  `(lambda (value)
     (condition-case err
       (,function value)
       (error
         (signal '>>=quit err)))))


(defalias '>>=chain '>>=function-chain)
(defmacro >>=function-chain (&rest functions)
  "Compose a chain of FUNCTIONS.

Similar to function composition but FUNCTIONS are applied from left to right,
It can be thought of as a chaining process in which the output of one function
becomes the input of the next.

If a result is nil, that step is ignored.  Also when an error occurs, the
signaling step is ignored too unless you use `>>=breaker' to wrap a function
that breaks the chain.

This feature is especially useful for checking macro arguments at compile
time.

Example:
  (funcall (>>=function-chain car eval 1+) (quote ((+ 5 3) is not 9)))"
  `(lambda (value)
     (dolist (fn '(,@(mapcar '>>=cast-function functions)) value)
       (condition-case nil
         (when-let ((aux (>>=funcall fn value)))
           (setq value aux))
         (error)))))



;;; lists, property lists extensions

(define-obsolete-function-alias '>>=length '>>=slist-length "0.9.7")
(defsubst >>=slist-length (value)
  "Return the `length' of a VALUE that is a strict list, nil otherwise."
  (if (and (listp value) (listp (cdr value)))
    (length value)))


(defsubst >>=cast-list (value)
  "Force VALUE to be a strict list."
  (if (>>=slist-length value) value (list value)))


(defsubst >>=list-value (value)
  "Extract a singleton VALUE from a list if it has only one value."
  (if (eq (>>=slist-length value) 1) (car value) value))


(defun >>=fix-rest-list (value)
  "Normalize VALUE used as rest-list argument."
  (let ((len (>>=slist-length value)))
    (if len
      (if (eq len 1)
        (let ((res (car value)))
          (if (>>=slist-length res) res value))
        ;; else
        value)
      ;; else
      (list value))))


(defmacro >>=append (target &rest sequences)
  "Set TARGET to the result value from appending it with all the SEQUENCES."
  `(setq ,target (append ,target ,@sequences)))


(defsubst >>=list/find (predicate options)
  "Find the first item satisfying PREDICATE in OPTIONS sequence.
Return the matching (not nil) PREDICATE result, or nil if not found."
  (let (res)
    (while (and (not res) options)
      (when-let ((item (funcall predicate (pop options))))
        (setq res item)))
    res))


(defun >>=list/deep-find (key sequence &optional test)
  "Return all KEY occurrences in SEQUENCE at any depth level.
Equality is defined by the function TEST, defaulting to `equal'."
  (when (consp sequence)
    (let ((head (car sequence))
          (tail (cdr sequence)))
      (if (funcall (or test 'equal) head key)
        (>>=cast-list tail)
        ;; else
        (append
          (>>=list/deep-find key head test)
          (>>=list/deep-find key tail test))))))


(defmacro >>=alist-do (spec &rest body)
  "Loop over an association-list using SPEC.

Evaluate BODY with KEY-VAR and VALUE-VAR bound to each pair from ALIST, in
turn.  Then evaluate RESULT to get return value, default nil.

Based on `dolist' original macro.  Keys are not checked to be valid keywords,
so this macro can be used to iterate over tuples of two values in any list.

\(fn (KEY-VAR VALUE-VAR ALIST [RESULT]) BODY...)"
  (declare (indent 1) (debug ((symbolp form &optional form) body)))
  (unless (consp spec)
    ;; TODO: must be in a "(declare (compiler-macro ", see `keymap-set'
    ;; (cl-define-compiler-macro FUNC ARGS &rest BODY)
    (signal 'wrong-type-argument (list 'consp spec)))
  (unless (<= 3 (length spec) 4)
    (signal 'wrong-number-of-arguments (list '(3 . 4) (length spec))))
  (let ((temp '>>--alist-do-tail--))
    (if lexical-binding
      `(let ((,temp ,(nth 2 spec)))
         (while ,temp
           (let ((,(car spec) (caar ,temp))
                 (,(nth 1 spec) (cdar ,temp)))
             ,@body
             (setq ,temp (cdr ,temp))))
         ,@(cdr (cdr (cdr spec))))
      ;; else
      `(let ((,temp ,(nth 2 spec))
             ,(car spec)
             ,(nth 1 spec))
         (while ,temp
           (setq
             ,(car spec) (caar ,temp)
             ,(nth 1 spec) (cdar ,temp))
           ,@body
           (setq ,temp (cdr ,temp)))
         ,@(if (cdr (cdr (cdr spec)))
             `((setq
                 ,(car spec) nil
                 ,(nth 1 spec) nil)
                ,@(cdr (cdr (cdr spec)))))))))


(defmacro >>=plist-do (spec &rest body)
  "Loop over a property-list using SPEC.

Evaluate BODY with KEY-VAR and VALUE-VAR bound to each pair from PLIST, in
turn.  Then evaluate RESULT to get return value, default nil.

Based on `dolist' original macro.  Keys are not checked to be valid keywords,
so this macro can be used to iterate over tuples of two values in any list.

\(fn (KEY-VAR VALUE-VAR PLIST [RESULT]) BODY...)"
  (declare (indent 1) (debug ((symbolp form &optional form) body)))
  (unless (consp spec)
    ;; TODO: must be in a "(declare (compiler-macro ", see `keymap-set'
    ;; (cl-define-compiler-macro FUNC ARGS &rest BODY)
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


(defun >>=plist-find-any (target &rest keys)
  "Find first member of any of the KEYS within the TARGET property-list."
  (let (res)
    (while (and keys (not res))
      (let ((aux (plist-member target (car keys))))
        (if aux
          (setq res aux)
          ;; else
          (setq keys (cdr keys)))))
    res))


(defun >>=plist-get-any (target &rest keys)
  "Find first member of any of the KEYS within the TARGET property-list."
  (let (res)
    (while (and keys (not res))
      (let ((aux (plist-get target (car keys))))
        (if aux
          (setq res aux)
          ;; else
          (setq keys (cdr keys)))))
    res))


(defun >>=plist-delete (target &rest keys)
  "Delete all KEYS in-place from a TARGET property-list."
  (dolist (key (>>=fix-rest-list keys) target)
    (map-delete target key)))


(define-obsolete-function-alias '>>=plist-exclude '>>=plist-remove "1.0")
(defsubst >>=plist-remove (target &rest keys)
  "Return a copy of a TARGET property-list with all KEYS excluded."
  (>>=plist-delete (copy-sequence target) keys))


(defun >>=plist-update (target &rest source)
  "Update TARGET from a SOURCE property-list."
  (>>=plist-do (key value (>>=fix-rest-list source) target)
    (if value
      (plist-put target key value)
      ;; else
      (map-delete target key))))


(defun >>=map-pair (fn sequence)
  "Apply FN to each `(key value)' pair of SEQUENCE.
Function FN must take two arguments but return a single value, not a pair."
  (let (key)
    (mapcan
      (lambda (item)
        (if key
          (prog1
            (list key (funcall fn key item))
            (setq key nil))
          ;; else
          (setq key item)
          nil))
      sequence)))


(defun >>=split-list (pred xs)
  "Split list XS into a `cons' of two lists (HEAD . TAIL).
HEAD is all successive items of XS for which (PRED item) returns nil.  TAIL is
a list of all items remaining starting from the first for which (PRED item)
returns a non-nil value."
  (let ((ys (list nil)) (zs (list nil)) flip)
    (dolist (x xs)
      (if flip
        (nconc zs (list x))
        (if (funcall pred x)
          (progn
            (setq flip t)
            (nconc zs (list x)))
          (nconc ys (list x)))))
    (cons (cdr ys) (cdr zs))))


(defun >>=pair-list (&rest flat-list)
  "Convert a FLAT-LIST into a paired-list."
  (setq flat-list (>>=fix-rest-list flat-list))
  (let (key res)
    (while flat-list
      (let ((value (car flat-list)))
        (if key
          (setq
            res (nconc res `((,key ,value)))
            key nil)
          ;; else
          (setq key value)))
      (setq flat-list (cdr flat-list)))
    (when key
      (setq res (nconc res `((,key)))))
    res))


(defun >>=plist-merge (target key &rest values)
  "Merge VALUES into a TARGET property-list KEY."
  (plist-put target key
    (if (plist-member target key)
      (append (>>=cast-list (plist-get target key)) values)
      ;; else
      (>>=list-value values))))


(defun >>=plist2alist (&rest pairs)
  "Convert a property-list PAIRS into an assotiation-list."
  (setq pairs (>>=fix-rest-list pairs))
  (let (key res)
    (while pairs
      (let ((value (car pairs)))
        (if key
          (setq
            res (nconc res `((,key . ,value)))
            key nil)
          ;; else
          (setq key value)))
      (setq pairs (cdr pairs)))
    (when key
      (setq res (nconc res `((,key)))))
    res))


(define-obsolete-function-alias
  '>>=plist-setdefault '>>=plist-set-defaults "0.9.8")
(defun >>=plist-set-defaults (target &rest defaults)
  "Insert into TARGET all DEFAULTS properties that are not already members.
Return updated TARGET."
  (>>=plist-do (key value (>>=fix-rest-list defaults) target)
    (when (and value (plist-get target key))
      (plist-put target key value))))


(defun >>=plist-fix (&rest source)
  "Fix a pseudo SOURCE property-list into a regular one."
  (setq source (delq 'elisp--witness--lisp (>>=fix-rest-list source)))
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
              (if (eq (>>=slist-length value) 1)
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
For example, in (use-package magit) the equivalent would be `use-package'
being the CLASS and `magit' being the NAME.

KEYWORDS are first fixed by the `>>=plist-fix' function, then DEFAULTS are
updated.

Finally each VALUE is normalized as follows: function `eval' is applied when
the VALUE is defined using the form (:eval <lisp-expression>); functions
`<CLASS>-normalize/<:KEY>' and `<NAME>-normalize/<:KEY>' are applied when
defined, these functions must be defined to get three arguments (KEY VALUE
KEYWORDS).  KEYWORDS will be passed as the lexical environment argument."
  (declare (obsolete nil "0.9.7"))    ;; never used
  (if (null keywords)
    (setq keywords defaults)
    ;; else
    (setq keywords (>>=plist-fix keywords))
    (>>=plist-do (key value defaults)
      (if (keywordp key)
        (when (not (plist-member keywords key))
          (plist-put keywords key value))
        ;; else
        (error ">>= '%s' must be a keyword, not %s" key (type-of key)))))
  (>>=plist-do (key value keywords)
    (let (changed)
      (when (eq (car-safe value) :eval)
        (if (eq (length value) 2)
          (setq
            value (eval (cadr value) `((keywords . ,keywords)))
            changed t)
          ;; else
          (error ">>= eval form '%s' must have two elements, not %s"
            value (length value))))
      (dolist (prefix `(,class ,name))
        (when-let* ((name (format "%s-normalize/%s" prefix key))
                    (check (>>=check-function name)))
          (setq
            value (funcall check value keywords)
            changed t)))
      (if changed
        (plist-put keywords key value))))
  keywords)


(defun >>=plist-rename-aliases (target &rest aliases)
  "Rename a set of ALIASES in a TARGET property-list.
ALIASES is given as an association-list of (CURRENT . NEW) pairs.  It could
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


(defun >>=alist-parse (&rest pairs)
  "Normalize a list PAIRS into an assotiation-list.
Similar to `>>=plist2alist' but members on key positions that already are a
`consp' are directly used."
  (setq pairs (>>=fix-rest-list pairs))
  (let (key pair res)
    (while pairs
      (let ((value (car pairs)))
        (if key
          (setq
            pair (cons key value)
            key nil)
          ;; else
          (if (consp value)
            (setq pair value)
            ;; else
            (setq key value)))
        (when pair
          (setq
            res (nconc res (list pair))
            pair nil)))
      (setq pairs (cdr pairs)))
    (when key
      (setq res (nconc res `((,key)))))
    res))



;;; misc utils

(defun >>=mode-find (mode &rest check-modes)
  "Not-nil if the MODE is one of the CHECK-MODES.
If MODE is an alias, then look up the real mode function first."
  (when-let ((alias (symbol-function mode)))
    (when (symbolp alias)
      (setq mode alias)))
  (car (memq mode (>>=fix-rest-list check-modes))))


(defun >>=derived-mode-p (child &rest parent-modes)
  "Non-nil if CHILD is a derived mode from one of the PARENT-MODES."
  (apply 'provided-mode-derived-p child (>>=fix-rest-list parent-modes)))


(defun >>=validate-major-mode (mode)
  "Verify a `major-mode' symbol.
MODE could be a symbol; or a string in which case it is converted to a symbol
by adding the suffix '-mode' and then using using `intern'."
  (when (and (symbolp mode) (not (booleanp mode)))
    (setq mode (symbol-name mode)))
  (if (stringp mode)
    (intern
      (let ((suffix "-mode"))
        (if (string= (>>=suffix mode (length suffix)) suffix)
          mode
          ;; else
          (concat mode suffix))))
    ;; else
    (error ">>= invalid value '%s' for major-mode" mode)))



;;; basic editing commands

(defun >>=kill-new (string)
  "Make STRING the latest kill in the kill ring unless it is already there."
  (unless (equal string (car kill-ring))
    (kill-new string)))


(defun >>=yank-filename (&optional prefix)
  "Make buffer abbreviate file-name the latest kill in the kill ring.
Optional argument PREFIX controls whether the line-number must be included,
`C-u'; or the true name representation of the file-name, `C-0'; any other
value will combine both logics."
  (interactive "P")
  (let (name ln)
    (cond
      ((null prefix)
        (setq name buffer-file-name))
      ((consp prefix)
        (setq
          name buffer-file-name
          ln t))
      ((eq prefix 0)
        (setq name buffer-file-truename))
      (t
        (setq
          name buffer-file-truename
          ln t)))
    (>>=kill-new
      (if name
        (concat
          (abbreviate-file-name name)
          (if ln (format ":%s:" (line-number-at-pos)) ""))
        ;; else
        (or
          (bound-and-true-p exwm-title)
          (buffer-name))))))


(defun >>=yank-default-directory ()
  "Make default directory the latest kill in the kill ring."
  (interactive)
  (>>=kill-new
    (file-name-as-directory (abbreviate-file-name default-directory))))



;;; files and directories

(define-obsolete-variable-alias '>>=!path/separator '>>=!path-separator "1.0")
(defconst >>=!path/separator
  "Character used by the operating system to separate pathname components."
  (substring (file-name-as-directory "x") 1))


(define-obsolete-function-alias '>>=dir-join '>>=path/join "1.0")
(defun >>=path/join (&rest parts)
  "Join PARTS to a single path."
  (setq parts (delq nil parts))
  (when parts
    (let ((res (mapconcat 'file-name-as-directory parts "")))
      (if (file-directory-p res) res (directory-file-name res)))))


(defun >>=path/find-existing (&rest options)
  "Find the first existing file or directory in a sequence of OPTIONS."
  (let (res)
    (while (and (not res) (consp options))
      (let ((option (pop options)))
        (if (and option (file-exists-p option))    ; discard nil options
          (setq res option))))
    res))


(defun >>=path/find-regular (&rest options)
  "Find the first existing file or directory in a sequence of OPTIONS."
  (let (res)
    (while (and (not res) (consp options))
      (let ((option (pop options)))
        (if (and option (file-exists-p option))    ; discard nil options
          (setq res option))))
    res))


(define-obsolete-function-alias '>>=find-dir '>>=path/find-directory "1.0")
(defun >>=path/find-directory (&rest options)
  "Find the first existing directory in a sequence of OPTIONS."
  (let (res)
    (while (and (not res) (consp options))
      (let ((option (pop options)))
        (if (and option (file-directory-p option))    ; discard nil options
          (setq res option))))
    res))


(defun >>=path/ensure-directory (&rest parts)
  "Create the directory which name is specified by joining PARTS.
See `mkdir' and `>>=path/join'."
  (let ((name (apply '>>=path/join parts)))
    (unless (file-directory-p name)
      (mkdir name 'parents))
    name))


(define-obsolete-function-alias
  '>>=canonical-directory-name '>>=path/canonical-directory-name "1.0")
(defun >>=path/canonical-directory-name (name)
  "Convert directory NAME to absolute canonical form."
  (when name
    (expand-file-name (file-name-as-directory name))))


(defun >>=file-in-dir-tree (files base &optional exclude)
  "Return the first item member of FILES and is inside the BASE directory.
The optional argument EXCLUDE could be a string or a list of strings."
  ;; Based on `dired-in-this-tree'
  (let ((base (concat "^" (regexp-quote (expand-file-name base))))
        (files (if (stringp files) (list files) files))
        case-fold-search)
    (when (stringp exclude)
      (setq exclude (list exclude)))
    (setq exclude (mapcar 'expand-file-name exclude))
    (seq-find
      (lambda (item)
        (let ((fn (expand-file-name item)))
          (and
            (not (member fn exclude))
            (string-match-p base fn))))
      files)))


(defun >>=find-env-executable (format &rest options)
  "Find the first valid command from a set of OPTIONS.
This is different from `>>=executable-find' in that each option is first
formatted with the FORMAT string, upcased, and looked up in the environment
using `getenv'.  Another difference is that the result is a `cons' with the
form (OPTION . COMMAND)."
  (declare (obsolete nil "0.9.5"))    ;; never used
  (let ((aux (delq nil (>>=fix-rest-list options)))
        (fmt (or format "%s"))
        res)
    (while (and aux (not res))
      (let* ((var (>>=str (car aux)))
             (env-var (getenv (upcase (format fmt var))))
             (tmp (executable-find (or env-var var))))
        (if tmp
          (setq res (cons var tmp))))
      (setq aux (cdr aux)))
    res))


(defun >>=executable-find (&rest options)
  "Search first valid command using the function `executable-find'.
A set of OPTIONS is searched until a valid one is found.  Any item could be a
symbol, a string, or a `cons' like `(command . args)', nil items are just
discarded."
  (setq options (>>=fix-rest-list options))
  (let (res)
    (while (and options (null res))    ; discard nil options
      (when-let ((item (car options)))
        (if (consp item)
          (when-let (aux (executable-find (>>=str (car item))))
            (setq res (cons aux (cdr item))))
          ;; else
          (when-let (aux (executable-find (>>=str item)))
            (setq res aux))))
      (setq options (cdr options)))
    res))


(defun >>=file-string (file)
  "Return the trimmed contents of the given FILE as a string."
  (if (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (string-trim (buffer-string)))))



;;; workspace management

(defconst >>=!home-dir
  (purecopy (>>=path/canonical-directory-name "~"))
  "Home directory.")


(defvar >>=|preferred-default-directory
  (>>=path/find-directory
    (>>=path/canonical-directory-name (getenv "WORKSPACE"))
    (>>=path/join >>=!home-dir "work" "src")
    (>>=path/join >>=!home-dir "work")
    (>>=path/join >>=!home-dir "src")
    >>=!home-dir)
  "Preferred default directory when start a new session.")


(defun >>=set-default-directory ()
  "Set the default directory to its original value."
  (let ((aux (>>=path/canonical-directory-name default-directory)))
    (when (equal aux >>=!home-dir)
      (setq default-directory >>=|preferred-default-directory))))



;;; buffers and windows

(defun >>=local-buffer (&optional buffer)
  "Not nil if BUFFER visits a local (not remote) file."
  (let ((fname (buffer-file-name buffer)))
    (and fname (not (file-remote-p fname)))))


(defun >>=current-buffer-remote? ()
  "Return non-nil if current buffer is remote."
  ;; TODO: this is redundant with previous function
  (require 'files)
  (seq-find
    (lambda (test) (and (stringp test) (file-remote-p test)))
    (list (buffer-file-name) list-buffers-directory default-directory)))


(defun >>=buffer-focused-text ()
  "Return focused-text in current buffer (selected region or current line)."
  (let ((region (use-region-p))
        begin
        end)
    (if region
      (setq
        begin (point)
        end (mark))
      ;; else
      (save-restriction
        (widen)
        (save-excursion
          (beginning-of-line)
          (setq begin (point))
          (end-of-line)
          (setq end (point)))))
    (prog1
      (buffer-substring-no-properties begin end)
      (if region
        (setq deactivate-mark t)))))


;; TODO: not used
(defun >>=buffer-name-match (name regexp)
  "Test function using a REGEXP pattern to be used in `>>=rename-buffer'.
It uses `string-match-p' internally to find the NAME."
  (string-match-p regexp name))


(defun >>=rename-buffer (name &optional replacements)
  "Give the current buffer a new unique NAME.
If optional argument REPLACEMENTS is not nil, an alternative NAME is matched
in this association list ((REGEX . ALTERNATIVES) ...)."
  (when-let ((rep (cdr (assoc name replacements 'string-match-p))))
    (if (stringp rep)
      (setq name rep)
      ;; else
      (while rep
        (setq
          name (car rep)
          rep (if (get-buffer name) (cdr rep) nil)))))
  (rename-buffer name 'unique))



;;; modes
;; TODO: refactor all this section

(defvar >>-criteria-mode-cache nil
  "Internal cache used for `>>-criteria-mode-y-or-n-p'.")


(defun >>-criteria-mode-y-or-n-p (criteria mode)
  "Version of `y-or-n-p' caching first time for CRITERIA/MODE pair.
Used for `>>=check-major-mode' when CRITERIA is a semantic identity."
  (when after-init-time
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

A list contains valid major modes, without the suffix `-mode'; for example
`python-mode' will be represented as plain `python'.

A symbol represents the semantic identity of the condition kind, in which case
a version of the function `y-or-n-p' will be used, caching the result for
every pair `<MODE>/<CRITERIA>'.

If optional argument MODE is not given, current `major-mode' is used by
default."
  (let ((mode (>>=safe-replace "-mode$" "" (or mode major-mode))))
    (cond
      ((listp criteria)
        (member mode criteria))
      ((booleanp criteria)
        criteria)
      ((functionp criteria)
        (funcall criteria))
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


(defun >>=mode-command-alist (source command &rest modes)
  "Join the SOURCE association-list with a sequence of `(MODE . COMMAND)'.
Each item in MODES is validated and associated with the given COMMAND."
  (if modes
    (let
      ((new
         (mapcar
           (lambda (mode) (cons (>>=validate-major-mode mode) command))
           (>>=fix-rest-list modes))))
      (append
        new
        (delq nil
          (mapcar
            (lambda (tuple) (if (not (assq (car tuple) new)) tuple))
            source))))
    ;; else
    source))



;;; system

(defun >>=command/get-name (command &optional full)
  "Extract the executable name from a COMMAND string.
If FULL is nil, return only the file name part sans its directory."
  (let ((res (>>=prefix-beffore "[[:space:]]" command)))
    (if full res (file-name-nondirectory res))))


(define-obsolete-function-alias
  '>>=setup/command-check '>>=command/check "1.0")
(defun >>=command/check (command)
  "Check if a system COMMAND is installed.
Intended to find out if a feature that depends on the given command can be
configured."
  ;; See `use-package-ensure-system-package' fo a more elaborated solution.
  (when command
    (or
      (executable-find command)
      (when init-file-debug
        (warn ">>= warning: '%s' command is not installed" command)
        nil))))


(defun >>=shell-command-to-string (command)
  "Execute shell COMMAND and return its output as a trimmed string."
  (string-trim (shell-command-to-string command)))


(defun >>=process/safe-lines (program &rest args)
  "Execute PROGRAM with ARGS, returning its output as a list of lines.
Returns nil, if an error is signaled."
  (when-let ((executable (>>=executable-find program)))
    (when (and (eq (length args) 1) (listp (car args)))
      (setq args (car args)))
    (condition-case nil
      (apply 'process-lines executable args)
      (error nil))))



;;; Xorns Lisp configuration files

;; TODO: not used
(defun >>=config/read-lisp (file)
  "Read a configuration FILE into a Lisp form.
Each line must be an independent form."
  (if (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let (res)
        (ignore-errors
          (while (not (eobp))
            (setq res (cons (read (current-buffer)) res))))
        (nreverse res)))))


;; TODO: not used
(defun >>=config/write-lisp (file form)
  "Write a Lisp FORM into a configuration FILE.
Each line will be an independent form."
  (with-temp-buffer
    (mapc (lambda (line) (insert (format "%S\n" line))) form)
    (write-file file)))



;;; projects

(defvar >>-project/variables '()
  "Association list mapping project variables.")


(defun >>=project-root (&optional dir)
  "Retrieves the root directory of a project if available.
If DIR is not supplied its set to the current directory by default."
  ;; Function `projectile-project-root' was added to commands section in
  ;; `projectile' configuration (see `xorns-project' module).
  (>>=path/canonical-directory-name
    (if (fboundp 'projectile-project-root)
      (funcall 'projectile-project-root dir)
      ;; else
      (when-let (project (project-current nil dir))
        (project-root project)))))


;; TODO: not used
(defun >>=project/get-value (symbol &optional dir)
  "Return SYMBOL's value in the context of a project.
Argument DIR is used to determine the project root; when not given, the
current directory is used by default.  If the symbol is not found in the
project's local variable mapping, the standard value is returned if bound."
  (cdr
    (or
      (when-let*
        ((prj (>>=project-root dir))
         (vars (alist-get prj >>-project/variables nil nil #'string=)))
        (assq symbol vars))
      (cons nil (and (boundp symbol) (symbol-value symbol))))))


;; TODO: not used
(defun >>=project/set-value (symbol value &optional dir)
  "Set SYMBOL's VALUE in the context of a project.
Argument DIR is used to determine the project root; when not given, the
current directory is used by default.  An error is issued if no project is
found in DIR."
  (let ((prj (>>=project-root dir)))
    (if prj
      (let ((vars (assoc prj >>-project/variables #'string=)))
      ;; (let ((vars (alist-get prj >>-project/variables nil nil #'string=)))
      (if vars
        (setf (alist-get symbol (cdr vars) nil 'remove) value)
        ;; else
        (nconc
          >>-project/variables
          (list (list prj (cons symbol value))))))
      ;; else
      (if (boundp symbol)
        (set symbol value)
        ;; else
        (let ((aux (if dir (format "'%s'" dir) "default-directory")))
          (error ">>= void variable '%s' for project %s" symbol aux))))))



;;; key bindings

(define-obsolete-function-alias '>>-kbd '>>=key-parse "0.9.7")
(defsubst >>=key-parse (key)
  "Convert KEY to the internal Emacs key representation (a vector).
Similar to `kbd' function with a main extension: if KEY is already a `vector',
return the value unchanged.  The result of this function can be used in any of
the functions in the `define-key' family.  For an inverse of this, see
`>>=key-desc'."
  (if (vectorp key)
    key
    ;; else
    (let ((res (kbd key)))
      (if (stringp res)
        (vconcat res)
        ;; else
        res))))


(defsubst >>=key-desc (key)
  "Convert an internal KEY representation to a valid description (a string).
Similar to `key-description' function with a main extension: if KEY is already
valid (see `key-valid-p'), return the value unchanged.  The result of this
function can be used in any of the functions in the `keymap-set' family.  For
an inverse of this, see `>>=key-parse'."
  (cond
    ((vectorp key)
      (key-description key))
    ((key-valid-p key)
      key)
    (t
      (key-description key))))


(defsubst >>=key-binding (key)
  "Return the binding for command KEY in current keymaps.
Similar to `key-binding', but parsing the KEY using `>>=key-parse'."
  (key-binding (>>=key-parse key)))


(defsubst >>=key-normalize (key)
  "Safe convert KEY to the internal Emacs key representation."
  (condition-case nil
    (>>=key-parse key)
    (error key)))


(defun >>=bind-global-key (key command)
  "Give KEY a global binding as COMMAND.

This function is just a wrapper to `global-set-key' to allow libraries to use
their own mechanism (see `advice-add') to set global keys, for example
`exwm-input-set-key' when configuring `exwm'.

KEY is an internal Emacs key representation (a vector), but because extended
`>>=key-parse' function is used, KEY could also be a string that satisfies
`key-valid-p' predicate.

Unless you store a key sequence as a variable value, avoid using this function
directly, instead use the `>>=bind-global-keys' macro."
  (global-set-key (>>=key-parse key) command))


(define-obsolete-function-alias
  '>>=global-set-keys '>>=bind-global-keys "0.9.7")
(defmacro >>=bind-global-keys (&rest pairs)
  "Bind on the current global keymap [KEY COMMAND] PAIRS.

PAIRS can be in plain list format or association list members (see
`>>=alist-parse').

KEY can be a string satisfying `key-valid-p' or a vector for an internal Emacs
key-sequence representation.  Keys are converted to vectors at compile time.

COMMAND can be a quoted or unquoted symbol, a lambda definition, or any valid
Lisp expression returning a command.  To use the value of a variable as a
COMMAND, use the function `identity'.  The compiler normalizes or solves
COMMAND values at compile time unless an error occurs.

Example:
  (let ((add-function '>>=xterm/add))
    (>>=bind-global-keys
      [3 116] >>=main-term
      [142606452] '>>=main-term
      \"C-`\" (let ((aux \">>=\")) (intern (format \"%sxterminal\" aux)))
      \"C-~\" (lambda () (interactive) (>>=xterm/add))
      (\"s-?\" . (identity add-function))))"
  (macroexp-progn
    (mapcar
      (lambda (pair)
        `(>>=bind-global-key
           ,(>>=key-normalize (car pair))
           ,(>>=normalize-function (cdr pair))))
      (>>=alist-parse pairs))))


(defmacro >>=remap (key command alt-key)
  "Give KEY a global binding as COMMAND.
In this case, KEY is a standard `key-binding', whose original command will
receive the alternative ALT-KEY binding."
  `(>>=bind-global-keys
     ,key ,command
     ,alt-key (>>=key-binding ,key)))


(defmacro >>=remap* (key command alt-key)
  "Give KEY a global binding as COMMAND.
In this case KEY is NOT a `key-binding' but a base command, which will receive
the alternative ALT-KEY binding."
  `(>>=bind-global-keys
     `[remap ,key] ,command
     ,alt-key ,key))


(provide 'xorns-tools)
;;; xorns-tools.el ends here
