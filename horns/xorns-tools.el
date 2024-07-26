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
(eval-and-compile
  (require 'cl-lib)
  (require 'map))
(require 'files)
(require 'project)


;;; general

(defsubst >>-xorns/prefix-message (string)
  "Prefix the given STRING with the xorns symbol '>>='."
  (concat ">>= " string))


(defun >>=warn (message &rest objects)
  "Display a warning MESSAGE using the OBJECTS argument to format it."
  (display-warning 'xorns (apply #'format-message message objects)))


(defun >>=error (message &rest objects)
  "Signal an `error', making a MESSAGE by formatting OBJECTS."
  (apply 'error (>>-xorns/prefix-message message) objects))


(defsubst >>=message (format-string &rest arguments)
  "Display a message at the bottom of the screen.
Similar to standard function `message' but prefixing the FORMAT-STRING with
'>>= '.  Extra ARGUMENTS can also be used."
  (apply 'message (>>-xorns/prefix-message format-string) arguments))


(defsubst >>=on-debug-message (format-string &rest args)
  "Display a message only when `init-file-debug' is active.
Use the same parameters as `message' standard function: FORMAT-STRING and
ARGS."
  (when init-file-debug
    (apply '>>=message format-string args)))


(defsubst >>=error-message (error &optional place)
  "Displays a `message' related to an ERROR that has occurred at a PLACE."
  (>>=message
    (concat "error" (if place (format " on '%s'" place) "") ": %s")
    (error-message-string error)))


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


(define-obsolete-function-alias '>>=value-of 'bound-and-true-p "0.10.19")


(defmacro >>=get-standard-value (symbol)
  "Return standard value of SYMBOL or nil if that is void."
  `(ignore-errors (eval (car (get ',symbol 'standard-value)))))


(defmacro >>=get-original-value (symbol)    ; TODO: check this
  "Return original value of SYMBOL or nil if that is void."
  `(if (boundp ',symbol)
     (or
       (eval (car (get ',symbol 'standard-value)))
       (default-value ',symbol))))


(defmacro >>=restore-original-value (symbol)    ; TODO: check this
  "Restore SYMBOL's original value."
  `(setq ,symbol (>>=get-original-value ,symbol)))


(defsubst >>=customized? (symbol)
  "Return t if SYMBOL is already customized.
Same protocol as `boundp', but not the same as the `custom-variable-p'
function."
  (or
    (get symbol 'customized-value)
    (get symbol 'saved-value)))


(defun >>=customized-value (symbol)
  "Return customized value of SYMBOL."
  (when-let ((def (>>=customized? symbol)))
    (car def)))


(defmacro >>=set-custom-value? (symbol value)
  "Set SYMBOL to VALUE if not already customized."
  (declare (debug setq))
  `(unless (>>=customized? ',symbol)
     (customize-set-variable ',symbol ,value)))


(defsubst >>=init-time ()
  "Return initialization time in seconds for this session."
  (float-time (time-subtract after-init-time before-init-time)))


(defsubst >>=load (file)
  "Load a FILE silently except if in debug mode."
  (let ((silent (not init-file-debug)))
    (load file silent silent)))



;;; object oriented programming

(defalias 'defgeneric 'cl-defgeneric)
(defalias 'defmethod 'cl-defmethod)
(defalias 'typep 'cl-typep)
(defalias 'instance-of 'cl-typep)



;;; comparison of values

(defsubst symbol-or-string-p (value)
  "Return t if VALUE is a symbol or a string."
  (or (symbolp value) (stringp value)))


(require 'compat nil 'noerror)
(if (fboundp 'value<)    ;; TODO: remove this after Emacs version 30
  (defalias '>>=value< 'value<)
  ;; else
  (defalias 'value< '>>=value<)
  (defun >>=value< (a b)
    "Return non-nil if A precedes B in standard value order.
A and B must have the same basic type.  Numbers are compared with <.  Strings
and symbols are compared with string-lessp.  Lists, vectors, bool-vectors and
records are compared in lexicographical order.  Markers are compared in
lexicographical order by buffer and position.  Buffers and processes are
compared by name.  Other types are considered unordered and the return value
will be ‘nil’."
    (cond
      ((and (number-or-marker-p a) (number-or-marker-p b))
        (< a b))
      ((and (symbol-or-string-p a) (symbol-or-string-p b))
        (string-lessp a b))
      ((and (listp a) (listp b))
        (while (and (consp a) (consp b) (equal (car a) (car b)))
          (setq a (cdr a) b (cdr b)))
        (cond
          ((not b) nil)
          ((not a) t)
          ((and (consp a) (consp b)) (value< (car a) (car b)))
          (t (value< a b))))
      ((and (vectorp a) (vectorp b))
        (let* ((na (length a))
               (nb (length b))
               (n (min na nb))
               (i 0))
          (while (and (< i n) (equal (aref a i) (aref b i)))
            (cl-incf i))
          (if (< i n) (value< (aref a i) (aref b i)) (< n nb))))
      ((and (bufferp a) (bufferp b))
        ;; `buffer-name' is nil for killed buffers.
        (setq
          a (buffer-name a)
          b (buffer-name b))
        (cond
          ((and a b) (string< a b))
          (b t)))
      ((and (processp a) (processp b))
        (string< (process-name a) (process-name b)))
      ;; TODO: add support for more types here
      ((eq (type-of a) (type-of b))
        nil)    ;; other values of equal type are considered unordered
      ;; different types.
      (t
        (error "value< type mismatch: %S %S" a b)))))


(defalias 'value= '>>=value=)
(defun >>=value= (a b)
  "Return if A is equal to B in standard value order.
See the `value<' function for more information on how values ​​are compared."
  (cond
    ((and (number-or-marker-p a) (number-or-marker-p b))
      (= a b))
    ((and (symbol-or-string-p a) (symbol-or-string-p b))
      (string-equal a b))
    ((and (listp a) (listp b))
      (and
        (eq (safe-length a) (safe-length b))
        (value= (car a) (car b))
        (value= (cdr a) (cdr b))))
    ((and (vectorp a) (vectorp b))
      (let ((n (length a))
            (i 0))
        (when (eq n (length b))
          (while (and (< i n) (value= (aref a i) (aref b i)))
            (setq i (1+ i))))))
    ((and (bufferp a) (bufferp b))
      ;; `buffer-name' is nil for killed buffers
      (let ((na (buffer-name a))
            (nb (buffer-name b)))
        (and na nb (string-equal na nb))))
    ((and (processp a) (processp b))
      (string-equal (process-name a) (process-name b)))
    ;; TODO: add support for more types here
    (t
      (equal a b))))


(defalias 'value/= '>>=value/=)
(defsubst >>=value/= (a b)
  "Return if A is different to B in standard value order.
See the `value<' function for more information on how values ​​are compared."
  (not (value= a b)))


(defalias 'value> '>>=value>)
(defsubst >>=value> (a b)
  "Return if A follows B in standard value order.
See the `value<' function for more information on how values ​​are compared."
  (value< b a))


(defalias 'value<= '>>=value<=)
(defsubst >>=value<= (a b)
  "Return whether A precedes B, or they are equal, in standard value order.
See the `value<' function for more information on how values ​​are compared."
  (or (value< a b) (value= a b)))


(defalias 'value>= '>>=value>=)
(defsubst >>=value>= (a b)
  "Return whether A follows B, or they are equal, in standard value order.
See the `value<' function for more information on how values ​​are compared."
  (or (value> a b) (value= a b)))


(defalias 'value-in '>>=value-in)
(defun >>=value-in (item sequence)
  "Return non-nil if ITEM is an element of SEQUENCE.
Comparison done with `value='."
  (cl-position item sequence :test 'value=))



;;; predicates

(defconst >>-!wrapped-nil ''nil
  "Value to be used in predicates where nil must be a valid result.
Value nil has different meanings, it could be an empty list, a flag for an
undefined value, or the boolean value false.  This is a problem for predicate
functions that should return values ​​to signal that a result is valid.  See
function `>>=car-safe' as an example.")


(defsubst >>=keywordp (value)
  "Return VALUE if it is a keyword."
  (if (keywordp value)
    value))


(defsubst >>=stringp (value)
  "Return VALUE if it is a string."
  (if (stringp value)
    value))


(defsubst >>=real-symbol (value)
  "Return if VALUE is a real symbol (not including a boolean)."
  (and (symbolp value) (not (eq value t)) value))


(defsubst >>=string-or-symbol (value)
  "Return if VALUE is a real symbol or a string."
  (or (>>=stringp value) (>>=real-symbol value)))


(define-obsolete-function-alias '>>=str-non-empty '>>=non-empty-string
  "0.11.5")
(defsubst >>=non-empty-string (value &optional required)
  "Return VALUE if it is a non-empty string, nil otherwise.
When REQUIRED is not-nil, an error is yielded if the VALUE is not a string or
is empty."
  (or
    (when (stringp value)
      (if (string-empty-p value) nil value))
    (when required
      (error
        ">>= expecting a non empty string%s, not %s '%s'"
        (if (memq required '(t required)) "" (format " for '%s'" required))
        (type-of value) value))))


(defsubst >>=non-empty-string-or-symbol (value)
  "Return if VALUE is a real symbol or a non-empty string."
  (or (>>=real-symbol value) (>>=non-empty-string value)))


(defsubst >>=as-symbol (value &optional safe)
  "Return VALUE if it is a real symbol or a non-empty string, nil otherwise.
When SAFE is not-nil, an error is yielded if the VALUE is not a string."
  (or
    (>>=real-symbol value)
    (if-let ((aux (>>=non-empty-string value)))
      (intern aux)
      ;; else
      (when safe
        (error
          ">>= invalid value %s '%s' to intern a symbol"
          (type-of value) value)))))


(defsubst >>=as-string (value &optional required)
  "Get VALUE as a string if it is a non-empty string or a real-symbol.
If REQUIRED is given, it yields an error if the VALUE can not be converted."
  (if (>>=real-symbol value)
    (symbol-name value)
    ;; else
    (>>=non-empty-string value required)))


(defsubst >>=cast-string (object)
  "Return the string representation of OBJECT."
  (if (stringp object) object (prin1-to-string object)))


(defsubst >>=str-trim (string &optional trim-left trim-right)
  "Trim a STRING using `string-trim' but returning nil on an empty result.
Arguments TRIM-LEFT and TRIM-RIGHT are used verbatim."
  (when string
    (>>=non-empty-string (string-trim string trim-left trim-right))))


(defsubst >>=wrap-nil (value)
  "Wrap a VALUE if nil."
  (or value >>-!wrapped-nil))


(defsubst >>=unwrap-nil (value)
  "Unwrap a VALUE if nil."
  (unless (equal value >>-!wrapped-nil)
    value))


(defsubst >>=quoted (value)
  "Return if a VALUE is wrapped using the `quote' function."
  (and (eq (car-safe value) 'quote) (length= value 2)))


(defsubst >>-atom (value)
  "Return if the given VALUE is atomic (nil is wrapped using `quote')."
  (when (atom value)
    (>>=wrap-nil value)))


(defsubst >>=atom (value)
  "Return a normalized VALUE if the given argument is atomic, nil otherwise.
Similar to `atom' but returns the value itself instead of a boolean (when the
value is nil the result is wrapped)."
  (or
    (>>-atom value)
    (when-let ((aux (and (>>=quoted value) (>>-atom (cadr value)))))
      (if (>>=real-symbol aux) value aux))))


(defsubst >>=car-safe (list)
  "Return the car of LIST if it is a `cons' cell, or else nil.
A null result from the standard `car-safe' function is ambiguous; it may mean
that the value at the head of the LIST is the null value itself, or that the
argument is not a list at all.  Because of that, a valid nil result is wrapped
so that this function can be used as a predicate."
  (or (car-safe list) (when (consp list) >>-!wrapped-nil)))


(defsubst >>=car (list)
  "Return the car of LIST or nil if the argument is nil.
A valid nil result is wrapped so that this function can be used as a
predicate."
  (or (car list) (and list >>-!wrapped-nil)))


(defsubst >>=nth (n list)
  "Return the Nth element of LIST.
N counts from zero.  If LIST is not that long, nil is returned.  A valid nil
result is wrapped so that this function can be used as a predicate."
  (or (nth n list) (when (< n (length list)) >>-!wrapped-nil)))



;;; string - symbol conversion

(defun >>=prefix (string size)
  "Return the STRING prefix of SIZE characters."
  (if (> size (length string))
    string
    ;; else
    (substring string 0 size)))


(defsubst >>=prefix-equal (string prefix)
  "Return STRING if it has the given PREFIX."
  (when (string-equal (>>=prefix string (length prefix)) prefix)
    string))


(defun >>=suffix (string size)
  "Return the STRING prefix of SIZE characters."
  (if (> size (length string))
    string
    ;; else
    (substring string (- size))))


(defsubst >>=suffix-equal (string suffix)
  "Return STRING if it has the given SUFFIX."
  (when (string-equal (>>=suffix string (length suffix)) suffix)
    string))


(defun >>=prefix-beffore (regexp string)
  "Return the STRING prefix before REGEXP is matched."
  (let ((count (string-match-p regexp string)))
    (if count
      (substring string 0 count)
      ;; else
      string)))


(defsubst >>=str-first-line (string)
  "Return the first line of a STRING."
  (when string
    (car (split-string string "[\n\r]"))))


(defun >>=safe-replace (regexp rep source)
  "Replace all matches for REGEXP with REP in SOURCE.
SOURCE could be either a string or a symbol, return a new value of the same
type containing the replacements.  See `replace-regexp-in-string' function."
  (when source
    (let* ((is-symbol (symbolp source))
           (value (if is-symbol (symbol-name source) source))
           (res (replace-regexp-in-string regexp rep value)))
      (if is-symbol (intern res) res))))


(defun >>=strict-replace (regexp rep source)
  "Safe replace all matches for REGEXP with REP in SOURCE.
Return nil if no replacement is made.  See `>>=safe-replace' for more
information."
  (let ((res (>>=safe-replace regexp rep source)))
    (unless (eq res source)
      res)))


(defun >>=bisect-string (source separator)
  "Split SOURCE string or symbol into two sub-strings delimited by SEPARATOR.
The sub-strings are returned in a (HEAD . TAIL) `cons'.  TAIL is optional,
defaults to nil, but HEAD is required."
  (let ((src (>>=as-string source))
        (sep (>>=as-string separator))
        (len (length separator)))
    (when-let ((pos (string-search sep src)))
      (if-let ((head (>>=non-empty-string (substring src 0 pos))))
        (cons head (>>=non-empty-string (substring src (+ pos len))))
        ;; else
        (error
          ">>= HEAD part is required when bisecting '%s' with separator '%s'"
          source separator)))))


(defun >>=split (source &optional separator)
  "Split SOURCE into sub-strings bounded by matches for SEPARATOR.
The SOURCE and SEPARATOR arguments can be strings or symbols.  A null
SEPARATOR is an alias for blanks.  Unlike the `string-split', SEPARATOR is a
literal string, not a regular expression.  All sub-strings are trimmed and all
empty ones are omitted from the list."
  (let ((src (>>=as-string source 'required))
        (sep (>>=as-string separator 'required))
        (blank "[[:blank:]]+"))
    (string-split src (if sep (regexp-quote sep) blank) 'omit-nulls blank)))


(defun >>=split-domains (source &optional count)
  "Split SOURCE up to COUNT times into a list of domains.
Argument SOURCE could be a string or a symbol.  If the optional COUNT argument
is omitted all possible values are returned.  This function is not intended to
split a string using a dot as a separator; see `split-string' for that;
instead it returns all parent domains, for example \"x.y.z\" results in (\"x\"
\"x.y\" \"x.y.z\")."
  (unless count    ;; a large number that is never possible
    (setq count 65535))
  (setq source (>>=as-string source))
  (let ((start 0)
        res)
    (while (and (> count 0) (length> source start))
      (let ((pos (string-search "." source start))
            aux)
        (if pos
          (setq
            aux (list (substring-no-properties source 0 pos))
            count (1- count)
            start (1+ pos))
          ;; else
          (setq
            aux (list source)
            count 0))
        (setq res (if res (nconc res aux) aux))))
    res))


(defsubst >>=key/counter (key)
  "Return a counter of how many times a KEY has been accessed.
Keys must be a symbol or a string."
  (when key
    (unless (boundp '>>-key--counters)
      (set-default '>>-key--counters (list (cons nil 0))))
    (let* ((counters (symbol-value '>>-key--counters))
           (counter (assoc-string key counters)))
      (if counter
        (setcdr counter (1+ (cdr counter)))
        ;; else
        (setq counter (cons key 0))
        (nconc counters (list counter)))
      (cdr counter))))


(defsubst >>=new-symbol (base-name)
  "Generate a new symbol with a unique name.
The result symbol is made by appending a unique number to BASE-NAME.  This
function is similar to `cl-gentemp' but using different approach for suffixes,
see the function `>>=key/counter'."
  (setq base-name (>>=as-string base-name 'required))
  (let (res)
    (while (not res)
      (let* ((idx (>>=key/counter base-name))
             (name (if (zerop idx) base-name (format "%s-%s" base-name idx)))
             (aux (intern-soft name)))
        (unless aux
          (setq res (intern name)))))
    res))



;;; functions and form evaluation

(define-error '>>=quit ">>= xorns quit" 'quit)


(defmacro >>=progn (&rest body)
  "Extended version of `progn' special form.
Safely evaluate BODY forms sequentially and return the value of the last one.
An error is managed as a message.  If the first form of the body is a symbol
or a string, a message is logged when `init-file-debug' is non-nil, or in case
of error, to report the identity of the enclosed body."
  (when body
    (let ((header (>>=non-empty-string (car body))))
      (when header
        (setcar body `(>>=on-debug-message ,header)))
      `(condition-case err
         ,(macroexp-progn body)
         (error
           (>>=error-message err ,header)
           (when init-file-debug
             (signal (car err) (cdr err))))))))


(defun >>=macroexp-progn (&rest exps)
  "Return EXPS (a list of expressions) with `progn' prepended.
Similar to the standard `macroexp-progn' function but EXPS can be passed as
several optional additional arguments (`&rest'), and this function also
removes all the nil elements from EXPS."
  (macroexp-progn (delq nil (>>=fix-rest-list exps))))


(defsubst >>=function-arglist (function)
  "Return the argument list for the FUNCTION formatted as a string."
  (let ((res (help-function-arglist function t)))
    (if res (format "%s" res) "()")))


(defun >>=function-repr (function)
  "Return FUNCTION string representation.
For a lambda function, its documentation is returned if it exists."
  (when (functionp function)
    (cond
      ((when-let ((doc (>>=str-first-line (documentation function 'raw))))
         (format "'%s'" (string-trim-right doc "[.]"))))
      ((symbolp function)
        (symbol-name function))
      ((when-let ((kind (>>=real-symbol (car-safe function))))
         (when (eq kind 'closure)
           (setq kind 'lambda))
         (format "(%s %s ...)" kind (>>=function-arglist function))))
      ((format "%s:%s" (type-of function) function)))))


(defmacro ->? (function &rest arguments)
  "Call FUNCTION when it is bound, passing remaining ARGUMENTS.
This is a safe macro that prints a debug message if `init-file-debug' is not
nil."
  `(when (fboundp ',function)
     (>>=progn ,(format "calling: %s" (>>=function-repr function))
       (,function ,@arguments))))


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
This function does not check if the result is valid, use `>>=function/ensure'
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


(defsubst >>=function/intern-soft (name)
  "Convert NAME to a canonical identifier for a function (a symbol).
The differences from `intern-soft' are that this function also returns nil if
the symbol exists but is not a function, and that the default value of
`obarray' is always used."
  (when-let ((res (intern-soft name)))
    (when (fboundp res)
      res)))


(defsubst >>=function/find (&rest names)
  "Return the first existing function among the given NAMES.
The result is nil if none is found."
  (>>=list/some '>>=function/intern-soft names))


(defsubst >>=function/search (pattern &rest objects)
  "Return the first existing function by searching among multiple names.
Similar to `>>=function/find' but options are obtained by formatting a PATTERN
with each element of OBJECTS."
  (apply
    '>>=function/find
    (mapcar (lambda (obj) (format pattern obj)) objects)))


(defsubst >>=function/ensure (&rest options)
  "Search for an existing function by trying among the given OPTIONS.
An error is issued if no option is valid."
  (or
    (apply '>>=function/find options)
    (error ">>= no function found in: %s" options)))


(defsubst >>=check-function (value &optional strict)
  "Check if VALUE is an existing function.
When STRICT is not nil and VALUE is not a function, an error is issued."
  (declare (obsolete use-package "0.11.5"))
  (if strict
    (>>=function/ensure value)
    ;; else
    (>>=function/intern-soft value)))



;;; lists extensions

(define-obsolete-function-alias '>>=slist-length 'proper-list-p "0.11.5")


(defsubst >>=cast-list (value)
  "Force VALUE to be a strict list."
  (if (proper-list-p value) value (list value)))


(defsubst >>=list-value (value)
  "Extract a singleton VALUE from a list if it has only one value."
  (if (eq (proper-list-p value) 1) (car value) value))


(defun >>=fix-rest-list (value)
  "Normalize VALUE used as rest-list argument."
  (let ((len (proper-list-p value)))
    (if len
      (if (eq len 1)
        (let ((res (car value)))
          (if (proper-list-p res) res value))
        ;; else
        value)
      ;; else
      (list value))))


(defmacro >>=append (target &rest sequences)
  "Set TARGET to the result value from appending it with all the SEQUENCES."
  `(setq ,target (append ,target ,@sequences)))


(defun >>=list/find (predicate sequence &rest extra)
  "Return the first element in SEQUENCE that satisfies PREDICATE.
The result is nil if no such element is found.  This function is similar to
`seq-find', but in this case the PREDICATE can take EXTRA arguments.

Note that this function has an ambiguity if the found element is nil, as in
that case it is impossible to know whether an element was found or not."
  (let (res)
    (while sequence
      (let ((item (car sequence)))
        (if (apply predicate item extra)
          (setq
            res item
            sequence nil)
          ;; else
          (setq sequence (cdr sequence)))))
    res))


(defun >>=list/some (predicate list &rest extra)
  "Return the first non-nil result of applying PREDICATE to each item of LIST.
Similarly to `>>=list/find' PREDICATE can take EXTRA arguments, but this
function returns the result instead of the found element."
  (let (res)
    (while (and (not res) list)
      (setq
        res (apply predicate (car list) extra)
        list (cdr list)))
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

The number of elements in the list does not necessarily have to be even, the
default value for the last odd tuple is nil.  Use `cl-evenp' to check that in
a client function.

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
    (setq target (map-delete target key))))


(defsubst >>=plist-remove (target &rest keys)
  "Return a copy of a TARGET property-list with all KEYS excluded."
  (>>=plist-delete (copy-sequence target) keys))


(defun >>=plist-update (target &rest source)
  "Update TARGET from a SOURCE property-list."
  (if target
    (>>=plist-do (key value (>>=fix-rest-list source) target)
      (if value
        (plist-put target key value)
        ;; else
        (map-delete target key)))
    ;; else
    source))


(defmacro >>=plist-pop (place key)
  "Return KEY value of PLACE, and remove it from PLACE.
PLACE must be symbol representing a generalized variable whose value is a
property list."
  (if (symbolp place)
    `(when-let ((value (plist-get ,place ,key)))
       (prog1
         value
         (setq ,place (map-delete ,place ,key))))
    ;; else
    (error ">>= PLACE must be a symbol, not '%s'" place)))


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


(defun >>=split-list (predicate xs)
  "Split XS into lists, each headed with an element validated by PREDICATE."
  (when xs
    (unless (functionp predicate)
      (let ((aux predicate))
        (setq predicate (lambda (arg) (value= aux arg)))))
    (let ((res (list nil))
          (pivot (list (car xs))))
      (setq xs (cdr xs))
    (dolist (x xs)
      (if (funcall predicate x)
        (progn
          (nconc res (list pivot))
          (setq pivot (list x)))
        ;; else
        (nconc pivot (list x))))
      (nconc res (list pivot))
      (cdr res))))


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


(defun >>=alist-update (target key value)
  "Update TARGET association list with the given KEY VALUE pair.
This function alters TARGET unless it is null."
  (if-let ((cell (assq key target)))
    (setcdr cell value)
    ;; else
    (setq target (nconc target `((,key . ,value)))))
  target)


(defun >>=alist-add-item (target key item)
  "Add ITEM to the list `cdr' of the cell mapped by KEY in TARGET.
This function alters TARGET unless it is null."
  (if-let ((cell (assq key target)))
    (setcdr cell (nconc (cdr cell) `(,item)))
    ;; else
    (setq target (nconc target `((,key . (,item))))))
  target)


(defmacro >>=alist-pop (source key &optional default)
  "Get the element of SOURCE whose `car' equals KEY and return its `cdr'.
The found cell is deleted.  If KEY is not found, return DEFAULT.  Because this
is a macro, argument SOURCE must be a variable symbol."
  `(let ((>>--result-- (assq ,key ,source)))
     (if >>--result--
       (prog1
         (cdr >>--result--)
         (setq ,source (assq-delete-all ,key ,source)))
       ;; else
       ,default)))



;;; misc utils

(defsubst >>=str2lisp (string)
  "Read one Lisp expression which is represented as text by STRING.
The differences with the `read-from-string' function are that STRING is
trimmed, only the recognized object is returned, and an error is issued if the
entire string is not recognized."
  (when-let ((trimmed (>>=str-trim string)))
    (let ((pair (read-from-string trimmed)))
      (if (= (cdr pair) (length trimmed))
        (car pair)
        ;; else
        (error
          ">>= invalid syntax error parsing '%s' string at index %s"
          trimmed (cdr pair))))))


(defsubst >>=declare-argument-obsolete (function obsolete when)
  "Declare using an argument as OBSOLETE in a FUNCTION call.
WHEN should be a string indicating when the argument was first made obsolete,
for example a date or a release number."
  (>>=warn
    "`%s' is an obsolete %s argument (as of %s)"
    obsolete function when))


(defmacro >>=check-obsolete-variable (obsolete current when &optional info)
  "Check if an OBSOLETE variable is being used.
When OBSOLETE is bound, a warning is issued indicating that CURRENT should be
used instead.

CURRENT is usually a SYMBOL that represents the new variable that will receive
the value of the OBSOLETE.  In case CURRENT involves more complex definitions,
it must be a LISP form (`this' can be used as an alias for the OBSOLETE value
in this body).  The optional INFO argument can be used in place of CURRENT in
the `use instead' message.

WHEN should be a string indicating when the variable was first made obsolete,
for example a date or a release number.

This declaration must be evaluated before the definition of the CURRENT
variable(s)."
  (unless (>>=real-symbol current)
    (setq current (macroexpand current)))
  (unless info
    (if (>>=real-symbol current)
      (setq info (symbol-name current))
      ;; else
      (error
        (concat
          ">>= missing `info' argument for obsolete variable `%s' definition "
          "for a non-symbol `current' argument")
        obsolete)))
  (let* ((msg "`%s' is an obsolete variable (as of %s); use %s instead")
         (sexps `((>>=warn ,msg ',obsolete ',when ',info))))
    (setq sexps
      (cons
        (if (>>=real-symbol current)
          `(set-default-toplevel-value ',current ,obsolete)
          ;; else
          `(let ((this ,obsolete)) ,current))
        sexps))
    (setq sexps
      (cons `(make-obsolete-variable ',obsolete ',info ',when)
        sexps))
    (when (>>=real-symbol current)
      (setq sexps
        (cons
          `(dolist (p '(saved-value saved-variable-comment))
             (when-let ((v (unless (get ',current p) (get ',obsolete p))))
               (put ',current p v)))
        sexps)))
    `(when (boundp ',obsolete)
       ,@sexps)))


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
  (when (>>=real-symbol mode)
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

(defconst >>=!path/separator
  "Character used by the operating system to separate pathname components."
  (substring (file-name-as-directory "x") 1))


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


(defun >>=read-initial-directory (&optional id)
  "Read the value of an initial directory to be used as an argument.
A service ID could be given to conform the prompt argument to call the
function `read-directory-name'."
  (read-directory-name
    (concat ">>= " (if id (format "%s in" id) "initial") " directory: ")
    nil nil 'must-match))



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


;; TODO: remove this
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

(defun >>=command-to-process-name (command)
  "Return process name from a given COMMAND string.
The COMMAND should be in the format '[VAR=VALUE...] program [args...]'.  The
result prioritizes the program part."
  (when-let ((process-name
               (>>=list/find
                 (lambda (part) (not (string-match "=" part)))
                 (split-string-shell-command command))))
    (let* ((res (file-name-base process-name))
           (counter (>>=key/counter res)))
      (if (= 0 counter) res (format "%s-%s" res counter)))))


(defun >>=command/get-name (command &optional full)
  "Extract the executable name from a COMMAND string.
If FULL is nil, return only the file name part sans its directory."
  (let ((res (>>=prefix-beffore "[[:space:]]" command)))
    (if full res (file-name-nondirectory res))))


(define-obsolete-function-alias '>>=executable-find '>>=command/find "0.11.5")
(defun >>=command/find (&rest options)
  "Search first valid command using the function `executable-find'.
A set of OPTIONS is searched until a valid one is found.  Any item could be a
symbol, a string, or a `cons' like `(command . args)', nil items are just
discarded."
  (let (res)
    (while (and options (null res))
      (when-let ((item (car options)))
        (let ((head (car-safe item)))
          (when-let ((aux (executable-find (>>=as-string (or head item)))))
            (setq res (if head (cons aux (cdr item)) aux)))))
      (setq options (cdr options)))
    res))


(defun >>=command/check (command)
  "Check if a system COMMAND is installed.
Intended to find out if a feature that depends on the given command can be
configured."
  ;; See `use-package-ensure-system-package' fo a more elaborated solution.
  (when command
    (or
      (executable-find command)
      (when init-file-debug
        (>>=warn "'%s' command is not installed" command)
        nil))))


(defun >>=shell-command-to-string (command)
  "Execute shell COMMAND and return its output as a trimmed string."
  (string-trim (shell-command-to-string command)))


(defun >>=process/safe-lines (program &rest args)
  "Execute PROGRAM with ARGS, returning its output as a list of lines.
Returns nil, if an error is signaled."
  (when-let ((executable (>>=command/find program)))
    (when (and (eq (length args) 1) (listp (car args)))
      (setq args (car args)))
    (condition-case nil
      (apply 'process-lines executable args)
      (error nil))))



;;; Lisp configuration files

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
  "Convert KEY to the internal Emacs representation."
  (condition-case nil
    (>>=key-parse key)
    (error key)))


(define-obsolete-function-alias '>>=normalize-function '>>=function/normalize
  "0.11.5")
(defsubst >>=function/normalize (value)
  "Normalize VALUE as a function (useful for for macro expansion)."
  (let ((res (>>=cast-function value)))
    (if (>>=real-symbol res) (list 'quote res) res)))


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
      (\"s-?\" . (identity add-function))))

\(fn [KEY COMMAND]...)"
  (macroexp-progn
    (mapcar
      (lambda (pair)
        `(>>=bind-global-key
           ,(>>=key-normalize (car pair))
           ,(>>=function/normalize (cdr pair))))
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
