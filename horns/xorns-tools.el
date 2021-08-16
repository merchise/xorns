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
(eval-when-compile (require 'cl-lib))
(require 'project)


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


(defsubst >>=check-function (value &optional strict)
  "Check if VALUE is an existing function.
When STRICT is not nil and VALUE is not a function, an error is issued."
  (if (functionp value)
    value
    ;; else
    (when strict
      (error ">>= wrong%s function '%s'"
        (if (eq strict t) "" (format " %s" strict)) value))))


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



;;; string - symbol conversion

(defun >>=prefix (string size)
  "Return the STRING prefix of SIZE characters."
  (if (> size 0)
    (if (> size (length string)) string
      ;; else
      (substring string 0 size))
    ;; else
    ""))


(defun >>=suffix (string size)
  "Return the STRING prefix of SIZE characters."
  (if (> size 0)
    (if (> size (length string))
      string
      ;; else
      (substring string (- size)))
    ;; else
    ""))


(defsubst >>=str (value &optional strict)
  "Return a string only if VALUE is a symbol or a string.
When STRICT is given, and this function fails, an error is issued, otherwise
nil is returned."
  (or
    (if (stringp value)
      value
      ;; else
      (when (and (symbolp value) (not (booleanp value)))
        (symbol-name value)))
    (when strict
      (error ">>= %s'%s' can not be converted to a string"
        (if (eq strict t) "" (format "%s " strict)) value))))


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
  ;; Adapted from `map--plist-delete'
  (let ((tail target) last)
    (when keys
      (while (consp tail)
        (cond
          ((not (memq (car tail) keys))
            (setq
              last tail
              tail (cddr last)))
          (last
            (setq tail (cddr tail))
            (setf (cddr last) tail))
          (t
            (cl-assert (eq tail target))
            (setq
              target (cddr target)
              tail target)))))
    target))


(defun >>=plist-remove (target &rest keys)
  "Return a copy of a TARGET property-list with all KEYS removed."
  (if (apply '>>=plist-find-any target keys)
    (apply '>>=plist-delete (copy-sequence target) keys)
    ;; else
    target))


(define-obsolete-function-alias '>>=plist-exclude '>>=plist-remove
  "xorns 1.0" "Remove all KEYS from a TARGET property-list.")


(defun >>=plist-update (target &rest source)
  "Update TARGET from a SOURCE property-list."
  (setq source (>>=fix-rest-list source))
  (>>=plist-do (key value source target)
    (plist-put target key value)))


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
  "Split list XS into a `cons' of two lists '(HEAD . TAIL)'.

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


(defun >>=plist-setdefault (target key &optional default)
  "Insert KEY with a value of DEFAULT if KEY is not member of TARGET.
Return the value for KEY if it is in TARGET, else DEFAULT."
  (let ((res (plist-member target key)))
    (if res
      (nth 1 res)
      ;; else
      (plist-put target key default)
      default)))


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
    (>>=plist-do (key value defaults)
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



;;; value verifiers

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


;; TODO: deprecate this function
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
    (while (and options (null res))
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
  (purecopy (>>=canonical-directory-name (or (getenv "HOME") "~")))
  "Home directory.")


(defvar >>=|preferred-default-directory
  (>>=find-dir
    (>>=canonical-directory-name (getenv "WORKSPACE"))
    (>>=dir-join >>=!home-dir "work" "src")
    (>>=dir-join >>=!home-dir "work")
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


(defun >>=kill-buffer-and-window (&optional buffer)
  "Kill the specified BUFFER, and delete the window currently displaying it.
Argument nil or omitted means kill the current buffer."
  (setq buffer (or buffer (current-buffer)))
  (let ((win (get-buffer-window buffer)))
    (prog1
      (kill-buffer buffer)
      (when win
        (ignore-errors
          (delete-window win))))))



;;; modes
;; TODO: refactor all this section

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



;;; projects

(defvar >>-project/variables '()
  "Association list mapping project variables.")


(defun >>=project-root (&optional dir)
  "Retrieves the root directory of a project if available.
If DIR is not supplied its set to the current directory by default."
  "Return the project instance in DIR or `default-directory'."
  (>>=canonical-directory-name
    (if (functionp 'projectile-project-root)
      (funcall 'projectile-project-root dir)
      ;; else
      (when-let (project (project-current nil dir))
        (car (project-roots project))))))


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


(provide 'xorns-tools)
;;; xorns-tools.el ends here
