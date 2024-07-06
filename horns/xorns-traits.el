;;; xorns-traits.el --- Customize system configuration units -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module allows you to customize the dependencies for the system
;; configuration.  The term trait is used to name a unit of configuration of a
;; feature or package.  See `>>=trait' macro on how to declare traits.
;;
;; Enabled traits are triggered (together with their dependencies) after the
;; Emacs session is initialized (the default), or after the first time
;; entering a `major-mode', or immediately after they are defined.
;;
;; Note that traits are not intended to replace `use-package' (that tool is
;; great), but rather are a complement to organize when each definition should
;; be run.

;; Enjoy!


;;; Code:

(eval-and-compile
  (require 'use-package)
  (require 'xorns-tools))


(defconst >>-!trait/format-string ">>-|%s"
  "String used to format a trait symbol using its name.")


(defsubst >>-trait/internal-symbol (trait)
  "Get the symbol for a TRAIT name."
  (intern (format >>-!trait/format-string trait)))


(defsubst >>-trait/bound-symbol (trait)
  "Get the symbol for a TRAIT name, or nil if undefined."
  (intern-soft (format >>-!trait/format-string trait)))


(defsubst >>-trait/safe-symbol (trait)
  "Get the symbol for a TRAIT name checking that it is a symbol."
  (if (>>=real-symbol trait)
    (>>-trait/internal-symbol trait)
    ;; else
    (error ">>= wrong trait value '%s', must be a symbol" trait)))


(defsubst >>-trait/error (trait message &rest args)
  "Signal a TRAIT error by passing MESSAGE and ARGS to `error'."
  (apply 'error (concat (format ">>= trait '%s' - " trait) message) args))


(defsubst >>-trait/repeated-keyword (trait keyword)
  "Signal an error if a KEYWORD repeated in a TRAIT definition."
  (>>-trait/error trait "value of %s is repeated" keyword))


(defsubst >>-trait/value-error (trait keyword type &optional value)
  "Signal a KEYWORD VALUE error expecting a TYPE in a TRAIT definition."
  (let ((msg (format "invalid keyword '%s', '%s' expected" keyword type)))
    (when value
      (setq msg (format "%s, not '%s'" msg value)))
  (>>-trait/error trait msg)))


(defsubst >>-trait/mutex-error (trait keyword cached)
  "Signal KEYWORD is mutually exclusive with CACHED in a TRAIT definition."
  (let ((aux (car cached)))
    (if (eq keyword aux)
    (>>-trait/repeated-keyword trait keyword)
    ;; else
    (>>-trait/error trait
      "%s keyword is mutually exclusive with %s" keyword aux))))


(defsubst >>-trait/kvp (trait body)
  "Return the first (KEYWORD, VALUE) pair from the TRAIT definition BODY."
  (if-let ((key (>>=keywordp (car body))))
    (if (length> body 1)
      key
      ;; else
      (>>-trait/error trait "keyword %s given without a value" key))))


(defsubst >>-trait/normalize-mode (value)
  "Normalize a VALUE as a `major-mode' symbol."
  (let ((aux (>>=as-string value))
        (suffix "-mode"))
    (if (>>=suffix-equal aux suffix)
      value
      ;; else
      (intern (concat aux suffix)))))


;; TODO: Check if this function can be generalized in 'tools' module
(defsubst >>-trait/initial-value? (value)
  "Return a normalized VALUE suitable for initial-value argument."
  (cond
    ((null value)
      '(identity nil))
    ((and (atom value) (not (keywordp value)) (not (stringp value)))
      value)
    ((eq (car-safe value) 'quote)
      (let ((aux (cadr value)))
        (if (or (keywordp aux) (stringp aux)) aux value)))
    ((eq (car-safe value) 'function)
      value)
    ((memq (car-safe value) '(lambda closure identity))
      value)))


(defun >>-trait/parse-name (name)
  "Parse a trait NAME and return a (BASE-NAME OPERATOR VALUE) list.
The NAME syntax is `BASE-NAME[OPERATOR[VALUE]]'."
  (setq name (>>=as-string name))
  (catch 'found
    (dolist (op '(!= = : !))
      (when-let ((pair (>>=bisect-string name (symbol-name op))))
        (let ((base (car pair))
              (value (cdr pair))
              keyword)
          (if value
            (progn
              (setq value (>>=str2lisp value))
              (if (booleanp value)
                (>>-trait/error name
                  "explicit boolean values are not allowed in operators")
                ;; else
                (pcase op
                  ('=
                    (setq keyword :if-equal))
                  ('!=
                    (setq keyword :if-not-equal))
                  (':
                    (setq keyword :if-trait-enabled))
                  ('!
                    (setq keyword :if-trait-not-enabled))
                  (_
                    (>>-trait/error name
                      "invalid operator '%s' with value '%s'" op value)))))
            ;; else
            (if (eq op ':)
              (setq
                keyword :immutable
                value t)
              ;; else
              (>>-trait/error name
                "no empty value allowed for operator '%s'" op)))
          (throw 'found (list base keyword value)))))
      ;; default
    (list name)))


(defun >>-trait/register-mode (mode trigger)
  "Register a trait TRIGGER to run when entering a major MODE."
  (let ((hook (intern (format "%s-hook" mode)))
        after)
    (setq after
      (lambda ()
        (remove-hook hook trigger)
        (advice-remove trigger after)))
    (advice-add trigger :after after)
    (add-hook hook trigger)))


(defmacro >>=trait/check-obsolete (obsolete trait when)
  "Check if an OBSOLETE variable is being used instead of a TRAIT.
This uses `>>=check-obsolete-variable' internally, the WHEN argument has the
same meaning."
  (let ((info (format "trait `%s'" trait))
        (symbol (>>-trait/internal-symbol trait)))
    `(>>=check-obsolete-variable ,obsolete ,symbol ,when ,info)))


(defun >>-trait/get-value-sexp (trait)
  "Internal function to get the expression of a TRAIT atomic value."
  (let ((var (>>-trait/internal-symbol trait)))
    `(if (boundp (quote ,var)) ,var t)))


(defmacro >>=trait? (trait)
  "Check if a TRAIT is enabled check its super-domains.
Not defined traits are enabled by default."
  (if-let ((parent-domains (cdr (reverse (>>=split-domains trait)))))
    `(and
       ,@(mapcar #'>>-trait/get-value-sexp parent-domains)
       ,(>>-trait/get-value-sexp trait))
    ;; else
    (>>-trait/get-value-sexp trait)))


(defmacro >>=trait/bound (trait)
  "Return the value of a TRAIT if it is defined, else nil."
  `(bound-and-true-p ,(>>-trait/internal-symbol trait)))


(defmacro >>=trait/set (&rest pairs)
  "Set each trait in PAIRS to its value.
Traits do not need to be defined because this is the way to configure them
at system initialization.

\(fn [TRAIT VALUE]...)"
  (if (cl-evenp (length pairs))
    (let (sexps)
      (>>=plist-do (trait value pairs)
        (setq sexps
          (cons
            `(set-default-toplevel-value
               ',(>>-trait/safe-symbol trait)
               ,value)
            sexps)))
      (macroexp-progn
        (reverse sexps)))
    ;; else
    (signal 'wrong-number-of-arguments `(>>=trait/set odd ,(length pairs)))))


(defmacro >>=trait (name &rest body)
  "Define NAME as a new trait.
A trait is composed of a control variable and/or an optional execution BODY.
The variable is named using the trait name prefixed with \">>-|\".  In some
contexts a function is necessary to encapsulate the execution body, in these
cases the same name is used for it.

Optional extra arguments can contain four concepts: a INITIAL-VALUE, the
DOCUMENTATION string, any number of [KEYWORD VALUE] pairs, and the execution
BODY.

The INITIAL-VALUE argument has the same semantics as the `:initial-value'
keyword but only atomic or quoted definitions can be specified.

The following keys are accepted:

:initial-value VALUE
    The initial value of the control variable.  Traits are enabled by default.
    Using nil disables the trait.  Use the `>>=trait/set macro within the
    `>>=settings/init' function to change the default value during the Emacs
    initialization process.  This parameter can be explicitly given as the
    second argument.

:after-load FILE
    By default, traits are executed immediately after they are defined.  This
    behavior can be changed using this keyword or one of those defined below.
    The trait runs immediately after the FILE is loaded.  The value can be a
    symbol or a string as defined in the `eval-after-load' function.  Special
    values `emacs' and `init' can be used to run the trait immediately after
    initializing the Emacs session.

:entering-mode SYMBOL
    The trait runs when entering a `major-mode' for the first time.

:after-delay NUMBER
    The trait runs after a delay of the specified seconds.

A trait can be an extension to a `use-package' definition, however keywords
for traits must be specified before those for `use-package'.

\(fn NAME [INITIAL-VALUE] [DOCUMENTATION] [KEYWORD VALUE]... &rest BODY])"
  (declare (doc-string 3) (indent 2))
  (let ((symbol (>>-trait/internal-symbol name))
        (doc nil)
        (initial-value t)
        (defer nil)
        (sexps nil))
    (let ((extra-body-head nil))
      (when-let ((aux (when body (>>-trait/initial-value? (car body)))))
        (when (eq (car-safe aux) 'identity)
          (setq aux (cadr aux)))
        (setq
          extra-body-head (list :initial-value aux)
          body (cdr body)))
      (when (stringp (car body))
        (setq doc (pop body)))
      (when extra-body-head
        (setq body (nconc extra-body-head body))))
    ;; keywords
    (catch 'done
      (while-let ((key (>>-trait/kvp name body)))
        (let ((value (nth 1 body)))
          (pcase key
            (:initial-value
              (cond
                ((not (eq initial-value t))
                  (>>-trait/repeated-keyword name key))
                ((eq value t)
                  (>>-trait/error name "keyword %s is t by default" key))
                (t
                  (setq initial-value value))))
            (:after-load
              (cond
                (defer
                  (>>-trait/mutex-error name key defer))
                ((>>=non-empty-string-or-symbol value)
                  (setq defer (cons key value)))
                (t
                  (>>-trait/value-error name key "symbol or string" value))))
            (:entering-mode
              (cond
                (defer
                  (>>-trait/mutex-error name key defer))
                ((>>=real-symbol value)
                  (setq
                    defer (cons key (>>-trait/normalize-mode value))))
                (t
                  (>>-trait/value-error name key "symbol" value))))
            (:after-delay
              (cond
                (defer
                  (>>-trait/mutex-error name key defer))
                ((and (numberp value) (< 0 value 600.001))
                  (setq
                    defer (cons key value)))
                (t
                  (>>-trait/value-error name key
                    "number of seconds up to 10 minutes" value))))
            (_
              (setq body `((use-package ,name ,@body)))
              (throw 'done nil))))
        (setq body (nthcdr 2 body))))
    (when (and defer (not body))
      (>>-trait/value-error name (car defer) "non-empty body"))
    (unless doc
      (setq doc
        (format "Trait \"%s\" %s." name (if body "configuration" "flag"))))
    (when body
      (setq sexps
        (if (not defer)
          `((if ,symbol ,(macroexp-progn body)))
          ;; else
          (list
            `(defun ,symbol () ,doc ,@body)
            `(if ,symbol
               ,(pcase (car defer)
                  (:after-load
                    (let ((file (cdr defer)))
                      (if (memq file '(init emacs))
                        `(if after-init-time
                           (,symbol)
                           ;; else
                           (add-hook 'after-init-hook ',symbol))
                        ;; else
                        `(eval-after-load ',file '(,symbol)))))
                  (:entering-mode
                    `(>>-trait/register-mode ',(cdr defer) ',symbol))
                  (:after-delay
                    `(run-with-timer ,(cdr defer) nil ',symbol))))))))

    `(prog1
       (defvar ,symbol ,initial-value ,doc)
       (put ',symbol 'standard-value ,initial-value)
       ,@sexps)))


(provide 'xorns-traits)
;;; xorns-traits.el ends here
