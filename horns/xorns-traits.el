;;; xorns-traits.el --- Customize system configuration units -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Traits are designed to extend the customization of Emacs initialization.
;; The term "trait" is used to name a unit of configuration of a feature or
;; package.  Use the `>>=trait' macro to declare traits.
;;
;; For example:
;;
;;   (>>=trait saveplace
;;     (save-place-mode +1))
;;
;; The main extensions are:
;;
;;   - A virtual system to enable/disable traits.  The condition to determine
;;     whether a trait is enabled is implemented with "virtual" control
;;     variables.  To disable a trait use the `>>=trait/set' macro using a nil
;;     value:
;;
;;       (>>=trait/set saveplace nil)
;;
;;   - Alternatives for when to run traits.  They can be triggered either when
;;     they are defined, or after the Emacs session is initialized, or after
;;     the first time a `major-mode' is entered.
;;
;; Traits can be used to extend a `use-package' definition, this is done by
;; using a non-valid trait keyword assuming that it is valid for use package.
;; For example:
;;
;;   (>>=trait flycheck
;;     :ensure t
;;     :init
;;     (global-flycheck-mode))
;;
;; is equivalent to:
;;
;;   (when (>>=trait? flycheck)
;;     (use-package flycheck
;;       :ensure t
;;       :init
;;       (global-flycheck-mode)))
;;
;; The assignment of the control variable of a trait must be done before
;; defining it, usually in the Emacs initialization process.
;;
;; A trait can be disabled by default, this is done by passing nil as a
;; parameter of the `>>=trait' macro or by using the `:default-value' keyword:
;;
;;   (>>=trait lsp-pyright
;;     :default-value nil
;;     :ensure t)

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


(defsubst >>-trait/use-package-symbol (trait)
  "Get the symbol to use in a `use-package' definition from a TRAIT name."
  (intern (car (last (>>=split trait ".")))))


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
  "Set each TRAIT to its VALUE as defined in PAIRS.
The primary intent of this macro is to toggle a trait's condition between
enabled and disabled.  It should be used before load the module where the
trait is defined, see Info node `Init File'.

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


(defalias '>>-trait/is= 'value=)
(defalias '>>-trait/is/= 'value/=)
(defalias '>>-trait/is< 'value<)
(defalias '>>-trait/is<= 'value<=)
(defalias '>>-trait/is> 'value>)
(defalias '>>-trait/is>= 'value>=)


(defsubst >>-trait/is| (one two)
  "Intersection operator.
Returns whether the intersection of the sets ONE and TWO is nonempty.  If any
of the values ​​is not a list, it is considered to be the set containing that
single element."
  (or
    (value= one two)
    (let ((aux (proper-list-p two)))
      (if (proper-list-p one)
        (if aux
          (seq-intersection one two 'value=)
          ;; else
          (value-in two one))
        ;; else
        (when aux
          (value-in one two))))))


(defsubst >>-trait/is^ (one two)
  "Extended intersection operator.
Return non-nil when the intersection operator '|' is validated on arguments
ONE and TWO, but it is also validated when ONE (the current value of the
trait) is the literal boolean t so that the operand TWO is considered as the
default value."
  (or
    (eq one t)
    (>>-trait/is| one two)))


(defsubst >>-trait/error (trait message &rest args)
  "Signal a TRAIT error by passing MESSAGE and ARGS to `error'."
  (apply 'error (concat (format ">>= trait '%s' - " trait) message) args))


(defsubst >>-trait/operator (value)
  "Check if a VALUE is a valid trait operator."
  (when (memq value '(= | ^ /= < <= > >= :))
    value))


(defsubst >>-trait/operator-pair (trait body)
  "Return the head operator/value pair from a TRAIT BODY definition."
  (when-let ((op (>>-trait/operator (car body))))
    (if (length> body 1)
      (let ((value (nth 1 body)))
        (if (not (booleanp value))
          (cons op value)
          ;; else
          (>>-trait/error trait
            "operator '%s' uses an invalid literal boolean value" op)))
      ;; else
      (>>-trait/error trait "operator '%s' given without a value" op))))


(defsubst >>-trait/keyword-pair (trait body)
  "Return the head keyword/value pair from a TRAIT BODY definition."
  (when-let ((key (>>=keywordp (car body))))
    (if (length> body 1)
      (cons key (nth 1 body))
      ;; else
      (>>-trait/error trait "keyword %s given without a value" key))))


(defsubst >>-trait/inline-default-value (body)
  "Check for a valid inline default value in BODY."
  (when-let ((value (>>=car body)))
    (and
      (not (keywordp value))
      (or (atom value) (>>=quoted value) (keywordp (nth 1 body))))))


(defsubst >>-trait/identifier (base operator value)
  "Get a new identifier from a BASE symbol, an OPERATOR and a VALUE."
  (intern (format "%s%s%s" base operator (>>=identifier value 'unique))))


(defmacro >>-trait/check-keyword (target type-info checker)
  "Internal tool to be used only for `>>=trait' macro.
Argument TARGET is the variable bind to store the value, TYPE-INFO should be a
string explaining the expected type, CHECKER is a expression to check if the
value is valid or not."
  `(if ,target
     (let ((duplicated (or (car-safe ,target) keyword)))
       (if (eq keyword duplicated)
         (>>-trait/error name "%s keyword is repeated" keyword)
         ;; else
         (>>-trait/error name
           "%s is mutually exclusive with %s" keyword duplicated)))
     ;; else
     (if-let ((aux ,checker))
       (setq ,target (cons keyword (>>=unwrap-nil aux)))
       ;; else
       (>>-trait/error name
         "invalid '%s' value for %s keyword, expecting '%s'"
         value keyword ,type-info))))


(defsubst >>-trait/normalize-mode (value)
  "Normalize a VALUE as a `major-mode' symbol."
  (let ((aux (>>=as-string value))
        (suffix "-mode"))
    (if (>>=suffix-equal aux suffix)
      value
      ;; else
      (intern (concat aux suffix)))))


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


(defmacro >>=trait (name &rest body)
  "Define NAME as a new trait.
A trait declares a unit of configuration code along with a condition to
execute it only when enabled.

Argument NAME must is the identity of the trait.  The name can be
multi-domain, for example `python.blacken', which allows traits to evaluate
conditions hierarchically: a trait is only enabled when all its super-domains
are enabled.

BODY can contain several concepts: a DEFAULT-VALUE or a CONDITION, any number
of [KEYWORD VALUE] pairs, and the actual BODY with the code to execute when
the trait is enabled.

Actually, the VALUE bound to a trait is a special case of a CONDITION (the
trait is enabled if its value is non-null).  The DEFAULT-VALUE argument has
the same semantics as the `:default-value' keyword but only atomic values can
be specified.  An explicit CONDITION is defined by a pair of an OPERATOR and
an OPERAND (the trait is enabled if OPERAND is true).

The following binary operators that compare in standard order the value of
current trait with a given operand are accepted: '=', '<', '<=', '>', and
'>='.  These operators are validated with the function using the format
pattern '>>-trait/is[OPERATOR]', for example see `>>-trait/is='.

There are other binary operators that are more specific than the above:


'|'
    The current value of the trait and the operand are considered sets.  It is
    validated when the intersection of the two sets is nonempty.  If any of
    the values ​​is not a list, it is considered to be the set containing that
    single element.  It is executed using `>>-trait/is|'.

    This operator can be used with several semantics.  The two main ones
    are, (1) the trait is configured with several options so that it is
    validated in any of these cases, or (2) the configured value of the trait
    is one of the options given as the operand value.

'^'
    True if the intersection operator ’|’ is validated or when the current
    value of the trait is the literal boolean t, allowing a default
    option (usually a symbol).  It is executed with `>>-trait/is^'.

The unary operator ':' is used to specify a Lisp expression that is evaluated
using the symbol `this' to refer to the current value of the trait.  A plain
name (a symbol) can also be specified meaning that the given trait must be
enabled.

The following keywords are accepted:

:default-value VALUE
    The default value of a trait.  Use the `>>=trait/set' macro to change the
    value of a trait during the Emacs initialization process.

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

When a trait is used as an extension of `use-package', the package name is the
last subdomain of the trait name unless one of the '=', '|', '^' or ':'
operators uses a symbol as an operand, in which case it is used to determine
the package name.

See the main module documentation for more information.

\(fn NAME [DEFAULT-VALUE|CONDITION] [KEYWORD VALUE]... &rest BODY])"
  (declare (indent 2))
  (let (condition trigger aux-name)
    ;; operator, default-value
    (if-let ((pair (>>-trait/operator-pair name body)))
      (let* ((op (car pair))
             (value (cdr pair)))
        (setq body (nthcdr 2 body))
        (when (and (memq op '(= | ^ :)) (symbolp value))
          (setq aux-name value))
        (if (eq op :)    ;; unary
          (when (symbolp value)
            (setq value `(>>=trait? ,value)))
          ;; else        ;; binary
          (let ((fn (>>=function-intern ">>-trait/is%s" op)))
            (setq value `(,fn this ,(if (symbolp value) `',value value)))))
        (setq condition (cons op value)))
      ;; else
      (when (>>-trait/inline-default-value body)
        (setq body (cons :default-value body))))
    ;; parse keywords
    (catch 'done
      (while-let ((pair (>>-trait/keyword-pair name body)))
        (let ((keyword (car pair))
              (value (cdr pair)))
          (pcase keyword
            (:default-value
              (>>-trait/check-keyword condition "not literal boolean t"
                (unless (eq value t)
                  (>>=wrap-nil value))))
            (:after-load
              (>>-trait/check-keyword trigger "string or symbol"
                (>>=non-empty-string-or-symbol value)))
            (:entering-mode
              (>>-trait/check-keyword trigger "symbol (major-mode)"
                (when (>>=real-symbol value)
                  (>>-trait/normalize-mode value))))
            (:after-delay
              (>>-trait/check-keyword trigger
                "number of seconds up to 10 minutes"
                (when (and (numberp value) (< 0 value 600.001))
                  value)))
            (_
              (let ((pkg (>>-trait/use-package-symbol (or aux-name name))))
                (setq body `((use-package ,pkg ,@body)))
                (throw 'done nil)))))
        (setq body (nthcdr 2 body))))
    ;; Set definitions and generate code
    (let ((symbol (>>-trait/internal-symbol name))
          var-def cond-def code-def)
      (when condition
        (let ((key (car condition))
              (aux (cdr condition)))
          (if (eq key :default-value)
            (setq var-def
              `(defvar ,symbol ,aux
                 ,(format "Trait `%s' configuration variable." name)))
            ;; else
            (setq
              symbol (>>-trait/identifier symbol key (or aux-name aux))
              cond-def `,aux))))
      (when (and (or trigger cond-def) (not body))
        (>>-trait/error name
          "keyword %s is invalid with empty body" (car trigger)))
      (when body
        (setq code-def
          (if (null trigger)
            body
            ;; else
            (list
              `(defun ,symbol ()
                 ,(format "Trait `%s' function." name)
                 ,@body)
              `(declare-function ,symbol nil)  ;; WTF: avoid compiler warning
              (pcase (car trigger)
                (:after-load
                  (let ((file (cdr trigger)))
                    (if (memq file '(init emacs))
                      `(if after-init-time
                         (,symbol)
                         ;; else
                         (add-hook 'after-init-hook ',symbol))
                      ;; else
                      `(eval-after-load ',file '(,symbol)))))
                (:entering-mode
                  `(>>-trait/register-mode ',(cdr trigger) ',symbol))
                (:after-delay
                  `(run-with-timer ,(cdr trigger) nil ',symbol)))))))
      (>>=macroexp-progn
        var-def
        (when code-def
          `(let ((this (>>=trait? ,name)))
             (if ,(if cond-def `(and this ,cond-def) 'this)
               ,(>>=macroexp-progn code-def))))))))


(provide 'xorns-traits)
;;; xorns-traits.el ends here
