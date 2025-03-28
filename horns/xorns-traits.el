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


(defsubst >>-trait/symbol-name (trait)
  "Return the canonical symbol for a TRAIT."
  (format ">>-|%s" trait))


(defsubst >>-trait/intern (trait)
  "Return the canonical symbol for a TRAIT."
  (intern (>>-trait/symbol-name trait)))


(defsubst >>-trait/intern-soft (trait)
  "Return the canonical symbol for a TRAIT, or nil if none exists."
  (intern-soft (>>-trait/symbol-name trait)))


(defsubst >>-trait/new-function-identifier (trait)
  "Internal function to get a unique function identifier for a TRAIT."
  (>>=new-symbol (>>-trait/symbol-name trait)))


(defsubst >>-trait/variable-documentation (trait)
  "Get the documentation string for a TRAIT variable definition."
  (format "Configuration variable for `%s' trait." trait))


(defsubst >>-trait/function-documentation (trait)
  "Get the documentation string for a TRAIT function definition."
  (format "Execution body function for `%s' trait ." trait))


(defsubst >>-trait/safe-symbol (trait)
  "Get the symbol for a TRAIT name checking that it is a symbol."
  (if (>>=real-symbol trait)
    (>>-trait/intern trait)
    ;; else
    (error ">>= wrong trait value '%s', must be a symbol" trait)))


(defsubst >>-trait/use-package-symbol (trait)
  "Get the symbol to use in a `use-package' definition from a TRAIT name."
  (intern (car (last (>>=split trait ".")))))


(defun >>-trait/get-value-sexp (trait)
  "Internal function to get the expression of a TRAIT atomic value."
  (let ((var (>>-trait/intern trait)))
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
  `(bound-and-true-p ,(>>-trait/intern trait)))


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


(defsubst >>-trait/inline-default-value (body)
  "Check for a valid inline default value in BODY."
  (when-let ((value (>>=car body)))
    (and
      (not (keywordp value))
      (or (atom value) (>>=quoted value) (keywordp (nth 1 body))))))


(defsubst >>-trait/valid-atomic-value (value)
  "Return if the argument is a valid atomic VALUE for special operators."
  (unless (or (booleanp value) (keywordp value) (proper-list-p value))
    value))


(defsubst >>-trait/atomic-list (body)
  "Return a list of valid atomic values from BODY for special operators."
  (let (res)
    (while-let ((value (>>-trait/valid-atomic-value (car body))))
      (setq
        res (append res (list value))
        body (cdr body)))
    res))


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
     (if (length> body 1)
       (let ((value (nth 1 body)))
         (if-let ((aux ,checker))
           (setq ,target (cons keyword (>>=unwrap-nil aux)))
           ;; else
           (>>-trait/error name
             "invalid '%s' value for %s keyword, expecting '%s'"
             value keyword ,type-info)))
       ;; else
       (>>-trait/error name "keyword %s given without a value" keyword))))


(defsubst >>-trait/parse-sections (trait body &optional aux-name)
  "Normalize sections present in a TRAIT BODY definition.
The optional argument AUX-NAME is not-nil when one of the '=', '|', '^' or ':'
operators is used for the TRAIT condition with a symbol as the operand value."
  (when-let ((keyword (>>=keywordp (car body))))
    (unless (eq keyword :use)
      (let ((pkg (>>-trait/use-package-symbol (or aux-name trait))))
        (setq body `(:use ,pkg ,@body)))))
  (let ((res (list nil)))
    (dolist (section (>>=split-list :use body) (cdr res))
      (when (keywordp (car section))
        (if-let ((pkg (>>=nth 1 section)))
          (if (>>=real-symbol pkg)
            (progn
              (setcar section 'use-package)
              (setq section (list section)))
            ;; else
            (if (eq pkg >>-!wrapped-nil)
              (progn
                (setq section (nthcdr 2 section))
                (unless section
                  (>>-trait/error trait "empty anonymous section")))
              ;; else
              (>>-trait/error trait
                "section package-name must be a symbol or nil, not %s" pkg)))
          ;; else
          (>>-trait/error trait "empty section")))
      (nconc res section))))


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
        (symbol (>>-trait/intern trait)))
    `(>>=check-obsolete-variable ,obsolete ,symbol ,when ,info)))


(defmacro >>=trait (name &rest body)
  "Define NAME as a new trait.
Each trait essentially defines one or more configuration sections.  An
underlying condition determines whether to execute a trait.  You can also
define when a trait should execute if it is enabled.

Argument NAME must is the identity of the trait.  The name can be
multi-domain, for example `python.venv', which allows traits to evaluate
conditions hierarchically: a trait is only enabled when all its super-domains
are enabled.

BODY can contain several concepts: a DEFAULT VALUE or a CONDITION, any number
of [KEYWORD VALUE] pairs, and the different sections, each one containing the
code to be executed when the trait is enabled.

Actually, the VALUE bound to a trait is a special case of a CONDITION (the
trait is enabled if its value is non-null).  The DEFAULT-VALUE argument has
the same semantics as the `:default-value' keyword (see below) but only atomic
values can be specified.  An explicit CONDITION is defined by a [OPERATOR
OPERAND] pair (the trait is enabled if the operation is true).

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

Normally a trait is used as an extension of `use-package' but can be also used
as a simple configuration body by only specifying the default section without
any specific `use-package' definition.

The supported keyword types are:

- General: can only be specified before the other two types.

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

- Explicit section definition:

  :use PACKAGE

    The body of a trait can define several sections, each one usually
    declaring a `use-package' configuration.  This keyword introduces a new
    section.  If the PACKAGE name is nil, the section will be simple
    configuration body.

    If the first section does not have this keyword, its kind depends on the
    context: when a keyword is heading it, a `use-package' declaration is
    used, otherwise it is a simple configuration block.  For the first case,
    the package name is inferred from the trait name unless one of the '=',
    '|', '^' or ':' operators is used for the condition with a symbol as the
    operand value.

See the main module documentation for more information.

\(fn NAME [DEFAULT-VALUE|CONDITION] [KEYWORD VALUE]... &rest BODY])"
  (declare (indent 2))
  (let (condition trigger aux-name)
    ;; operator, default-value
    (if-let ((pair (>>-trait/operator-pair name body)))
      (let* ((op (car pair))
             (value (cdr pair)))
        (setq body (nthcdr 2 body))
        (when (memq op '(| ^))
          (when-let ((tail (>>-trait/atomic-list body)))
            (setq
              value `'(,value ,@tail)
              body (nthcdr (length tail) body))))
        (when (and (memq op '(= | ^ :)) (symbolp value))
          (setq aux-name value))
        (if (eq op :)    ;; unary
          (when (symbolp value)
            (setq value `(>>=trait? ,value)))
          ;; else        ;; binary
          (if-let ((fn (>>=function/search ">>-trait/is%s" op)))
            (setq value `(,fn this ,(if (symbolp value) `',value value)))))
        (setq condition (cons op value)))
      ;; else
      (when (>>-trait/inline-default-value body)
        (setq body (cons :default-value body))))
    ;; parse keywords
    (catch 'done
      (while-let ((keyword (>>=keywordp (car body))))
        ;; (value (nth 1 body)) for `>>-trait/check-keyword'
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
          (_    ;; `:use' or a `use-package' specific keyword
            (throw 'done nil)))
        (setq body (nthcdr 2 body))))
    ;; parse sections
    (when body
      (if-let ((keyword (>>=keywordp (car body))))
        (let ((pkg (>>-trait/use-package-symbol (or aux-name name))))
          (setq body `((use-package ,pkg ,@body)))
        ;; (unless (eq keyword :use)
        ;;   (let ((pkg (>>-trait/use-package-symbol (or aux-name name))))
        ;;     (setq body `(:use ,pkg ,@body))))
        ;;   ;; else
        ;;   (setq body `(:use nil ,@body))
        )))
    ;; Set definitions and generate code
    (let (var-def cond-def code-def)
      (when condition
        (let ((key (car condition))
              (aux (cdr condition)))
          (if (eq key :default-value)
            (let ((var-name (>>-trait/intern name)))
              (setq var-def
                `(defvar ,var-name ,aux
                   ,(>>-trait/variable-documentation name))))
            ;; else
            (setq cond-def `,aux))))
      (when (and (or trigger cond-def) (not body))
        (>>-trait/error name
          "keyword %s is invalid with empty body" (car trigger)))
      (when body
        (setq code-def
          (if (null trigger)
            body
            ;; else
            (let ((fun-name (>>-trait/new-function-identifier name)))
              (list
                `(defun ,fun-name ()
                   ,(>>-trait/function-documentation name)
                   ,@body)
                `(declare-function ,fun-name nil)  ;; WTF: compiler warning
                (pcase (car trigger)
                  (:after-load
                    (let ((file (cdr trigger)))
                      (if (memq file '(init emacs))
                        `(if after-init-time
                           (,fun-name)
                           ;; else
                           (add-hook 'after-init-hook ',fun-name))
                        ;; else
                        `(eval-after-load ',file '(,fun-name)))))
                  (:entering-mode
                    `(>>-trait/register-mode ',(cdr trigger) ',fun-name))
                  (:after-delay
                    `(run-with-timer ,(cdr trigger) nil ',fun-name))))))))
      (>>=macroexp-progn
        var-def
        (when code-def
          `(let ((this (>>=trait? ,name)))
             (if ,(if cond-def `(and this ,cond-def) 'this)
               ,(>>=macroexp-progn code-def))))))))


(provide 'xorns-traits)
;;; xorns-traits.el ends here
