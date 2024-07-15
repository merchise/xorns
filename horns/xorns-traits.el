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
;; parameter to the initial value of the `>>=trait' macro or by using the
;; `:initial-value' keyword:
;;
;;   (>>=trait lsp-pyright
;;     :initial-value nil
;;     :ensure t)

;;; Backlog:

;; - Define options to conditionally execute traits.
;;
;; - Define some aliases to main macros.
;;
;; - Create the possibility of `use-package' generation templates.
;;
;; - Check which `use-package' definitions can be converted to traits.

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
      (setq msg (format "%s, not %s '%s'" msg (type-of value) value)))
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


(defmacro >>=trait (name &rest body)
  "Define NAME as a new trait.
A trait declares a unit of configuration code along with a condition to
execute it only when enabled.

Argument NAME must be a symbol and define the identity of the trait.  The name
can be multidomain, for example `python.blacken', which allows traits to
evaluate conditions hierarchically: a trait is only enabled when all
super-domains are enabled.

The INITIAL-VALUE argument has the same semantics as the `:initial-value'
keyword but only atomic or quoted definitions can be specified.

Optional BODY extra argument can contain several concepts: a INITIAL-VALUE,
any number of [KEYWORD VALUE] pairs, and the execution code.

The following keywords are accepted:

:initial-value VALUE
    The initial value of the control variable.  Traits are enabled by default.
    Boolean value nil disables the trait.  Use the `>>=trait/set' macro to
    change the default value during the Emacs initialization process.  This
    parameter can be explicitly given as the second argument, it is not valid
    when an equality operator is given.

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

See the main module documentation for more information.

\(fn NAME [INITIAL-VALUE|CONDITION] [KEYWORD VALUE]... &rest BODY])"
  (declare (doc-string 3) (indent 2))
  (let ((symbol (>>-trait/internal-symbol name))
        (doc nil)
        (initial-value t)
        (defer nil)
        (sexps nil))
    ;; initial value and documentation string
    (let ((extra-body-head nil))
      (when-let ((aux (when body (>>-trait/initial-value? (car body)))))
        (when (eq (car-safe aux) 'identity)
          (setq aux (cadr aux)))
        (setq
          extra-body-head (list :initial-value aux)
          body (cdr body)))
      (when (stringp (car body))
        (>>=declare-argument-obsolete
          `(>>=trait ,name ...) 'documentation-string "0.11.3")
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
              (let ((pkg (>>-trait/use-package-symbol name)))
                (setq body `((use-package ,pkg ,@body)))
                (throw 'done nil)))))
        (setq body (nthcdr 2 body))))
    (when (and defer (not body))
      (>>-trait/value-error name (car defer) "non-empty body"))
    (unless doc
      (setq doc
        (format "Trait \"%s\" %s." name (if body "configuration" "flag"))))
    (when body
      (setq sexps
        (if (not defer)
          `((if (>>=trait? ,name) ,(macroexp-progn body)))
          ;; else
          (list
            `(defun ,symbol () ,doc ,@body)
            `(if (>>=trait? ,name)
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
    (when (not (eq initial-value t))
      (setq sexps (nconc `((defvar ,symbol ,initial-value ,doc)) sexps)))
    (macroexp-progn sexps)))


(provide 'xorns-traits)
;;; xorns-traits.el ends here
