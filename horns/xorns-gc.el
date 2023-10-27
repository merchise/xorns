;;; xorns-gc.el --- Garbage Collection Strategy  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Configure garbage collection strategy.  When Emacs initialization file is
;; loaded, the values of `gc-cons-threshold', and `gc-cons-percentage' are
;; increased to avoid garbage collection running during this process to
;; improve startup time.
;;
;; After startup process is finished, original values for `gc-cons-threshold'
;; and `gc-cons-percentage' variables will be restored, then selected strategy
;; in `>>=|gc/strategy' will be configured.

;; Enjoy!


;;; Code:

(require 'xorns-tools)
(require 'xorns-init)

(eval-and-compile
  (require 'use-package nil 'noerror))


(defconst >>=!gc/default-threshold-base #x4000000    ; 64M
  "Default value to be used when `>>=|gc/strategy' is t.")


(defvar >>=|gc/strategy nil
  "Configure Garbage Collection Strategy.

Could be an integer value, a `cons', one of the two canonical boolean values,
or any of the symbols `smart' or `magic'.

An integer value will be used as a base to calculate `gc-cons-threshold', see
`>>=gc/threshold-from-base' for the method to calculate the actual value of
`gc-cons-threshold' variable.

When a `cons' is given, its `car' is used as the former integer threshold
base, and its `cdr' is used to redefine `gc-cons-percentage' variable.

When one the symbols `smart', or `magic', is given, `gcmh-mode' will be
activated.

The canonical boolean value nil means no configuration at all, and t to use
the value defined in `>>=!gc/default-threshold-base'.")


(defun >>=gc/threshold-from-base (base)
  "Calculate `gc-cons-threshold' based on an integer BASE.
This value is duplicated if a graphic display is used, or a system with high
memory requirements is being used (like `exwm')."
  (* base
    (if (display-graphic-p) 2 1)
    (if (featurep 'exwm-input) 2 1)))


(defun >>-gc/strategy-configure ()
  "Configure garbage collection strategy after startup process is finished.
Restore original threshold and percentage values, call `garbage-collect', and
configure defined strategy in `>>=|gc/strategy' variable."
  (setq
    gc-cons-threshold (or (>>=get-original-value gc-cons-threshold) 800000)
    gc-cons-percentage (or (>>=get-original-value gc-cons-percentage) 0.1))
  (garbage-collect)
  (cond
    ((null >>=|gc/strategy)
      ;; nil? do nothing
      )
    ((booleanp >>=|gc/strategy)    ; t
      (setq gc-cons-threshold
        (>>=gc/threshold-from-base >>=!gc/default-threshold-base)))
    ((integerp >>=|gc/strategy)
      (setq
        gc-cons-threshold (>>=gc/threshold-from-base >>=|gc/strategy)))
    ((consp >>=|gc/strategy)
      (setq
        gc-cons-threshold (>>=gc/threshold-from-base (car >>=|gc/strategy))
        gc-cons-percentage (cdr >>=|gc/strategy)))
    ((and (symbolp >>=|gc/strategy) (memq >>=|gc/strategy '(smart magic)))
      (use-package gcmh
        :ensure t
        :config
        (declare-function gcmh-mode 'gcmh)    ; WTF
        (gcmh-mode +1)))
    (t
      (warn ">>= invalid `>>=|gc/strategy' value: %s" >>=|gc/strategy))))


(with-eval-after-load 'xorns-gc
  (if >>=xorns-initialized
    (>>-gc/strategy-configure)
    ;; else
    (warn ">>= `xorns-gc' must be configured after initialization process")))


(provide 'xorns-gc)
;;; xorns-gc.el ends here
