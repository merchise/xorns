;;; xorns-keywords.el --- Processing of keyword arguments

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Manipulate keyword style arguments using some ideas from module
;; `use-package'.


;; Enjoy!


;;; Code:


(require 'xorns-tools)


(defconst >>-required ::required::
  "Marker that a required value is expected.")


(defun >>=kw-get (key keywords &optional default)
  "Find first element of KEYWORDS whose `car' equals KEY and return its `cdr'.

Equality with KEY is always tested by `eq'.  Similarly to `alist-get', If KEY
is not found, return DEFAULT, unless `>>-required' is used in which case an
`error' is thrown."
  (let ((res (assq key keywords)))
    (if res
      (cdr res)
      ;; else
      (if (eq default >>-required)
	(error ">>= Key '%s' is required" key)
	;; else
	default))))


(defun >>=keywords->alist (&rest keywords)
  "Convert a sequence of KEYWORDS into an association-list."
  (setq keywords (>>=cast-list keywords))
  (let (res key)
    (while (setq key (car keywords))
      (if (symbolp key)
	(setq keywords (cdr keywords))
	;; else
	(error ">>= key '%s' must be a symbol, not %s" key (type-of key)))
      (when (assq key keywords)
	(error ">>= repeated key '%s'" key))
      (push (cons key (pop keywords)) res))
    (nreverse res)))


(defun >>=normalize-alist (prefix base &rest defaults)
  "Normalize a BASE association-list'.

BASE is updated with all the values in DEFAULTS not yet included.  Then, each
VALUE is normalized with up to two possible functions using the `eval'
protocol: VALUE is used as the main argument and BASE as the LEXICAL
environment.  Function `eval' is applied when the VALUE is defined using the
form '(:eval <lisp-expression>)', and also a function whose name is
\('<PREFIX>-normalize/<KEY>') when it is defined."
  (setq prefix (format "%s-normalize/" (or prefix ">>")))
  (if defaults
    (setq base
      (append base
	(delq nil
	  (mapcar
	    (lambda (pair) (unless (assq (car pair) base) pair))
	    (apply '>>=keywords->alist defaults))))))
  (mapc
    (lambda (pair)
      (let ((key (car pair))
	    (value (cdr pair))
	    changed)
	(when (and (listp value) (eq :eval (car value)))
	  (setq
	    value (eval (cadr value) base)
	    changed t))
	(let ((check (intern-soft (format "%s%s" prefix key))))
	  (when (functionp check)
	    (setq
	      value (funcall check value base)
	      changed t)))
	(when changed
	  (setcdr pair value))))
    base)
  base)


(provide 'xorns-keywords)
;;; xorns-keywords.el ends here
