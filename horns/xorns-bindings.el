;;; xorns-bindings.el --- Key binding simple tools

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tools to manage key-bindings.  It will be taken into account if `exwm' is
;; active when assigning each command.

;; Enjoy!


;;; Code:

(require 'xorns-init)
(require 'xorns-tools)


(defsubst >>-kbd (key)
  "Convert KEY to its internal representation but first trying a string."
  (if (vectorp key) key (kbd key)))


(defsubst >>-command (command)
  "Check a COMMAND always recognizing the symbols as valid."
  (if (symbolp command) command (>>=cast-function command 'validate)))


(defsubst >>-key-pair (key command)
  "Validate a KEY/COMMAND and convert it to a `cons' tuple."
  (cons (>>-kbd key) (>>-command command)))


(defun >>=global-set-keys (&rest pairs)
  "Bind on the current global keymap [KEY COMMAND] PAIRS.
`exwm' is used when running Emacs as a window manager."
  (setq pairs
    ;; normalize key/command pairs
    (let (key)
      (prog1
	(delq nil
	  (mapcar
	    (lambda (aux)
	      (if key
		(prog1
		  (>>-key-pair key aux)
		  (setq key nil))
		;; else
		(if (consp aux)
		  (>>-key-pair (car aux) (cdr aux))
		  ;; else
		  (setq key aux)    ; use the key in the next iteration
		  nil)))
	    (>>=fix-rest-list pairs)))
	(when key
	  (error ">>= final key '%s' without command pair" key)))))
  (if (and >>=!emacs-as-wm (require 'exwm nil 'noerror))
    (funcall
      (if >>=xorns-initialized 'customize-set-variable 'customize-set-value)
      'exwm-input-global-keys
      (append (bound-and-true-p exwm-input-global-keys) pairs))
    ;; else
    (let ((map (current-global-map)))
      (dolist (pair pairs)
	(define-key map (car pair) (cdr pair)))))
  pairs)


(defmacro >>=remap (key command alt-key)
  "Give KEY a global binding as COMMAND.
In this case, KEY is a standard `key-binding', whose original command will
receive the alternative ALT-KEY binding.  See also `>>=remap*'"
  `(let ((kbind (>>-kbd ,key)))
     (>>=global-set-keys
       kbind ',command
       ,alt-key (key-binding kbind))))


(defmacro >>=remap* (key command alt-key)
  "Give KEY a global binding as COMMAND.
In this case KEY is NOT a `key-binding' but a base command, which will receive
the alternative ALT-KEY binding.  See also `>>=remap'"
  `(progn
     (>>=global-set-keys
       [remap ,key] ',command
       ,alt-key ',key)))


(provide 'xorns-bindings)
;;; xorns-bindings.el ends here
