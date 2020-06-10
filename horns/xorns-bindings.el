;;; xorns-bindings.el --- Key binding simple tools

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tools to manage key-bindings.  It will be taken into account if `exwm' is
;; active when assigning each command.

;; Enjoy!


;;; Code:


;;
;; (fboundp 'exwm-input-set-key)
;; (featurep 'exwm-input)


(defun >>=global-set-key (key command)
  "Internal to give KEY a global binding as COMMAND.
It uses `exwm' if enabled."
  (if (featurep 'exwm-input)
    (exwm-input-set-key key command)
    ;; else
    (let ((map (current-global-map)))
      (define-key map key command))))


(defmacro >>=remap (key command alt-key)
  "Give KEY a global binding as COMMAND.
In this case, KEY is a standard `key-binding', whose original command will
receive the alternative ALT-KEY binding.  See also `>>=remap*'"
  `(let* ((kbind (kbd ,key))
	  (abind (kbd ,alt-key))
	  (org (key-binding kbind)))
     (>>=global-set-key kbind ',command)
     (>>=global-set-key abind org)))


(defmacro >>=remap* (key command alt-key)
  "Give KEY a global binding as COMMAND.
In this case KEY is NOT a `key-binding' but a base command, which will receive
the alternative ALT-KEY binding.  See also `>>=remap'"
  `(progn
     (>>=global-set-key [remap ,key] ',command)
     (>>=global-set-key (kbd ,alt-key) ',key)))


(provide 'xorns-bindings)
;;; xorns-bindings.el ends here
