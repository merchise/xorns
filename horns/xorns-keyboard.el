;;; xorns-keyboard.el --- Merchise keyboard extensions for Emacs

;; Copyright (c) Merchise Autrement [~º/~]

;;; Commentary:

;; To manage `key-chord-mode', the `>>=|key-chord-delay-level' variable is
;; used together the `>>=key-chord-mode' function.
;;
;; The mode will be disabled with zero or negative value, a positive value
;; will enable it.
;;
;; Use a negative value to start Emacs with the mode disabled but with a
;; default value to use when enabling it.  If 0 is used the system will use 5
;; as the default value.
;;
;; When enabling the mode, the value of the variable will be used to calculate
;; the internal variables of the module `key-chord-two-keys-delay' and
;; `key-chord-one-key-delay' using the formula (* 0.01 (* 3 (1+ level))) for
;; the first, and dividing this result by 5 for the second.
;;
;; The function could receive an argument as follow: nil will toggle the mode,
;; the symbol 'setup' will initialize the mode depending on the value of the
;; variable, any integer value will enable o disable the mode accordingly.

;; Enjoy!


;;; Code:

(require 'use-package)


(defvar >>=|key-chord-delay-level -5
  "Delay level for `key-chord-mode' when enabled (see module documentation).")


(use-package use-package-chords
  :ensure t
  :init
  (defun >>=key-chord-mode (arg)
    "Toggle key-chord mode based in ARG level (see `>>=|key-chord-delay')."
    (interactive "P")
    (let ((aux >>=|key-chord-delay-level)
	  (default 5))
      (setq >>=|key-chord-delay-level
	(cond
	  ((eq arg 'setup) aux)
	  ((null arg) (if (eq aux 0) default (- aux)))
	  ((eq arg 0) (- default))
	  (t (prefix-numeric-value arg)))))
    (if (<= >>=|key-chord-delay-level 0)
      (key-chord-mode 0)
      ;; else
      (let ((level (* 0.01 (* 3 (1+ >>=|key-chord-delay-level)))))
	(setq
	  key-chord-two-keys-delay (/ level 5)
	  key-chord-one-key-delay level)
	(key-chord-mode 1))))
  :bind
  ("C-c M-c" . >>=key-chord-mode)
  :config
  (>>=key-chord-mode 'setup))


(provide 'xorns-keyboard)
;;; xorns-keyboard.el ends here
