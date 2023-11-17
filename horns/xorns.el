;;; xorns.el --- Merchise extensions for Emacs  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]
;; URL: https://github.com/merchise/xorns
;; Version: 0.9.4

;;; Commentary:

;; To use `xorns', just require its main module in the user’s initialization
;; file:
;;
;;   (require 'xorns)
;;
;; We advise to use our version of `user-init-file'.

;; Enjoy!


;;; Code:


(eval-and-compile
  (require 'server)
  (require 'use-package nil 'noerror))

(require 'xorns-init)



;; Configuration Variables

(defvar >>=|enable-server nil
  "If non-nil, start an Emacs server if one is not already running.")


(>>=progn "base initialization"
  (require 'xorns-base))


(when >>=!emacs-as-wm
  (>>=progn "start emacs as a window manager"
    (require 'xorns-exwm)))


(add-hook
  'emacs-startup-hook
  (defun >>-startup-hook ()
    (->? >>=user-code)
    (setq >>=xorns-initialized
      (float-time (time-subtract after-init-time before-init-time)))
    (require 'xorns-gc)    ; Configure GC strategy
    ;; (>>=font/configure)
    (run-with-timer 1 nil
      'message ">>= xorns initialized in %.1f seconds." >>=xorns-initialized)
    ))


(when (and >>=|enable-server (not noninteractive))
  (unless (or (daemonp) (server-running-p))
    (message ">>= starting server...")
    (server-start)))


(>>=progn "main initialization"
  (require 'xorns-common-systems)
  (require 'xorns-building-blocks))


(provide 'xorns)
;;; xorns.el ends here
