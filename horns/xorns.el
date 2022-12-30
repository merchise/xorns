;;; xorns.el --- Merchise extensions for Emacs  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]
;; URL: https://github.com/merchise/xorns
;; Version: 0.9

;;; Commentary:

;; To use `xorns', just require its main module in the user’s initialization
;; file:
;;
;;   (require 'xorns)
;;
;; We advise to use our version of `user-init-file'.

;; Enjoy!


;;; Code:

;; `xorns-packages' must be the first module loaded here
(require 'xorns-packages)

(eval-and-compile
  (require 'server)
  (require 'use-package nil 'noerror))

(require 'xorns-display)
(require 'xorns-init)



;; Configuration Variables


(defvar >>=|enable-server nil
  "If non-nil, start an Emacs server if one is not already running.")


(require 'xorns-config)
(->? >>=building-blocks/configuration)    ; TODO: Move this to `xorns-config'


(>>=progn "base initialization"
  (require 'xorns-base))


(when >>=!emacs-as-wm
  (>>=progn "start emacs as a window manager"
    (require 'xorns-exwm)))


(defun >>-initialized ()
  "Function called after `xorns' is completely initialized."
  (>>=configure-font)
  (message ">>= xorns initialized in %s seconds." >>=xorns-initialized))


(add-hook
  'emacs-startup-hook
  (defun >>=startup-hook ()
    (->? >>=user-code)
    (setq >>=xorns-initialized
      (format "%.1f seconds"
        (float-time (time-subtract after-init-time before-init-time))))
    (require 'xorns-gc)    ; Configure GC strategy
    (run-with-timer 1 nil '>>-initialized)
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
