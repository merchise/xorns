;;; xorns.el --- Merchise extensions for Emacs  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]
;; URL: https://github.com/merchise/xorns
;; Version: 0.11.2

;;; Commentary:

;; To use `xorns', just require its main module in the user's initialization
;; file (`user-init-file'):
;;
;;   (require 'xorns)

;; Enjoy!


;;; Code:

(eval-and-compile
  (require 'xorns-tools))

(require 'xorns-packages)    ; must be loaded first any module using ELPA


(when init-file-debug
  (setq message-log-max 10000))


(defconst >>=!window-manager
  (or
    (getenv "DESKTOP_SESSION")
    (getenv "XDG_SESSION_DESKTOP")
    (getenv "GDMSESSION")
    (getenv "XDG_CURRENT_DESKTOP"))
  "Name of started Desktop Window Manager.")


(defconst >>=!emacs-as-wm
  (when
    (and
      >>=!window-manager
      (string-match-p "\\(emacs\\|exwm\\)" >>=!window-manager))
    >>=!window-manager)
  "Name of started Desktop Window Manager when EXWM is started.")


(defvar >>=|enable-server nil
  "If non-nil, start an Emacs server if one is not already running.")


(defgroup xorns nil
  "Declare a customization group for `xorns'.
Intended for future use of configuration variables."
  :group 'starter-kit
  :prefix '>>=|)


(>>=progn "user configuration setting"
  (require 'xorns-config))


(>>=progn "base initialization"
  (require 'xorns-base))


(when >>=!emacs-as-wm
  (>>=progn "start emacs as a window manager"
    (require 'xorns-exwm)))


(add-hook
  'emacs-startup-hook
  (defun >>-startup-hook ()
    (->? >>=user-code)
    (require 'xorns-gc)    ; Configure GC strategy
    (run-with-timer 1 nil
      'message ">>= xorns initialized in %.1f seconds." (>>=init-time))))


(when (or (and >>=|enable-server (not noninteractive)) >>=!emacs-as-wm)
  (eval-and-compile
    (require 'server))
  (unless (or (daemonp) (server-running-p))
    (message ">>= starting server...")
    (server-start)))


(>>=progn "main initialization"
  (require 'xorns-common-systems))


(provide 'xorns)
;;; xorns.el ends here
