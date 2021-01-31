;;; xorns-init.el --- Initialization management module  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;;; Commentary:

;; In this module all the functionalities related to the Emacs initialization
;; process are configured (including if it is running as a desktop
;; environment).

;; Enjoy!


;;; Code:

(let ((emacs-min-version "26.1"))
  (unless (version<= emacs-min-version emacs-version)
    (error "This `xorns' version requires Emacs >='%s'" emacs-min-version)))


(defconst >>=!window-manager
  (or
    (getenv "DESKTOP_SESSION")
    (getenv "XDG_SESSION_DESKTOP")
    (getenv "GDMSESSION")
    (getenv "XDG_CURRENT_DESKTOP"))
  "Name of started Desktop Window Manager.")


(defconst >>=!emacs-as-wm
  (and
    >>=!window-manager
    (string-match-p "\\(emacs\\|exwm\\)" >>=!window-manager))
  "Name of started Desktop Window Manager.")


(defvar >>=xorns-initialized nil
  "Whether or not Xorns has finished the startup process.
This is set to true when executing `emacs-startup-hook'.")


(provide 'xorns-init)
;;; xorns-init.el ends here
