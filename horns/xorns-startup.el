;;; xorns-startup.el --- Xorns Initialization Library

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; New-age (>>=) module.  This library defines several utility functions used
;; in Emacs main initialization process.  Including all definitions for user
;; custom configuration.

;;; Code:

(require 'use-package)
(require 'xorns-tools)
(require 'xorns-fonts)



;; Configuration Variables

(defvar >>=xorns-initialized nil
  "Whether or not Xorns has finished the startup process.
This is set to true when executing `emacs-startup-hook'.")


(defvar >>=|enable-server t
  "If non-nil, start an Emacs server if one is not already running.")



;; Configuration functions

(defun >>=xorns/init ()
  "General startup initialization."
  (require 'xorns-config)
  (require 'xorns-migration)
  (->? >>=custom/user-init)
  ; TODO: load-default-theme
  (use-package xorns-ui
    :hook
    (after-init . spaceline-xorns-theme)
    :config
    (>>=frame-title-init))
  (>>=configure-font)
  (->? >>=units/configuration)
  (use-package xorns+base
    :config
    (>>=+base/init))
  (>>=setup-emacs-startup-hook)
  (when >>=|enable-server
    (require 'server)
    (unless (server-running-p)
      (message ">>= starting server...")
      (server-start)))
  )


(defun >>=setup-emacs-startup-hook ()
  "Add post initializtion processing."
  (add-hook
   'emacs-startup-hook
   (defun >>=startup-hook ()
     (->? >>=user-config)
     ; TODO: initialize-custom-file-sync
     (setq >>=xorns-initialized
       (float-time (time-subtract nil emacs-start-time)))
     (message ">>= xorns initialized in %s seconds." >>=xorns-initialized))))


(provide 'xorns-startup)
;;; xorns-startup.el ends here
