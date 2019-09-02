;;; xorns-startup.el --- Xorns Initialization Library

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; New-age (>>=) module.  This library defines several utility functions used
;; in Emacs main initialization process.  Including all definitions for user
;; custom configuration.

;;; Code:

(require 'xorns-packages)

(require 'xorns-tools)
(require 'xorns-fonts)


(defvar >>=xorns-initialized nil
  "Whether or not Xorns has finished the startup process.
This is set to true when executing `emacs-startup-hook'.")


;; Configuration Variables

(defvar >>=|mode-line-unicode-symbols t
  "If non nil unicode symbols are displayed in the mode-line.
If you use Emacs as a daemon and wants unicode characters only in GUI set
the value to quoted `display-graphic-p'.")


(defvar >>=|enable-server t
  "If non-nil, start an Emacs server if one is not already running.")


(defvar >>=|server-socket-dir nil
  "Set the Emacs server socket location.
If nil, uses whatever the Emacs default is, otherwise a directory path like
'~/.emacs.d/server'.  Has no effect if `>>=|enable-server' is nil.")



;; Configuration functions

(defun >>=xorns/init ()
  "General startup initialization."
  (require 'xorns-preface)
  (require 'use-package)
  (require 'xorns-config)
  (>>=-start-maximized)
  (->? >>=custom/user-init)
  ; TODO: load-default-theme
  (use-package xorns-ui
    :hook
    (after-init . spaceline-xorns-theme)
    :config
    (>>=frame-title-init))
  (>>=configure-font)
  (require 'xorns-units)
  (->? >>=units/configuration)
  (>>=units/load)
  (>>=setup-emacs-startup-hook))


(defun >>=-start-maximized ()
  "Start Emacs maximized."
  (set-frame-parameter nil 'undecorated t)
  (add-to-list 'default-frame-alist '(undecorated . t))
  (unless (frame-parameter nil 'fullscreen)
    (toggle-frame-maximized))
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))


(defun >>=setup-emacs-startup-hook ()
  "Add post initializtion processing."
  (add-hook
   'emacs-startup-hook
   (defun >>=startup-hook ()
     (->? >>=user-config)
     ; TODO: initialize-custom-file-sync
     (>>=configure-font)
     (setq >>=xorns-initialized
       (float-time (time-subtract nil emacs-start-time)))
     (message ">>= xorns initialized in %s seconds." >>=xorns-initialized))))


(provide 'xorns-startup)
;;; xorns-startup.el ends here
