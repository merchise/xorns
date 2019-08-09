;;; xorns+base.el --- Xorns Configuration for Base System

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This is a new-age (>>=) module.  It's installed just by calling `(require
;; 'xorns-packages)' in the initialization process, which is done
;; automatically.

;;; Code:
(eval-when-compile
  (require 'use-package))
(require 'xorns-tools)


;; Configuration variables

(defvar >>=|user-mail-address-template nil
  "Template to set `user-mail-address'.
If non-nil must be a string to use `substitute-env-vars' function (t is
converted to \"${USER}@merchise.org\").  The value of EMAIL environment
variable is the best way to configure this leaving this value unchanged as
nil).")


(defvar >>=|base/extra-packages '()
  "List of optional base packages to install.
There is a fix list (window files), that are always configured by default.
Here is the full list (autorevert recentf gcmh).")


(defvar >>=|make-backup-files nil
  "Non-nil means make a backup of a file the first time it is saved.
In this package, the default value is nil (negating taht used in Emacs
`make-backup-files').  In case to use a non-nil value to this variable, a
minimum configuration is done, but you may like to review some extra variables
to configure for yourself: see `save-buffer' function for more information.")


(defmacro >>=-base/configure? (pkg)
  "True if extra PKG must be configured."
  `(memq ',pkg >>=|base/extra-packages))


(defmacro >>=base/install-message (pkg)
  "Display message (using `>>=on-debug-message') notifying PKG installation."
  `(>>=on-debug-message "installing base package: %s" ',pkg))



;; Default base packages (always configured)

(defun >>=+base/init ()
  "Initialize 'base' building-block (unit)."
  (>>=-base/init-startup)
  (>>=-base/init-window)
  (>>=-base/init-files)
  (>>=-base/init-autorevert)
  (>>=-base/init-recentf)
  (>>=-base/init-gcmh))



;; Component packages

(defun >>=-base/init-startup ()
  "Initialize base unit 'startup' package."
  (use-package startup
    :defer
    :custom
    (inhibit-startup-screen t)
    (initial-scratch-message nil)
    ; (inhibit-startup-echo-area-message (or (getenv "USER") ""))
    :init
    (when >>=|user-mail-address-template
      (if (eq >>=|user-mail-address-template t)
	(setq >>=|user-mail-address-template "${USER}@merchise.org"))
      (setq user-mail-address
	(substitute-env-vars >>=|user-mail-address-template)))))


(defun >>=-base/init-window ()
  "Initialize base unit 'window' package."
  (use-package window
    :no-require t
    :custom
    (split-width-threshold 120)
    :chords
    ("xk" . kill-buffer-and-window)
    :init
    (>>=base/install-message window)))


(defun >>=-base/init-files ()
  "Initialize base unit 'files' package (file input and output commands)."
  (use-package files
    :bind (("C-c f /" . revert-buffer)
	   ("C-c f n" . normal-mode))
    :hook
    (before-save . delete-trailing-whitespace)
    :custom
    (require-final-newline t)
    :init
    (>>=base/install-message files)
    :config
    (if >>=|make-backup-files
      (setq
	make-backup-files t
	backup-by-copying t
	backup-directory-alist
	  `((".*" . ,(expand-file-name ".backups" user-emacs-directory)))
	delete-old-versions t
	kept-new-versions 6
	kept-old-versions 0    ; check this
	version-control t)
      ; else
      (setq make-backup-files nil))))


(defun >>=-base/init-autorevert ()
  "Initialize base unit 'autorevert' package.
Automatically reload files was modified by external program."
  (use-package autorevert
    :when (>>=-base/configure? autorevert)
    :init
    (defun >>=-auto-revert? ()
      (unless (>>=current-buffer-remote?)
	(auto-revert-mode)))
    :defer t
    :diminish (auto-revert-mode . " ⟲")
    :custom
    (auto-revert-verbose nil)
    (auto-revert-check-vc-info nil)
    :hook
    (find-file . >>=-auto-revert?)
    (dired-mode . auto-revert-mode)
    :init
    (>>=base/install-message autorevert)))


(defun >>=-base/init-recentf ()
  "Initialize base unit 'recentf' package.
Setup a menu of recently opened files."
  (use-package recentf
    :when (>>=-base/configure? recentf)
    :defer 0.1
    :custom
    (recentf-auto-cleanup 30)
    :init
    (>>=base/install-message recentf)
    :config
    (run-with-idle-timer 30 t 'recentf-save-list)))


(defun >>=-base/init-gcmh ()
  "Initialize base unit 'gcmh' package.
Garbage collector magic hack."
  (use-package gcmh
    :when (>>=-base/configure? gcmh)
    :ensure t
    :commands gcmh-mode
    :diminish " ♻"
    :init
    (progn
      (gcmh-mode 1)
      (>>=base/install-message gcmh))))


(provide 'xorns+base)
;;; xorns+base.el ends here
