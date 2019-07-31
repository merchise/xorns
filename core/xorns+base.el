;;; xorns+base.el --- Xorns Configuration for Base System

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module is installed just by calling `(require 'xorns-packages)' in the
;; initialization process, which is done automatically.

;;; Code:

(require 'xorns-tools)


;; Configuration variables

(defconst >>=!base/extra-packages-full-list
  '(autorevert recentf gcmh)
  "All possible extra packages that can be configured in the base module.")


(defvar >>=|base/user-mail-address-template nil
  "Template to set `user-mail-address'.
If non-nil must be a string to use `substitute-env-vars' function (t is
converted to \"${USER}@merchise.org\").  The value of EMAIL environment
variable is the best way to configure this leaving this value unchanged as
nil).")


(defvar >>=|base/extra-packages '()
  "List of optional base packages to install.
There is a fix list (window files), that are always configured by default.
See `>>=!base/extra-packages-full-list' for a full list.")


(defvar >>=|base/make-backup-files nil
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

(defun >>+base/init ()
  "Initialize 'base' building-block."
  (require 'use-package)
  (use-package startup
    :defer
    :custom
    (inhibit-startup-screen t)
    (initial-scratch-message nil)
    ; (inhibit-startup-echo-area-message (or (getenv "USER") ""))
    :init
    (when >>=|base/user-mail-address-template
      (if (eq >>=|base/user-mail-address-template t)
	(setq >>=|base/user-mail-address-template "${USER}@merchise.org"))
      (setq user-mail-address
	(substitute-env-vars >>=|base/user-mail-address-template))))
  ; window tree functions
  (use-package window
    :no-require t
    :custom
    (split-width-threshold 120)
    :chords
    ("xk" . kill-buffer-and-window)
    :init
    (>>=base/install-message window))
  ; file input and output commands
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
    (if >>=|base/make-backup-files
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
      (setq make-backup-files nil)))
  ; Automatically reload files was modified by external program
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
    (>>=base/install-message autorevert))
  ; setup a menu of recently opened files
  (use-package recentf
    :when (>>=-base/configure? recentf)
    :defer 0.1
    :custom
    (recentf-auto-cleanup 30)
    :init
    (>>=base/install-message recentf)
    :config
    (run-with-idle-timer 30 t 'recentf-save-list))
  ; garbage collector magic hack
  (use-package gcmh
    :when (>>=-base/configure? gcmh)
    :ensure t
    :diminish " ♻"
    :init
    (progn
      (gcmh-mode 1)
      (>>=base/install-message gcmh))))


(provide 'xorns+base)
;;; xorns+base.el ends here
