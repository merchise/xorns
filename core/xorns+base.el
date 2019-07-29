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


(defvar >>=|base/extra-packages '()
  "List of optional base packages to install.
There is a fix list (window files), that are always configured by default.
See `>>=!base/extra-packages-full-list' for a full list.")


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
  ; window tree functions
  (>>=base/install-message window)
  (use-package window
    :no-require t
    :custom
    (split-width-threshold nil)
    :chords
    ("xk" . kill-buffer-and-window))
  ; file input and output commands
  (>>=base/install-message files)
  (use-package files
    :bind (("C-c f /" . revert-buffer)
	   ("C-c f n" . normal-mode))
    :hook
    (before-save . delete-trailing-whitespace)
    :custom
    (require-final-newline t)
    ;; backup settings
    (backup-by-copying t)
    (backup-directory-alist
      `((".*" . ,(expand-file-name ".backups" user-emacs-directory))))
    (delete-old-versions t)
    (kept-new-versions 6)
    (kept-old-versions 0)    ; check this
    (version-control t))
  ; Automatically reload files was modified by external program
  (when (>>=-base/configure? autorevert)
    (>>=base/install-message autorevert)
    (use-package autorevert
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
      (dired-mode . auto-revert-mode)))
  ; setup a menu of recently opened files
  (when (>>=-base/configure? recentf)
    (>>=base/install-message recentf)
    (use-package recentf
      :defer 0.1
      :custom
      (recentf-auto-cleanup 30)
      :config
      (run-with-idle-timer 30 t 'recentf-save-list)))
  ; garbage collector magic hack
  (when (>>=-base/configure? gcmh)
    (>>=base/install-message gcmh)
    (use-package gcmh
      :ensure t
      :diminish " ♻"
      :init
      (gcmh-mode 1))))


(provide 'xorns+base)
;;; xorns+base.el ends here
