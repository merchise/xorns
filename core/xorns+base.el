;;; xorns+base.el --- Xorns Configuration for Base System

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module is installed just by calling `(require 'xorns-packages)' in the
;; initialization process, which is done automatically.


(eval-when-compile
  (require 'use-package recentf))

;; Configuration variables


(defconst >>=!base/extra-packages-full-list
  '(autorevert recentf gcmh)
  "All possible extra packages that can be configured in the base module.")


(defvar >>=|base/extra-packages-to-configure '()
  "List of optional base packages to install.
There is a fix list (window files), that are always configured by default.
See `>>=!base/extra-packages-full-list' for a full list.")


(defmacro >>=-base/configure? (pkg)
  "True if extra PKG must be configured."
  `(memq ',pkg >>=|base/extra-packages-to-configure))



;; Default base packages (always configured)

(use-package window
  :no-require t
  :custom
  (split-width-threshold nil)
  :chords
  (" k" . kill-buffer-and-window))


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
   `((".*" . ,(expand-file-name
               (concat user-emacs-directory "backups")))))
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 0)    ; check this
  (version-control t))

(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 10
      kept-old-versions 0
      vc-make-backup-files t
      version-control t
      backup-directory-alist
      `((".*" . ,(expand-file-name "backup" siren-cache-dir))))


;; Automatically reload files was modified by external program

if (>>=-base/configure? autorevert)
  (use-package autorevert
    :init
    (defun >>=-auto-revert? ()
    (unless (current-buffer-remote-p)
      (auto-revert-mode)))
    :defer 0.1
    :diminish (auto-revert-mode . " Ⓐ")
    :custom
    (auto-revert-verbose nil)
    :hook (after-init . global-auto-revert-mode)))

(use-package autorevert
  :defer t
  :init
  (setq auto-revert-verbose nil)
  (if (not degrade-p-terminal)
      (setq auto-revert-mode-text " ♻"
            auto-revert-tail-mode-text " ♻~")
    (setq auto-revert-mode-text " ar"
          auto-revert-tail-mode-text " ar~"))
  :config
  (defun >>=-auto-revert? ()
    (unless (current-buffer-remote-p)
      (auto-revert-mode)))
   (add-hook 'find-file-hook '>>=-auto-revert?))

;; Don't show anything for auto-revert-mode, which doesn't match its package
;; name.

(use-package autorevert         ; Auto-revert buffers of changed files
  :diminish auto-revert-mode
  :config (global-auto-revert-mode))

(use-package autorevert
  :init (after-init #'global-auto-revert-mode)
  :custom (auto-revert-check-vc-info nil))

(use-package autorevert
  :config
  (global-auto-revert-mode 1)

  ;; auto-update dired buffers
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil))

(use-package autorevert                 ; Auto-revert buffers of changed files
  :init (global-auto-revert-mode)
  :config
  (setq auto-revert-verbose nil         ; Shut up, please!
    ;; Revert Dired buffers, too
    global-auto-revert-non-file-buffers t)

  (when (eq system-type 'darwin)
    ;; File notifications aren't supported on OS X
    (setq auto-revert-use-notify nil))
  :diminish (auto-revert-mode . " Ⓐ"))

(use-package autorevert
  :demand t
  :hook (dired-mode . auto-revert-mode)
  :init
  (progn
    (setq auto-revert-verbose nil))
  :config
  (progn
    (global-auto-revert-mode)))

;;;; autorevert
;; revert buffers when files on disk change
(use-package autorevert
  :diminish auto-revert-mode
  :config
  ;; auto revert buffers when changed on disk
  (global-auto-revert-mode 1))


(use-package autorevert
  :config
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'autorevert)))

  (setq global-auto-revert-non-file-buffers t
        auto-revert-remote-files t
        auto-revert-verbose nil
        auto-revert-mode-text nil))


(use-package autorevert
    :ensure nil
    :commands global-auto-revert-mode
    :demand
    :config (global-auto-revert-mode t))



(use-package autorevert
  :defer 0.1
  :diminish auto-revert-mode)



;;

(use-package recentf
  :defer 0.1
  :custom
  (recentf-auto-cleanup 30)
  :config
  (run-with-idle-timer 30 t 'recentf-save-list))



;; Garbage Collector Magic Hack (https://gitlab.com/koral/gcmh)

(defun >>=gc/init ()
  "Initialize Garbage Collector Magic Hack."
  (use-package gcmh
    :ensure t
    :init
    (gcmh-mode 1)))




;; TODO: Check: 'a13/init.el'
; (use-package cus-edit
;   :custom
;   (custom-file null-device "Don't store customizations"))

(provide 'xorns+base)
;;; xorns+base.el ends here
