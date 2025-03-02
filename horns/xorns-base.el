;;; xorns-base.el --- Xorns Configuration for Base System  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Configuration for basic libraries.

;; Configuration of `startup' also includes variables defined in C source
;; code.

;; Libraries (`files', `elec-pair', `mwheel', `iso-transl', and `man') are
;; always configured; `frame' is configured if a graphic display is present,
;; while `xt-mouse' and `xclip' when running in a console.
;;
;; Extra packages can be configured through traits `autorevert', `recentf' and
;; `saveplace'.

;; It's installed just by calling `(require 'xorns-packages)' in the
;; initialization process, which is done automatically.

;; TODO:
;; - See alternatives for `make-backup-files' in:
;;   - https://www.emacswiki.org/emacs/Auto Save
;;   - https://www.emacswiki.org/emacs/BackupFiles

;; Enjoy!


;;; Code:

(eval-and-compile
  (require 'use-package)
  (require 'xorns-tools)
  (require 'xorns-traits))



;; Configuration variables

(defvar >>=|coding-system
  (if (memq system-type '(gnu gnu/linux gnu/kfreebsd darwin))
    'utf-8-unix
    ;; else
    'utf-8)
  "Default value of various coding systems for multilingual environments.")


(defvar >>=|user-mail-address-template nil
  "Template to set `user-mail-address'.
If non-nil must be a string to use `substitute-env-vars' function (t is
converted to \"${USER}@merchise.org\").  The value of EMAIL environment
variable is the best way to configure this leaving this value unchanged as
nil).")


(>>=check-obsolete-variable >>=|base/extra-packages
  (>>=trait/set
    autorevert (memq 'autorevert this)
    recentf (memq 'recentf this)
    saveplace (memq 'saveplace this))
  "0.11.5"
  "traits `autorevert', `recentf' and `saveplace'")


(defvar >>=|make-backup-files nil
  "Non-nil means make a backup of a file the first time it is saved.
In this package, the default value is nil (negating taht used in Emacs
`make-backup-files').  In case to use a non-nil value to this variable, a
minimum configuration is done, but you may like to review some extra variables
to configure for yourself: see `save-buffer' function for more information.")


(defvar >>=|package/column-width 28
  "New width for `package' name column (use nil for standard behaviour).")



;; Default base packages (always configured)

(use-package startup
  ;; mainly configure 'C source code' variables
  :no-require t
  :custom
  (use-short-answers t)
  (inhibit-startup-screen t)
  (initial-scratch-message nil)
  (delete-by-moving-to-trash t)    ; trash is at '~/.local/share/Trash'
  (ring-bell-function 'ignore)
  (mark-even-if-inactive nil)      ; fix undo in commands affecting the mark
  (load-prefer-newer t)
  (scroll-preserve-screen-position t))


(use-package mule
  :no-require t
  :defines truncate-string-ellipsis    ; `mule-util' module
  :config
  (setq truncate-string-ellipsis "…")
  (set-charset-priority 'unicode)
  (when >>=|coding-system
    (setq locale-coding-system >>=|coding-system)
    (set-default-coding-systems >>=|coding-system)
    (prefer-coding-system >>=|coding-system)
    (set-selection-coding-system >>=|coding-system)))


(use-package help
  :no-require t
  :custom
  (help-window-select t))


(use-package frame
  :when (display-graphic-p)
  :custom
  (frame-resize-pixelwise t)
  :config
  ;; Kill `suspend-frame' and start Emacs maximized
  (keymap-global-unset "C-z")
  (keymap-global-unset "C-x C-z")
  (unless (frame-parameter nil 'fullscreen)
    (toggle-frame-maximized))
  (modify-all-frames-parameters
    '((internal-border-width . 0)
       (fullscreen . maximized)
       (fullscreen-restore . maximized)))
  (set-frame-parameter nil 'undecorated t))


(use-package simple
  :preface
  (defun >>=delete-trailing-whitespace ()
    "Delete trailing white-spaces for the full buffer in a safe way."
    ;; TODO: check `delete-trailing-lines' variable, and `whitespace-cleanup'
    ;; function
    (interactive)
    (save-restriction
      (widen)
      (save-mark-and-excursion
        (deactivate-mark 'force)
        (funcall-interactively 'delete-trailing-whitespace))))
  :hook
  (tabulated-list-mode . hl-line-mode)    ; TODO: why is this here?
  :bind
  ("C-c k f" . >>=yank-filename)
  ("C-c k d" . >>=yank-default-directory)
  ("M-SPC" . cycle-spacing)    ;; It was `just-one-space'
  ("M-s-;" . list-processes)
  (:map process-menu-mode-map
    ("k" . process-menu-delete-process))
  :custom
  (async-shell-command-buffer 'new-buffer)
  (column-number-mode +1)
  (mark-ring-max 32)
  (next-error-message-highlight t)
  (global-mark-ring-max 32)
  (kill-ring-max 128)
  (kill-do-not-save-duplicates t)
  (kill-whole-line t)
  (save-interprogram-paste-before-kill t)
  :config
  (setq-default indent-tabs-mode nil)   ; TODO: why is this here?
  (put 'set-goal-column 'disabled nil))


(use-package files
  :after simple
  :bind
  ("C-c f /" . revert-buffer)
  ("C-c f n" . normal-mode)
  :hook
  (before-save . >>=delete-trailing-whitespace)
  :custom
  (require-final-newline t)
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
    (setq make-backup-files nil)))


(use-package delsel
  ;; typed text replaces the selection
  :config
  (delete-selection-mode +1))


(use-package paren
  ;; parenthesis matching
  :custom
  (show-paren-style 'mixed)
  :config
  (show-paren-mode))


(use-package ispell
  :bind
  ("C-c i d" . ispell-change-dictionary)
  ("C-c i l" . ispell-change-dictionary)
  ("C-c i r" . ispell-region)
  ("C-c i b" . ispell-buffer)
  ("C-c i c" . ispell-comments-and-strings)
  ("C-c i k" . ispell-kill-ispell)
  ("C-c i m" . ispell-message)
  :custom
  (ispell-highlight-p t)
  (ispell-silently-savep t)
  (ispell-dictionary "english"))


(use-package xt-mouse
  :unless (display-graphic-p)
  :config
  ;; Enable mouse support when running in a console
  (require 'mouse)
  (xterm-mouse-mode +1)
  (>>=bind-global-keys
    [mouse-4] (lambda () (interactive) (scroll-down 1))
    [mouse-5] (lambda () (interactive) (scroll-up 1))))


;; Fix the clipboard in terminal or daemon Emacs (non-GUI)

(use-package xclip
  :unless (display-graphic-p)
  :ensure t
  :hook
  (tty-setup . xclip-mode))


(use-package elec-pair
  :demand t
  :config
  (electric-pair-mode +1))    ; TODO: Check `custom-set-variables' for user


(use-package mwheel
  :custom
  ;; Use the track-pad to scroll the buffer horizontally
  (mouse-wheel-flip-direction t)
  (mouse-wheel-tilt-scroll t))


;; Fix dead characters
;; https://wiki.archlinux.org/index.php/Emacs#Dead-accent_keys_problem:_.27.3Cdead-acute.3E_is_undefined.27
(use-package iso-transl
  :demand t
  :bind
  (:map key-translation-map
    ("M-[" . iso-transl-ctl-x-8-map)))


;; browse UNIX manual pages

(use-package man
  :commands man
  :custom
  (Man-notify-method 'aggressive))


(when >>=|package/column-width
  (defun >>-package-refresh-contents (&optional _async)
    "Wide the \"Package\" column."
    (interactive)
    (when (eq major-mode 'package-menu-mode)
      (let ((pkg-col (elt tabulated-list-format 0)))
        (when (equal (car pkg-col) "Package")
          (let ((tail (cdr pkg-col)))
            (setcar tail >>=|package/column-width)
            (setcdr pkg-col tail)))))
    ;; (apply org-func args)
    )
  (advice-add 'package-refresh-contents :before '>>-package-refresh-contents))


(use-package transient
  :ensure t
  :demand t)


(>>=trait autorevert
  :init
  (defun >>-auto-revert? ()
    (unless (>>=current-buffer-remote?)
      (auto-revert-mode)))
  :custom
  ;; global-auto-revert-non-file-buffers
  (auto-revert-verbose nil)
  (auto-revert-check-vc-info nil)
  :hook
  (find-file . >>-auto-revert?)
  (dired-mode . auto-revert-mode))


(>>=trait recentf
  :demand t
  :custom
  (recentf-max-saved-items 64)
  ;; Cleanup only when Emacs is idle for 5 minutes, not when the mode is
  ;; enabled, that unnecessarily slows down Emacs.
  (recentf-auto-cleanup 300)
  :bind
  ("C-x M-r" . recentf-open-files)
  ("C-x C-/" . recentf-open-files)
  :config
  ;; TODO: (run-with-idle-timer 600 t 'recentf-save-list)
  (recentf-mode +1))


(>>=trait saveplace
  (save-place-mode +1))



;;; Enable some disabled commands

;; Give us narrowing back!
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
;; Same for region casing
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


(provide 'xorns-base)
;;; xorns-base.el ends here
