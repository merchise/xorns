;;; xorns-base.el --- Xorns Configuration for Base System  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Packages ('startup', 'window', 'files', 'windmove', 'elec-pair', 'mwheel',
;; 'iso-transl', and 'man') are always configured; 'frame' is configured if a
;; graphic display is present, while 'xt-mouse' and 'xclip' when running in a
;; console.
;;
;; Extra packages can be configured through `>>=|base/extra-packages'
;; variable.  Options are ('autorevert', 'recentf', and 'saveplace').

;; It's installed just by calling `(require 'xorns-packages)' in the
;; initialization process, which is done automatically.
;;
;; TODO:
;; - See alternatives for `make-backup-files' in:
;;   - https://www.emacswiki.org/emacs/Auto Save
;;   - https://www.emacswiki.org/emacs/BackupFiles

;; Enjoy!


;;; Code:

(eval-and-compile
  (require 'use-package)
  (require 'xorns-tools)
  (require 'xorns-simple))



;; Configuration variables

(defvar >>=|user-mail-address-template nil
  "Template to set `user-mail-address'.
If non-nil must be a string to use `substitute-env-vars' function (t is
converted to \"${USER}@merchise.org\").  The value of EMAIL environment
variable is the best way to configure this leaving this value unchanged as
nil).")


(defvar >>=|base/extra-packages nil
  "List of optional base packages to install.
A full of options are (autorevert recentf saveplace).")


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
  :no-require t
  :custom
  (use-short-answers t)
  (inhibit-startup-screen t)
  (initial-scratch-message nil))


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


(use-package window
  :init
  (defconst >>-window-coach-mode-keys
    '((shrink-window "<up>" "p")
      (enlarge-window "<down>" "n")
      (enlarge-window-horizontally "<right>" "f")
      (shrink-window-horizontally "<left>" "b")
      (other-window "o")
      (>>=window/split-toggle "t")
      (>>=window-coach-mode "C-g" "<RET>")))

  (define-minor-mode >>=window-coach-mode
    "A simple window-coach minor mode."
    :init-value nil
    :lighter " Window-Coach"
    :global t
    :keymap
      (let ((map (make-sparse-keymap)))
        (dolist (item >>-window-coach-mode-keys)
          (let ((fn (car item))
                (keys (cdr item)))
            (dolist (key keys)
              (keymap-set map key fn))))
        map)
    :group 'window)

  (defun >>=window/split-toggle (&optional arg)
    "Toggle horizontal/vertical layout of 2 windows (use ARG to restore)."
    (interactive "P")
    (if (= (count-windows) 2)
      (let* ((tree (car (window-tree)))
              (one (nth 2 tree))
              (two (nth 3 tree))
              (aux (car tree))                ;; t: vertical -> horizontal
              (v2h (if arg (not aux) aux))    ;; (xor arg v2h)
              (state (window-state-get two)))
        (delete-other-windows one)
        (window-state-put
          state
          (funcall
            (if v2h
              #'split-window-horizontally
              ;; else
              #'split-window-vertically))))
      ;; else
      (warn "Only can toggle two windows!")))

  :custom
  (split-width-threshold 120)
  :bind
  ("C-c C-`" . >>=window-coach-mode)
  (:map ctl-x-4-map
    ("t" . >>=window/split-toggle)))


(use-package windmove
  :custom
  (windmove-wrap-around t)
  :config
  (windmove-default-keybindings 'ctrl))


(use-package winner
  :config
  (winner-mode +1))


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
  :defer t
  :custom
  (Man-notify-method 'aggressive))


(when >>=|package/column-width
  (defun >>-package-refresh-contents (&optional _async)
    "Wide the 'Package' column."
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


(use-package autorevert
  :when (memq 'autorevert >>=|base/extra-packages)
  :init
  (defun >>-auto-revert? ()
    (unless (>>=current-buffer-remote?)
      (auto-revert-mode)))
  :defer t
  :custom
  ;; global-auto-revert-non-file-buffers
  (auto-revert-verbose nil)
  (auto-revert-check-vc-info nil)
  :hook
  (find-file . >>-auto-revert?)
  (dired-mode . auto-revert-mode))


(use-package recentf
  :when (memq 'recentf >>=|base/extra-packages)
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


(use-package saveplace
  :when (memq 'saveplace >>=|base/extra-packages)
  :config
  (save-place-mode +1))


(provide 'xorns-base)
;;; xorns-base.el ends here
