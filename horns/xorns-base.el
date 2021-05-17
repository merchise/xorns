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
;; Pending tasks:
;; - See alternatives for `make-backup-files' in:
;;   - https://www.emacswiki.org/emacs/Auto Save
;;   - https://www.emacswiki.org/emacs/BackupFiles

;; Enjoy!


;;; Code:

(require 'use-package)
(require 'xorns-tools)
(require 'xorns-packages)



;; Configuration variables

(defvar >>=|user-mail-address-template nil
  "Template to set `user-mail-address'.
If non-nil must be a string to use `substitute-env-vars' function (t is
converted to \"${USER}@merchise.org\").  The value of EMAIL environment
variable is the best way to configure this leaving this value unchanged as
nil).")


(defvar >>=|base/extra-packages nil
  "List of optional base packages to install.
A full of options are (autorevert recentf saveplace gcmh).")


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
  :defer
  :custom
  (inhibit-startup-screen t)
  (initial-scratch-message nil)
  :init
  (progn
    ;; Replace `yes|not' commands for simpler `[yn]'
    (defalias 'yes-or-no-p 'y-or-n-p)
    ; TODO: Move this to another place
    (when >>=|user-mail-address-template
      (if (eq >>=|user-mail-address-template t)
        (setq >>=|user-mail-address-template "${USER}@merchise.org"))
      (setq user-mail-address
        (substitute-env-vars >>=|user-mail-address-template)))))


(use-package frame
  :when (display-graphic-p)
  :custom
  (frame-resize-pixelwise t)
  :config
  (progn
    ;; Kill `suspend-frame' and start Emacs maximized
    (global-unset-key (kbd "C-z"))
    (global-unset-key (kbd "C-x C-z"))
    (unless (frame-parameter nil 'fullscreen)
      (toggle-frame-maximized))
    (modify-all-frames-parameters
      '((internal-border-width . 0)
            (fullscreen . maximized)
            (fullscreen-restore . maximized)
            (undecorated . t)))
    ))


(use-package window
  :preface
  (progn
    (eval-when-compile
      (declare-function count-windows 'window)
      (declare-function window-tree 'window)
      (declare-function window-state-get 'window)
      (declare-function delete-other-windows 'window)
      (declare-function window-state-put 'window)
      (declare-function split-window-horizontally 'window)
      (declare-function split-window-vertically 'window))

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

    (defun >>-window-coach/done ()
      (interactive)
      (setq >>=window-coach nil)
      (message ">>= window-coach done."))

    (defvar >>=window-coach-map
      (let ((map (make-keymap))
            (commands
              '((shrink-window "<up>" "p")
                (enlarge-window "<down>" "n")
                (enlarge-window-horizontally "<right>" "f")
                (shrink-window-horizontally "<left>" "b")
                (other-window "o")
                (>>=window/split-toggle "t")
                (>>-window-coach/done "C-g" "q"))))
        (dolist (cmd commands)
          (let ((fn (car cmd))
                (keys (cdr cmd)))
            (dolist (key keys)
              (define-key map (kbd key) fn))))
        map))

    (define-minor-mode >>=window-coach
      "A simple window-coach minor mode."
      :init-value nil
      :lighter " Window-Coach"
      :keymap >>=window-coach-map
      :global t
      (if (<= (count-windows) 1)
        (progn
          (setq >>=window-coach nil)
          (message ">>= only root frame exists, abort."))
        ;; else
        (message ">>= use arrow-keys or p/n/f/b/o/t/q to manage windows.")))

    (provide 'window)
    )
  :custom
  (split-width-threshold 120)
  :bind
  ("C-c C-`" . >>=window-coach)
  (:map ctl-x-4-map
    ("t" . >>=window/split-toggle)))


(use-package files
  :bind (("C-c f /" . revert-buffer)
         ("C-c f n" . normal-mode))
  :hook
  (before-save . delete-trailing-whitespace)
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


(use-package windmove
  :custom
  (windmove-wrap-around t)
  :config
  (windmove-default-keybindings 'ctrl))


(use-package winner
  :config
  (progn
    (winner-mode +1)))


(use-package xt-mouse
  :unless (display-graphic-p)
  :config
  (progn
    ; Enable mouse support when running in a console
    (require 'mouse)
    (xterm-mouse-mode +1)
    (global-set-key [mouse-4]
      (lambda () (interactive) (scroll-down 1)))
    (global-set-key [mouse-5]
      (lambda () (interactive) (scroll-up 1)))))


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
  ;; Use the trackpade to scroll the buffer horizontally
  (mouse-wheel-flip-direction t)
  (mouse-wheel-tilt-scroll t))


;; Fix dead characters
;; https://wiki.archlinux.org/index.php/Emacs#Dead-accent_keys_problem:_.27.3Cdead-acute.3E_is_undefined.27
(use-package iso-transl
  :demand t
  :config
  (define-key key-translation-map (kbd "M-[") 'iso-transl-ctl-x-8-map))


;; browse UNIX manual pages
(use-package man
  :defer t
  :custom
  (Man-notify-method 'aggressive))


(when >>=|package/column-width
  (defadvice package-refresh-contents
    (around >>-package-refresh-contents activate)
    "Wide the 'Package' column."
    (interactive)
    (if (eq major-mode 'package-menu-mode)
      (let ((pkg-col (elt tabulated-list-format 0)))
        (when (equal (car pkg-col) "Package")
          (let ((tail (cdr pkg-col)))
            (setcar tail >>=|package/column-width)
            (setcdr pkg-col tail)))))
    ;; super
    ad-do-it))


(use-package transient
  :ensure t
  :demand t)


(use-package autorevert
  :when (memq 'autorevert >>=|base/extra-packages)
  :init
  (defun >>=-auto-revert? ()
    (unless (>>=current-buffer-remote?)
      (auto-revert-mode)))
  :defer t
  :custom
  ;; global-auto-revert-non-file-buffers
  (auto-revert-verbose nil)
  (auto-revert-check-vc-info nil)
  :hook
  (find-file . >>=-auto-revert?)
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
  ("C-c C-r" . recentf-open-files)
  :config
  (progn
    (run-with-idle-timer (* 2 recentf-auto-cleanup) t 'recentf-save-list)
    (recentf-mode +1)))


(use-package saveplace
  :when (memq 'saveplace >>=|base/extra-packages)
  :config
  (save-place-mode +1))


(when (memq 'gcmh >>=|base/extra-packages)
  ;; TODO: remove this in release 1.0
  (>>=deprecate 'gcmh :current 'xorns-base :new 'xorns-gc))


(provide 'xorns-base)
;;; xorns-base.el ends here
