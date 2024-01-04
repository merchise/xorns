;;; xorns-linux.el --- Helps you control your GNU/Linux computer  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module complements `desktop-environment' package, it is intended to
;; control GNU/Linux desktop, specially when you use Emacs as your X Window
;; Manager (see `exwm').  With `desktop-environment', you can control the
;; brightness, audio volume, take screenshots, and lock your screen.

;; The module depends on the availability of some shell commands, which can be
;; customized by changing the values of the variables where they are defined.
;; We define a new variable `>>=|linux/logind-command' to control
;; Linux/systemd sub-commands (see `>>=linux/exit' function).

;; Load this module requiring it as the last statement of `exwm-input':
;;
;;   (use-package exwm-input
;;     ...
;;     :config
;;     (progn
;;       ...
;;       (require 'xorns-linux)))

;; Enjoy!


;;; Code:

(eval-and-compile
  (require 'use-package))

(require 'xorns-tools)


;;; exit commands

(defconst >>=!linux/exit-commands
  `((lock . desktop-environment-lock-screen)
    (exit . >>-linux-logout)
    (suspend lock "suspend")
    (hibernate lock "hibernate")
    (reboot . "reboot")
    (shutdown . "poweroff"))
"A definition for exit commands, see `>>=linux/exit' function.")


(defvar >>=|linux/logind-command
  ;; based on 'i3exit' script
  (if (string-equal (>>=file-string "/proc/1/comm") "systemd")
    "systemctl"
    ;; else
    "loginctl")
  "Command to control Linux/systemd.")


(defun >>-linux/before-close-session ()
  "Save some modified file-visiting buffers and run `kill-emacs-hook'."
  (save-some-buffers)
  (run-hooks 'kill-emacs-hook))


(defun >>-linux-logout ()
  "Internal function for exit/logout without asking for confirmation."
  (let (confirm-kill-emacs confirm-kill-processes)
    (save-buffers-kill-terminal 'silently)))


(defun >>-linux-exit (rule)
  "Internal function to execute a RULE for exit commands."
  (cond
    ((functionp rule)
      (funcall rule))
    ((stringp rule)
     (let ((command (format "%s %s" >>=|linux/logind-command rule)))
       (message ">>= executing linux/logind command '%s'" command)
       (start-process-shell-command rule nil command)))
    ((listp rule)
      (dolist (item rule)
        (>>-linux-exit item)))
    (t    ; a symbol
      (>>-linux-exit (cdr (assq rule >>=!linux/exit-commands))))))


(defun >>=linux/exit (kind)
  "Center for exit commands.
KIND could be any of `(lock, exit, suspend, hibernate, reboot, shutdown)'.
See `>>=!linux/exit-commands' constant."
  (interactive (list (completing-read ">>= " >>=!linux/exit-commands nil t)))
  (let ((rule (assoc-string kind >>=!linux/exit-commands)))
    (when rule
      (if (memq (car rule) '(exit hibernate reboot shutdown))
        (>>-linux/before-close-session))
      (>>-linux-exit (cdr rule)))))


(use-package desktop-environment
  ;; TODO: This should probably be configured in the `xorns-exwm' module
  :ensure t
  :demand t
  :commands desktop-environment-mode
  :bind
  (:map desktop-environment-mode-map
    ("s-l" . >>=linux/exit))
  :config
  (desktop-environment-mode))


(provide 'xorns-linux)
;;; xorns-linux.el ends here
