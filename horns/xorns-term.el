;;; xorns-term.el --- Basic terminal support  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;;; Commentary:

;; In this module are configured all the features related to terminals.

;; A new terminal can be defined with the macro `>>=define-terminal' using
;; several optional keyword arguments to configure some parameters: the
;; command to execute as a shell, a list of `major-mode' symbols to associate
;; a terminal kind with buffers under these modes.  Example of terminals that
;; have already been defined: `>>=main-term' (the default terminal), and
;; `>>=python-term' (for Python modes) defined in the module `xorns-prog'.

;; The function `>>=terminal' orchestrates all terminal kinds based on their
;; associations with major modes.

;; Each terminal kind can trigger several tabs, each one is identified with a
;; zero or positive integer, also nil is used for the default tab.  To select
;; a terminal tab a prefix argument is used.  A negative value is used to
;; execute a paste operation from the current buffer to a target terminal
;; (using the absolute value).  The `universal-argument' (`C-u') is used to
;; paste to the default tab, and `negative-argument' (`C--') for the tab with
;; tab zero.

;; Every time a terminal is triggered, a reference to the current buffer is
;; linked to that tab, executing a command to select an active tab will
;; switch, or paste, to the linked buffer.

;; Customizable 256 colors are configured by default for `term' and
;; `ansi-term', to disable it set `>>=|term/install-customizable-colors'
;; variable to nil.

;; Enjoy!


;;; Code:

;; 95  variable ‘eshell-mode-map’ -
;; 109 function ‘eshell/alias’ -

(eval-and-compile
  (require 'esh-mode)
  (require 'em-alias)
  (require 'use-package nil 'noerror))

(require 'term)
(require 'xorns-tools)
(require 'xorns-init)
(require 'xorns-bindings)
(require 'xorns-core)
(require 'xorns-simple)
(require 'xorns-xterm)


;;; Common setup

(defvar >>=|term/preserve-finished-buffer nil
  "Preserve terminal buffer when finished.
After a terminal is closed, you get a useless buffer with no process.  That
buffer is killed automatically unless this variable is not nil.")


(defvar >>=|term/install-customizable-colors t
  "Add customizable 256 color support to `term' and `ansi-term' .")


(defun >>-term/kill-finished-buffer ()
  "Internal function to be used on `term-exec-hook'."
  ;; Based on https://oremacs.com/2015/01/01/three-ansi-term-tips/
  (let* ((buff (current-buffer))
         (proc (get-buffer-process buff)))
    (set-process-sentinel
      proc
      `(lambda (process event)
         (if (string= event "finished\n")
           (>>=kill-buffer-and-window ,buff))))))


(use-package eshell
  :commands (eshell eshell-command)
  :preface
  (progn
    (eval-when-compile
      (require 'em-term)
      (declare-function eshell-cmpl-initialize 'em-cmpl))

    (defun >>-eshell/first-time ()
      "Run the first time `eshell-mode' is entered.a"
      (add-to-list 'eshell-visual-commands "htop"))

    (defun >>-eshell/init ()
      "Initialize eshell."
      (eshell-cmpl-initialize)
      (define-key eshell-mode-map
        (kbd "C-c C-d") 'quit-window)
      (when (bound-and-true-p helm-mode)
        (require 'helm-eshell)
        (define-key eshell-mode-map
          [remap eshell-pcomplete] 'helm-esh-pcomplete)
        (define-key eshell-mode-map
          [remap eshell-list-history] 'helm-eshell-history))))
  :custom
  (eshell-history-size 1024)
  (eshell-hist-ignoredups t)
  :hook
  (eshell-first-time-mode . >>-eshell/first-time)
  (eshell-mode . >>-eshell/init)
  :config
  (progn
    (require 'esh-io)
    (require 'em-alias)
    (eshell/alias "l" "ls -lh $1")
    (eshell/alias "ll" "ls -alhF $1")
    (eshell/alias "la" "ls -A $1")))


(use-package comint
  :ensure helm
  :preface
  (defun >>-comint/init ()
    "Initialize comint."
    (when (bound-and-true-p helm-mode)
      (require 'helm-eshell)
      (define-key comint-mode-map
        (kbd "C-c C-l") 'helm-comint-input-ring)
      (define-key comint-mode-map
        (kbd "M-s f") 'helm-comint-prompts-all)))
  :hook
  (comint-mode . >>-comint/init))


(use-package term
  :preface
  (defun >>-term/raw-kill-line ()
    "Kill the rest of the current line in `term-char-mode'."
    (interactive)
    (term-send-raw-string "\C-k")
    (kill-line))
  :bind
  ((:map term-mode-map
     ("C-c C-t" . term-char-mode))
   (:map term-raw-map
     ("C-c C-t" . term-line-mode)
     ("C-y" . term-paste)
     ("C-k" . >>-term/raw-kill-line)))
  :custom
  (term-input-autoexpand t)
  :config
  (unless >>=|term/preserve-finished-buffer
    (add-hook 'term-exec-hook '>>-term/kill-finished-buffer)))


(use-package eterm-256color
  :when >>=|term/install-customizable-colors
  :ensure t
  :hook
  (term-mode . eterm-256color-mode))


(provide 'xorns-term)
;;; xorns-term.el ends here
