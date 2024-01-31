;;; xorns-term.el --- Basic terminal support  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;;; Commentary:

;; In this module are configured all the features related to terminals.

;; Customizable 256 colors are configured by default for `term' and
;; `ansi-term', to disable it set `>>=|term/install-customizable-colors'
;; variable to nil.

;; Enjoy!


;;; Code:

(eval-and-compile
  (require 'esh-io)
  (require 'em-alias)    ; eshell/alias
  (require 'term)
  (require 'use-package)
  (require 'xorns-tools)
  (require 'xorns-window))



;;; Common setup

;; TODO: remove this variable, always kill buffer
(defvar >>=|term/preserve-finished-buffer nil
  "Preserve terminal buffer when finished.
After a terminal is closed, you get a useless buffer with no process.  That
buffer is killed automatically unless this variable is not nil.")


(defvar >>=|term/install-customizable-colors t
  "Add customizable 256 color support to `term' and `ansi-term' .")


(defun >>=term/shell-file-name ()
  "Get the executable file name to load inferior shells from.
Attempt until finding a value, first the variables `explicit-shell-file-name'
and `shell-file-name', then the environment variables `ESHELL' and `SHELL' and
finally the executables `bash', `zsh' and `sh'."
  (purecopy
    (>>=executable-find
      explicit-shell-file-name
      shell-file-name
      (getenv "ESHELL")
      (getenv "SHELL")
      "bash"
      "zsh"
      "sh")))


;; TODO: https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org
(use-package eshell
  :defines eshell-visual-commands eshell-prompt-regexp
  :functions eshell-cmpl-initialize
  :commands eshell eshell-command
  :init
  (defun >>-eshell/delete-char-or-quit (arg)
    (interactive "p")
    ;; TODO: check using not nil value for `limit' in `looking-back'
    (if (and (eolp) (looking-back eshell-prompt-regexp nil))
      (progn
        (eshell-life-is-too-much)
        (ignore-errors    ; TODO: use `kill-buffer-and-window'
          (delete-window)))
      ;;; else
      (delete-char arg)))

  (defun >>-eshell/first-time ()
    "Run the first time `eshell-mode' is entered.a"
    (add-to-list 'eshell-visual-commands "htop"))

  (defun >>-eshell/init ()
    "Initialize eshell."
    (eshell-cmpl-initialize)
    (keymap-set eshell-mode-map "C-d" '>>-eshell/delete-char-or-quit)
    (when (bound-and-true-p helm-mode)
      (keymap-set eshell-mode-map
        "<remap> <eshell-pcomplete>" 'helm-esh-pcomplete)
      (keymap-set eshell-mode-map
        "<remap> <eshell-list-history>" 'helm-eshell-history)))
  :custom
  (eshell-history-size 1024)
  (eshell-hist-ignoredups t)
  :hook
  (eshell-first-time-mode . >>-eshell/first-time)
  (eshell-mode . >>-eshell/init)
  :config
  (eshell/alias "l" "ls -lh $1")
  (eshell/alias "ll" "ls -alhF $1")
  (eshell/alias "la" "ls -A $1"))


(use-package eshell-syntax-highlighting
  ;; TODO: Check `eshell-syntax-highlighting-highlight-in-remote-dirs'
  :after eshell
  :ensure t
  :hook
  (eshell-mode . eshell-syntax-highlighting-mode))


(use-package comint
  :ensure helm
  :preface
  (defun >>-comint/init ()
    "Initialize comint."
    (when (bound-and-true-p helm-mode)
      (require 'helm-eshell)
      (keymap-set comint-mode-map "C-c C-l" 'helm-comint-input-ring)
      (keymap-set comint-mode-map "M-s f" 'helm-comint-prompts-all)))
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
  (:map term-raw-map
    ("C-y" . term-paste)
    ("C-k" . >>-term/raw-kill-line))
  :custom
  (term-input-autoexpand t)
  :config
  (let ((key (concat (key-description term-escape-char) " C-t")))
    (keymap-set term-mode-map key 'term-char-mode)
    (keymap-set term-raw-map key 'term-line-mode))
  (unless >>=|term/preserve-finished-buffer
    (defun >>-term/handle-exit (&optional proc event)
      (ignore proc event)
      (ignore-errors
        (kill-buffer-and-window)))
    (advice-add 'term-handle-exit :after '>>-term/handle-exit)))


(use-package eterm-256color
  :when >>=|term/install-customizable-colors
  :ensure t
  :hook
  (term-mode . eterm-256color-mode))


(provide 'xorns-term)
;;; xorns-term.el ends here
