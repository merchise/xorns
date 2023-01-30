;;; xorns-simple.el --- Merchise basic editing commands for Emacs  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;;; Commentary:

;; Container for basic commands that are not related to any specific
;; major-mode.

;; Enjoy!


;;; Code:

(eval-and-compile
  (require 'files)
  (require 'xorns-tools)
  (require 'xorns-setup))



;;; configuration

(use-package simple
  :defer t
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
  (column-number-mode +1)
  (async-shell-command-buffer 'new-buffer)
  (mark-ring-max 32)
  (global-mark-ring-max 32)
  (kill-ring-max 128)
  (save-interprogram-paste-before-kill t)
  :config
  (progn
    ;; Do not use TABS
    (setq-default indent-tabs-mode nil)   ; TODO: why is this here?
    (put 'set-goal-column 'disabled nil)
  ))



;;; Enable some disabled commands

;; Give us narrowing back!
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
;; Same for region casing
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


(use-package delsel
  ;; typed text replaces the selection
  :config
  (delete-selection-mode))


(use-package paren
  ;; parenthesis matching
  :custom
  (show-paren-style 'mixed)
  :config
  (show-paren-mode))


;; dictionaries

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


(provide 'xorns-simple)
;;; xorns-simple.el ends here
