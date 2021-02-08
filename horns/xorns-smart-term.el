;;; xorns-smart-term.el --- Building-block for smart terminals  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;;; Commentary:

;; A new smart terminal can be defined with the macro `>>=define-terminal'
;; using several optional keyword arguments: the command to execute as a
;; shell, a function to paste text between ordinary buffers and terminals, a
;; list of `major-mode' symbols to associate a terminal kind with buffers
;; under these modes.  Example of terminals are `>>=main-term' (the default
;; terminal), and `>>=python-term' (for Python modes).

;; The function `>>=terminal' orchestrates all terminal kinds based on their
;; associations with major modes.  Each terminal kind can trigger several
;; tabs, each one is identified with a zero (the default) or positive integer.
;; To select a terminal tab a prefix argument is used.  A negative value is
;; used to execute a paste operation from the current buffer to a target
;; terminal (using the absolute value).  The `universal-argument' (`C-u') is
;; used to paste to the default tab, and `negative-argument' (`C--') for the
;; tab with index zero.

;; Every time a terminal is triggered, a reference to the current buffer is
;; linked to that tab, executing a command to select an active tab will
;; switch, or paste, to the linked buffer.

;; Enjoy!


;;; Code:

(require 'term)
(require 'xorns-tools)


;;; Variables

(defvar >>=|default-shell-file-name
  (purecopy
    (or
      explicit-shell-file-name
      shell-file-name
      (>>=executable-find (getenv "ESHELL") (getenv "SHELL") "bash" "zsh")))
  "System default shell file-name.")


(defvar >>-term/state nil
  "Terminal state (local variable in terminal buffers).")


(defvar >>-term/linked nil
  "Linked tab-index (local variable in buffers that trigger a terminal).")


(defvar >>-term-modes nil
  "Association-list mapping major modes to smart terminals.")


(provide 'xorns-smart-term)
;;; xorns-smart-term.el ends here
