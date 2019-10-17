;;; xorns-simple.el --- Merchise basic editing commands for Emacs

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;;; Commentary:

;; Basic Emacs commands not specifically related to any specific major mode
;; or to file-handling.

;; This module is automatically used when::
;;
;;     (require 'xorns)

;; Enjoy!


;;; Code:



(use-package simple
  :defer t
  :config
  ;; re-enable this command
  (put 'set-goal-column 'disabled nil))


;;; Enable some disabled commands

;; Give us narrowing back!
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; Same for region casing
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;; Typed text replaces the selection
(delete-selection-mode 1)


;; parenthesis matching

(use-package paren
  :custom
  (show-paren-style 'mixed)
  :config
  (show-paren-mode))


;; dictionaries

(use-package ispell
  :bind
  (("C-c i d" . ispell-change-dictionary)
   ("C-c i l" . ispell-change-dictionary)
   ("C-c i r" . ispell-region)
   ("C-c i b" . ispell-buffer)
   ("C-c i c" . ispell-comments-and-strings)
   ("C-c i k" . ispell-kill-ispell)
   ("C-c i m" . ispell-message))
  :custom
  (ispell-highlight-p t)
  (ispell-silently-savep t)
  (ispell-dictionary "english"))



;;; Some simple functions

(defun -set-buffer-read-only ()
  "Private function to be used in `xorns-next-grep-result'."
  (setq-default buffer-read-only t))


(defun xorns-next-grep-result (&optional arg reset)
  "Visit next grep result.

If no grep process is active, find next error in occur buffer.

A prefix ARG specifies how many error messages to move; negative means move
back to previous error messages.  Just \\[universal-argument] as a prefix
means reparse the error message buffer and start at the first error.
;;; interactively do (ido)

(defun xorns-yank-filename ()
  "Make buffer file-name the latest kill in the kill ring."
  (interactive)
  (save-excursion
    (kill-new (or buffer-file-truename (buffer-name)))))



(defun xorns-yank-default-directory ()
  "Make default directory the latest kill in the kill ring."
  (interactive)
  (save-excursion
    (kill-new default-directory)))
(use-package ido
  :bind
  ("C-x b" . ido-switch-buffer)
  :chords
  ("xb" . ido-switch-buffer)
  :custom
  (ido-auto-merge-work-directories-length -1)
  (ido-enable-flex-matching t)
  (ido-auto-merge-delay-time 1.5)
  :config
  (ido-mode 1))


;;; Custom key-bindings

(global-set-key (kbd "C-c r") 'rgrep)
(global-set-key (kbd "C-M-g") 'xorns-next-grep-result)
(define-key global-map (kbd "C-c M-w") 'xorns-yank-filename)
(define-key global-map (kbd "C-c C-w") 'xorns-yank-default-directory)


(provide 'xorns-simple)
;;; xorns-simple.el ends here
