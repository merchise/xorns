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

(defvar >>=|ext/ripgrep "rg"
  "Whether `ripgrep' extensions must be configured.
Could be a boolean, or a string specifying the `ripgrep' command name, the
default value is \"rg\".  Usually this variable is used with the function
`>>=setup/command-check'.")


(defvar >>=|ext/fzf nil
  "Whether `fzf' extensions must be configured.
Could be a boolean, or a string specifying the `fzf' command name, the default
value is nil, but use \"fzf\" if you want to activate it.  Usually this
variable is used with the function `>>=setup/command-check'.")


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



;;; grep facilities


(use-package grep    ;; todo: check `wgrep', `scf-mode', `deadgrep'
  :demand t
  :bind
  (("C-c C-g n" . find-name-dired)
   ("C-c C-g f" . find-grep)
   ("C-c C-g g" . grep)
   ("C-c C-g d" . find-grep-dired)
   ("C-c r" . rgrep)
   ("C-c C-g r" . rgrep))
  :config
  (progn
    (dolist
      (type
        '(jpg jpeg png gif    ; images
          mpg mjpg avi        ; videos
          rar zip 7z))        ; archives
      (add-to-list 'grep-find-ignored-files (format "*.%s" type)))
    (dolist
      (name '(".tox" ".hypothesis" ".idea" ".mypy_cache" ".vscode"))
      (add-to-list 'grep-find-ignored-directories name))))


(use-package deadgrep
  :when (>>=setup/command-check >>=|ext/ripgrep)
  :ensure t
  :after grep
  :init
  (use-package rg
    :ensure t
    :init
    (defvar >>=|rg/max-columns 512
      "Override value for `--max-columns' option.")
    :bind
    ("C-c s" . rg-project)
    :config
    (when >>=|rg/max-columns
      (let ((max (format "--max-columns=%s" >>=|rg/max-columns)))
        (setq rg-command-line-flags (cons max rg-command-line-flags)))))
  :bind
  ([remap rgrep] . deadgrep))


(provide 'xorns-simple)
;;; xorns-simple.el ends here
