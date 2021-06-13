;;; xorns-simple.el --- Merchise basic editing commands for Emacs  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;;; Commentary:

;; Container for basic commands that are not related to any specific
;; major-mode.

;; Enjoy!


;;; Code:

(require 'use-package)
(require 'xorns-tools)
(require 'xorns-setup)



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



;;; Basic editing commands

(use-package discover    ; TODO: See http://t.co/IwZnrqQBRO
  :ensure t
  :init
  (global-discover-mode +1))


(defun >>=kill-new (string)
  "Make STRING the latest kill in the kill ring unless it is already there."
  (unless (equal string (car kill-ring))
    (kill-new string)))


(defun >>=yank-filename (&optional prefix)
  "Make buffer abbreviate file-name the latest kill in the kill ring.
Optional argument PREFIX controls whether the line-number must be included,
`C-u'; or the true name representation of the file-name, `C-0'; any other
value will combine both logics."
  (interactive "P")
  (let (name ln)
    (cond
      ((null prefix)
        (setq name buffer-file-name))
      ((consp prefix)
        (setq
          name buffer-file-name
          ln t))
      ((eq prefix 0)
        (setq name buffer-file-truename))
      (t
        (setq
          name buffer-file-truename
          ln t)))
    (>>=kill-new
      (if name
        (concat
          (abbreviate-file-name name)
          (if ln (format ":%s:" (line-number-at-pos)) ""))
        ;; else
        (or
          (bound-and-true-p exwm-title)
          (buffer-name))))))


(defun >>=yank-default-directory ()
  "Make default directory the latest kill in the kill ring."
  (interactive)
  (>>=kill-new (>>=default-directory)))


(defun >>=shell-command-to-string (command)
  "Execute shell COMMAND and return its output as a trimmed string."
  (string-trim (shell-command-to-string command)))


(defun >>=buffer-focused-text ()
  "Return focused-text in current buffer, selected region or current line."
  (let (begin end
        (region (use-region-p)))
    (if region
      (setq
        begin (point)
        end (mark))
      ;; else
      (save-restriction
        (widen)
        (save-excursion
          (beginning-of-line)
          (setq begin (point))
          (end-of-line)
          (setq end (point)))))
    (prog1
      (buffer-substring-no-properties begin end)
      (if region
        (setq deactivate-mark t)))))


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
  (put 'set-goal-column 'disabled nil))



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



;;; grep facilities

(use-package grep    ;; todo: check `wgrep', `scf-mode', `deadgrep'
  :demand t
  :bind
  (("C-c C-g n" . find-name-dired)
   ("C-c C-g f" . find-grep)
   ("C-c C-g g" . grep)
   ("C-c C-g d" . find-grep-dired)
   ("C-c C-g r" . rgrep)
   ;; deprecate
   ("C-c r" . rgrep))
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
  :bind
  ([remap rgrep] . deadgrep)
  )



;;; Remote Access Protocol

(use-package tramp
  :config
  (defun >>=local-buffer (&optional buffer)
    "Not nil if BUFFER visits a local (not remote) file."
    (interactive "b")
    (not (tramp-connectable-p (buffer-file-name buffer)))))


(provide 'xorns-simple)
;;; xorns-simple.el ends here
