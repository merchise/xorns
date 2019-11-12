;;; xorns-simple.el --- Merchise basic editing commands for Emacs

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;;; Commentary:

;; Container for basic commands that are not related to any specific
;; major-mode.

;; Enjoy!


;;; Code:

(require 'use-package)
(require 'use-package-chords)
(require 'xorns-utils)


(use-package simple
  :defer t
  :init
  (progn
    (defun >>=kill-new (string)
      "Make not repeating STRING the latest kill in the kill ring."
      (unless (equal string (car kill-ring))
	(kill-new string)))

    (defun >>=yank-filename ()
      "Make buffer file-name the latest kill in the kill ring."
      (interactive)
      (>>=kill-new (or buffer-file-truename (buffer-name))))

    (defun >>=yank-default-directory ()
      "Make default directory the latest kill in the kill ring."
      (interactive)
      (>>=kill-new (>>=default-directory)))
    )
  :bind
  (("C-c k f" . >>=yank-filename)
   ("C-c k d" . >>=yank-default-directory)
   ;; deprecate next 2
   ("C-c M-w" . >>=yank-filename)
   ("C-c C-w" . >>=yank-default-directory))
  :custom
  (column-number-mode t)
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



;;; interactively do (ido)

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



;;; grep facilities

(use-package grep    ;; todo: check `wgrep', `scf-mode'
  :bind
  (("C-c C-g n" . find-name-dired)
   ("C-c C-g f" . find-grep)
   ("C-c C-g g" . grep)
   ("C-c C-g d" . find-grep-dired)
   ("C-c C-g r" . rgrep)
   ;; deprecate
   ("C-c r" . rgrep))
  :chords
  ("gn" . find-name-dired)
  ("gf" . find-grep)
  ("gg" . grep)
  ("gr" . rgrep)
  ("rr" . rgrep)
  :config
  (progn
    (dolist
      (type '(jpg jpeg png gif    ; images
	      mpg mjpg avi        ; videos
	      rar zip 7z))        ; archives
      (add-to-list 'grep-find-ignored-files (concat "*." (symbol-name type))))
    (dolist
      (name '(".tox" ".hypothesis" ".idea" ".mypy_cache" ".vscode"))
      (add-to-list 'grep-find-ignored-directories name))))



;;; Remote Access Protocol

(use-package tramp
  :config
  (defun >>=local-buffer (&optional buffer)
    "Not nil if BUFFER visits a local (not remote) file."
    (interactive "b")
    (not (tramp-connectable-p (buffer-file-name buffer)))))


(provide 'xorns-simple)
;;; xorns-simple.el ends here
