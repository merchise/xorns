;;; xorns-search.el --- Manage and navigate projects in Emacs easily  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;;; Commentary:

;; Manage and navigate projects in Emacs easily using Merchise tools.

;; Enjoy!


;;; Code:

(eval-and-compile
  (require 'xorns-tools)
  (require 'use-package))



;;; grep facilities

(defvar >>=|ext/ripgrep "rg"
  "Whether `ripgrep' extensions must be configured.
Could be a boolean, or a string specifying the `ripgrep' command name, the
default value is \"rg\".  Usually this variable is used with the function
`>>=setup/command-check'.")


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


(provide 'xorns-search)
;;; xorns-search.el ends here
