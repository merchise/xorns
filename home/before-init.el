;;; before-init.el --- Example Merchise Initialization File for local user

;; Copyright (C) 2014 Merchise Autrement

;; This program is free software (GPL v3 or any later).
;; Type `C-h C-c' in Emacs to see full licence.

;;; Commentary:

;; All files in this directory are examples to be sym-linked or copied as base
;; in "~/.emacs.d/".  This one is a initialization file that runs before
;; `xorns'.

;; The name in "~/.emacs.d/" could be created like:
;;   $ ln -sf ~/work/src/xorns/home/before-init.el before-init-med.el

;; Enjoy!

;;; Code:

;; Remove "--group-directories-first" in Mac
;; '(dired-listing-switches "-l --group-directories-first -h")


(setq
  user-mail-address (substitute-env-vars "${USER}@merchise.org")
  split-width-threshold 120)

;; TODO: Check this very well all these
(condition-case err
  (progn
    (require 'electric nil 'noerror)
    (electric-pair-mode t))
  (error (message "error@prog-mode-hook: %s" err)))


;; TODO: Check all these
(set-variable 'clean-buffer-list-delay-general 1)
(set-variable 'clean-buffer-list-delay-special 900)
(set-variable 'dired-isearch-filenames t)
(set-variable 'dired-isearch-filenames-regexp t)
(set-variable 'doc-view-continuous t)
(set-variable 'inferior-lisp-program "clisp")
(set-variable 'list-command-history-max 128)
(set-variable 'term-input-autoexpand t)
(set-variable 'wdired-allow-to-change-permissions t)


; (setq muse-project-alist '(("planner" ("~/.pim/planner/"))))


;;; before-init.el ends here
