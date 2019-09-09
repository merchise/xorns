;;; before-init.el --- Example Merchise Initialization File for local user

;; Copyright (C) Merchise Autrement [~º/~]

;; Author: Medardo Rodriguez <med@merchise.org>

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

;; TODO: Check this very well all these
(condition-case err
  (progn
    (require 'electric nil 'noerror)
    (electric-pair-mode t))
  (error (message "error@prog-mode-hook: %s" err)))


;; (custom-set-faces '(default ((t (:height 136)))))

;;; before-init.el ends here
