;;; init.el --- Merchise Initialization File

;; Copyright (C) 2014-2016 Merchise Autrement [~º/~]

;; Author: Medardo Rodriguez <med@merchise.org>
;; URL: http://dev.merchise.org/emacs/xorns
;; Keywords: initialization, merchise, convenience
;; Version: 20150516.1620

;; This program is free software (GPL v3 or any later).
;; Type `C-h C-c' in Emacs to see full licence.

;;; Commentary:

;; All files in this directory are examples to be sym-linked or copied as base
;; in "~/.emacs.d/".  This one is a normal initialization file to use `xorns'.

;; Enjoy!

;;; Code:

;; Activate ELPA packages.
(load "~/.emacs.d/elpa/nxml-mode-20041004/rng-auto.el")

(package-initialize)

;; Q: Why this is needed?
;; A: Because new magit versions check this to show a rather long message
;;    warning about changes from the old magit.
(setq magit-last-seen-setup-instructions "1.4.0")

(let ((xorns-config-level 'maximum))    ;; 'basic, 'general
  ;; Start the server and initialize all common Merchise settings.
  (require 'xorns)
  ;; Require extra features
  (require 'xorns-extra))

;;; init.el ends here
