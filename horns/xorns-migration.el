;;; xorns-migration.el --- Xorns Configuration for Base System

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This is a transitional module to migrate from past-time xorns, to the
;; new-age.

;;; Code:


(eval-when-compile
  (require 'use-package))
(require 'xorns-tools)



;; Automatic parenthesis pairing

(use-package elec-pair
  :demand t
  :config
  (electric-pair-mode t))    ; TODO: Check `custom-set-variables' for user


;; Get back font anti-aliasing
(push '(font-backend xft x) default-frame-alist)


(provide 'xorns-migration)
;;; xorns-migration.el ends here
