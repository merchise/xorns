;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Starting with version 27, Emacs is configured using an early init file.  It
;; is loaded before graphical elements such as the toolbar are initialized,
;; and before the package manager is initialized.  Earlier versions will not
;; load this file and do not implicitely call `package-initialize' before
;; loading the init file.
;;
;; NOTE: We are not currently testing Xorns for Emacs < 27, so some
;; optimization processes may not work properly.


;;; Code

;; Improve startup time by temporarily increasing these values to prevent
;; garbage collection from running.  When the initialization is complete, the
;; `xorns-gc' module must be executed to configure adequate values for the
;; normal operation of the system, if this is not done, crashes or stutters
;; may occur.
(setq
  gc-cons-threshold #x40000000    ; 1 GB
  gc-cons-percentage 0.6)


(provide 'early-init)
;;; early-init.el ends here
