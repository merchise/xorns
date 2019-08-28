;;; xorns-units.el --- Configure Building-Blocks (units) system

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; New-age (>>=) module.  Building-Blocks (units) are components, usually
;; optional, installed by user demands specified in the local site
;; configuration.  There are some default units (like `xorns+base',
;; `xorns+text', `xorns+vc', and `xorns+prog') that are always configured.

;;; Code:


(defun >>=units/load ()
  "Load configured units."
  ; There are several units that are always loaded
  (require 'xorns+base)
  (>>=+base/init))


(provide 'xorns-units)
;;; xorns-units.el ends here
