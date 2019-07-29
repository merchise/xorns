;;; xorns-building-blocks.el --- Configure package system for Xorns

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module is installed just by calling `(require 'xorns-building-blocks)'
;; in the initialization process, which is done automatically.

;;; Code:


; (eval-when-compile
;  (require 'use-package))


(defun >>=building-blocks/load ()
  "Load configured building-blocks."
  ; There are several building-blocks that are always loaded
  (require 'xorns+base)
  (>>+base/init))


(provide 'xorns-building-blocks)
;;; xorns-building-blocks.el ends here
