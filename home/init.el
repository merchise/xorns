;;; init.el --- Merchise Initialization File

;; Copyright (C) Merchise Autrement [~º/~]

;; Author: Medardo Rodriguez <med@merchise.org>
;; URL: http://dev.merchise.org/emacs/xorns

;; This program is free software (GPL v3 or any later).
;; Type `C-h C-c' in Emacs to see full licence.

;;; Commentary:

;; All files in this directory are examples to be sym-linked or copied as base
;; in "~/.emacs.d/".  This one is a normal initialization file to use `xorns'.

;; Enjoy!

;;; Code:

(require 'package)
(package-initialize)

(require 'use-package)

(let ((xorns-config-level 'maximum))    ;; 'basic, 'general
  ;; Start the server and initialize all common Merchise settings.
  (require 'xorns)
  ;; Require extra features
  (require 'xorns-extra))

(let ((proof-general "~/.emacs.d/lisp/PG/generic/proof-site"))
  (when (file-directory-p proof-general)
    (load proof-general)))

(when (null (functionp 'agda-mode))
  (-when-let* ((agda-mode (xorns-executable-find "agda-mode"))
               (agda-locate (concat agda-mode " locate"))
               (coding-system-for-read 'utf-8))
    (load-file (shell-command-to-string agda-locate))))


(autoload 'po-mode "po-mode"
          "Major mode for translators to edit PO files" t)


(provide 'init)
;;; init.el ends here
