;;; xorns-base.el --- Xorns Configuration for Base System

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Initialization functions for required base system packages.  All
;; sub-mpdules can be configured at once using the function `>>=+init-base'.

;;; Code:


(require 'use-package)


(defun >>=+init-base ()
  "Initialize all sub-modules of base system."
  (>>=+init-base-package-system)
  )



;; Package System

; TODO: see 'kaushalmodi/setup-packages.el:129'

; TODO: (use-package no-littering)

(defun >>=+init-base-package-system ()
  "Initialize the Emacs package system basic extensions."
  (use-package diminish
    :ensure t)
  (use-package use-package-chords
    :ensure t
    :config
    (key-chord-mode 1))
  (use-package system-packages
    :ensure t
    :custom
    (system-packages-noconfirm t))
  (use-package use-package-ensure-system-package
    :ensure t)
  (use-package quelpa
    :ensure t
    :defer t
    :custom
    (quelpa-update-melpa-p nil "Don't update the MELPA git repo."))
  (use-package quelpa-use-package
    :ensure t)
  )


(provide 'xorns-base)
;;; xorns-base.el ends here
