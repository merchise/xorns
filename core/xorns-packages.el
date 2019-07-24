;;; xorns-packages.el --- Configure package system for Xorns

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module is installed just by calling `(require 'xorns-packages)' in the
;; initialization process, which is done automatically.

;;; Code:


(eval-when-compile
  (require 'use-package))

(use-package diminish
  :ensure t)

(use-package delight
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


(provide 'xorns-packages)
;;; xorns-packages.el ends here
