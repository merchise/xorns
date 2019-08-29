;;; xorns-packages.el --- Initialize `use-package' and all its dependencies

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; New-age (>>=) module.  Must be required in the start of the initialization
;; process just by calling '(require 'xorns-packages)' in `xorns-startup'.
;;
;; Pending tasks:
;; - Use `quelpa' and `quelpa-use-package'.

;;; Code:

(require 'package)

; Bootstrap 'use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package diminish
  :ensure t)

(use-package delight
  :ensure t)

(use-package use-package-chords
  :ensure t)

(use-package system-packages
  :ensure t
  :custom
  (system-packages-noconfirm t))

(use-package use-package-ensure-system-package
  :ensure t)


(provide 'xorns-packages)
;;; xorns-packages.el ends here
