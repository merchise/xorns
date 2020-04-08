;;; xorns-common-systems.el --- Xorns Configuration for Base System

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; It's installed just by calling `(require 'xorns-packages)' in the
;; initialization process, which is done automatically.
;;
;; Pending tasks:
;; - See alternatives for `make-backup-files' in:
;;   - https://www.emacswiki.org/emacs/Auto Save
;;   - https://www.emacswiki.org/emacs/BackupFiles

;; Enjoy!


;;; Code:

(require 'use-package)
(require 'xorns-tools)

(use-package xorns-simple)
(use-package xorns-buffers)
(use-package xorns-dired)
(use-package xorns-git)
(use-package xorns-text)
(use-package xorns-prog)
(use-package xorns-prog-extra)
(use-package xorns-project)
(use-package xorns-term)
(use-package xorns-pim)


(use-package xorns-mail
  ;; TODO: mail has not yet been migrated to standalone mode
  :when (bound-and-true-p >>=init-mode/package))


(provide 'xorns-common-systems)
;;; xorns-common-systems.el ends here
