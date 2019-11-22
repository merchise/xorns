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


;;; Code:

(require 'use-package)
(require 'use-package-chords)
(require 'xorns-tools)


;; Discover more of Emacs. See http://t.co/IwZnrqQBRO
(require 'discover nil 'noerror)            ;
(when (functionp 'global-discover-mode)
  (global-discover-mode))


(use-package xorns-simple)
(use-package xorns-buffers)
(use-package xorns-dired)
(use-package xorns-git)
(use-package xorns-text)
(use-package xorns-prog)
(use-package xorns-project)


(when (not (bound-and-true-p >>=standalone-startup))
  ;; Main old modules
  (use-package xorns-term)
  (use-package xorns-org)
  ;; Previously in xorns-extra
  (use-package xorns-mail)
  )


(provide 'xorns-common-systems)
;;; xorns-common-systems.el ends here
