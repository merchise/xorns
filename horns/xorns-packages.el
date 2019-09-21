;;; xorns-packages.el --- Initialize `use-package' and all its dependencies

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; New-age (>>=) module.  Must be required in the start of the initialization
;; process just by calling '(require 'xorns-packages)' in `xorns' main module.
;;
;; Pending tasks:
;; - Use `quelpa' and `quelpa-use-package'.
;; - See: (setq system-packages-noconfirm t)

;;; Code:

(defvar >>=package-contents-refreshed nil
  "If `package-refresh-contents' is already executed in this session.")


(with-eval-after-load 'xorns-packages
  (>>=ensure-packages
    ; Bootstrap 'use-package' and dependencies
    'use-package
    'diminish
    'use-package-chords
    'system-packages
    'use-package-ensure-system-package
    ; Bootstrap 'UI' dependencies
    'minions
    'spaceline
    )
  (use-package use-package-chords
    :config (key-chord-mode 1)))


(defun >>=ensure-packages (&rest packages)
  "Ensure all PACKAGES are installed."
  (dolist (pkg packages)
    (unless (package-installed-p pkg)
      (unless >>=package-contents-refreshed
	(package-refresh-contents)
	(setq >>=package-contents-refreshed t))
      (package-install pkg))))


(provide 'xorns-packages)
;;; xorns-packages.el ends here
