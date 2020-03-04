;;; xorns-packages.el --- Initialize `use-package' and all its dependencies

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Must be required in the start of the initialization process just by calling
;; '(require 'xorns-packages)' in `xorns' main module.
;;
;; Pending tasks:
;; - Use `quelpa' and `quelpa-use-package'.
;; - See: (setq system-packages-noconfirm t)

;;; Code:


(defvar >>-package-contents-refreshed nil
  "If `package-refresh-contents' is already executed in this session.")


(defun >>=package-ensure (pkg)
  "Ensure PKG is installed."
  (unless (package-installed-p pkg)
    (unless >>-package-contents-refreshed
      (setq >>-package-contents-refreshed t)
      (package-refresh-contents))
    (package-install pkg)))


(defmacro >>=is-package-demanded (pkg domain-list)
  "Check if package PKG is in DOMAIN-LIST for configuration."
  `(when (memq ',pkg ,domain-list)
     (>>=package-ensure ',pkg)
     ',pkg))


(defmacro >>=ensure-packages (&rest packages)
  "Ensure that all PACKAGES are installed."
  `(dolist (pkg '(,@packages))
     (>>=package-ensure pkg)))


(defmacro >>=require (feature)
  "Ensure FEATURE is properly installed; and then `require' it."
  `(progn
     (>>=package-ensure ',feature)
     (require ',feature)))


(with-eval-after-load 'xorns-packages
  (>>=ensure-packages
    ; Bootstrap 'use-package' and dependencies
    use-package
    system-packages
    use-package-ensure-system-package
    ))


(provide 'xorns-packages)
;;; xorns-packages.el ends here
