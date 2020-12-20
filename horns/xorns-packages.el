;;; xorns-packages.el --- Initialize `use-package' and all its dependencies  -*- lexical-binding: t -*-

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

;; Enjoy!


;;; Code:


(setq package-archives
  `(("melpa" . "https://melpa.org/packages/")
    ("org" . "https://orgmode.org/elpa/")
    ("gnu" . ,(format "http%s://elpa.gnu.org/packages/"
                (if (gnutls-available-p) "s" "")))))


(defun >>=package/ensure (package)
  "Ensure that PACKAGE is properly installed."
  (unless (package-installed-p package)
    (condition-case nil
      (package-install package)
      (error
	(progn
          (package-refresh-contents)
          (condition-case nil
	    (package-install package)
	    (error (message ">>= could not ensure '%s'" package))))))))


(defmacro >>=package/config (feature &rest body)
  "Ensure FEATURE is a properly installed package.
Similar to a `progn' special form but loading FEATURE before using `require'
of the eval of BODY forms sequentially and return value of last one."
  "Safe evaluate BODY forms sequentially and return value of last one.
Use a HEADER message when `init-file-debug' is t, or in case of error, to
report the identity of the enclosed body."
  `(condition-case-unless-debug err
     (progn
       (>>=package/ensure ',feature)
       (require ',feature)
       ,@body)
     (error
       (message ">>= error configuring feature '%s': %s" ',feature err))))


;; TODO: Check `system-packages', and `use-package-ensure-system-package'
(with-eval-after-load 'xorns-packages
  ;; Bootstrap 'use-package'
  (>>=package/config use-package
    ;; (setq use-package-always-ensure t)
    ))


(provide 'xorns-packages)
;;; xorns-packages.el ends here
