;;; xorns-packages.el --- Initialize `use-package' and all its dependencies  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Must be required in the start of the initialization process just by calling
;; '(require 'xorns-packages)' in `xorns' main module.
;;
;; TODO:
;; - Use `quelpa' and `quelpa-use-package'.
;; - See: (setq system-packages-noconfirm t)

;; Enjoy!


;;; Code:

(eval-and-compile
  (require 'package))


(setq package-archives
  `(("melpa" . "https://melpa.org/packages/")
    ("org" . "https://orgmode.org/elpa/")
    ("gnu" . ,(format "http%s://elpa.gnu.org/packages/"
                (if (gnutls-available-p) "s" "")))))


(defun >>=package/ensure (package)
  "Ensure that PACKAGE is properly installed."
  (declare (obsolete use-package "0.11.5"))
  (unless (require package nil 'noerror)
    (condition-case nil
      (package-install package)
      (error
        (progn
          (package-refresh-contents)
          (condition-case nil
            (package-install package)
            (error (message ">>= could not ensure '%s'" package))))))))


(defmacro >>=package/config (feature &rest body)
  "Safe evaluate BODY forms sequentially and return value of last one.
Similar to a `progn' special form but loading FEATURE before using `require'
of the eval of BODY forms sequentially and return value of last one."
  (declare (obsolete use-package "0.11.5"))
  `(condition-case-unless-debug err
     (progn
       (>>=package/ensure ',feature)
       (require ',feature)
       ,@body)
     (error
       (message ">>= error configuring feature '%s': %s" ',feature err))))


(defmacro >>=ensure-packages (&rest packages)
  "Ensure that all PACKAGES are installed."
  (declare (obsolete use-package "0.11.5"))
  `(dolist (pkg '(,@packages))
     (>>=package/ensure pkg)))


(provide 'xorns-packages)
;;; xorns-packages.el ends here
