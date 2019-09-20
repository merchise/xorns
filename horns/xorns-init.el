;;; xorns-init.el --- Xorns initialization as a module

;;; Commentary:
;;
;; Xorns definitions to initialize a Emacs session.

;;; Code:

(require 'xorns-preface)
(require 'xorns-packages)
(require 'use-package)


(let ((emacs-min-version "26.1"))
  (if (not (version<= emacs-min-version emacs-version))
    (error "This xorns version requires Emacs version >='%s'"
      emacs-min-version)))


(let ( ; speed boost during 'init'
      (gc-cons-threshold 134217728)    ; (* 128 1024 1024)
      (gc-cons-percentage 0.6)
      (file-name-handler-alist nil))
  (if (bound-and-true-p >>=standalone-startup)
    (use-package xorns-startup
      :commands >>=xorns/init
      :init
      (>>=xorns/init))
    ; else
    (require 'xorns)
    ; # TODO: Check next
    (autoload 'po-mode "po-mode"
      "Major mode for translators to edit PO files" t)
    )
  (garbage-collect))


(provide 'xorns-init)
;;; xorns-init.el ends here
