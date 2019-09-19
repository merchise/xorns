;;; xorns-init.el --- Xorns initialization as a module

;;; Commentary:
;;
;; Xorns definitions to initialize a Emacs session.

;;; Code:


(defconst >>=start-time
  (or (bound-and-true-p emacs-start-time) (current-time))
  "Define this in 'init.el' file first statement.")


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
    (require 'xorns-extra)
    ; # TODO: Check next
    (autoload 'po-mode "po-mode"
      "Major mode for translators to edit PO files" t)
    )
  (garbage-collect))


(defconst xorns-init-elapsed-time
  (float-time (time-subtract nil >>=start-time))
    "Time (seconds) spent in Emacs initialization process.")


(provide 'xorns-init)
;;; xorns-init.el ends here
