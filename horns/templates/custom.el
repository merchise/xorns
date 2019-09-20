;;; Xorns file for local user configurations -*- mode: emacs-lisp -*-

;;; Code:

(defun >>=settings/init ()
  "Configuration code for user-settings.
This function is called at the very beginning of the startup process.  It
should only modify setting-variables (those prefixed with '>>=|') when their
default values are not suitable for your configuration."
  (setq-default
    ;; >>=|default-font '(:size 12.0 :weight normal :width normal)
    ;; >>=|make-backup-files t
    ;; >>=|user-mail-address-template "${USER}@gmail.com"
    ;; >>=|show-title-in-header-line t
    ))


(defun >>=units/configuration ()
  "Configuration code for building-blocks customization.
This function should only modify variables to customize how building-blocks
are loaded (those prefixed with '>>=+') when their default values are not
suitable for your configuration."
  (setq-default
    ;; >>=|base/extra-packages '(autorevert recentf gcmh)
    ))


(defun >>=user-conde ()
  "User-code executed after initialization process."
  )



;; Do not write anything past this comment.  This is where Emacs will
;; auto-generate custom variable definitions.
