;;; xorns-config.el --- Configure and load customization information file

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Load customization information file (`custom-file').  In `xorns', this file
;; is stored in the standard directory for user-specific configurations
;; (XDG_CONFIG_HOME), defaults to "~/.config/xorns".  If that folder does not
;; exist on your system, "~/.xorns" is used.
;;
;; The first version of this file is copied from "templates/custom.el".  If
;; this file is not found, `custom-file' variable maintains its default value.

;;; Code:

(require 'xorns-tools)


(with-eval-after-load 'xorns-config
  (if (not custom-file)
    (let ((config-file (>>=-config-file-name)))
      (setq custom-file
	(if (or (file-exists-p config-file) (>>=-create-new config-file))
	  config-file
	  ;; else
	  (>>=locate-user-emacs-file "custom-${USER}.el" "custom.el")))
      (if (file-exists-p custom-file)
	(load custom-file (not init-file-debug))))
    ;; else
    (warn ">>= `custom-file' already assigned with value '%s'." custom-file)))


(defun >>=-template-location ()
  "Return base template location for `custom-file'."
  (require 'package)
  (file-expand
    "custom.el"
    (or
      (bound-and-true-p >>=standalone-startup)
      (dir-join
	package-user-dir
	(package-desc-full-name (cadr (assq 'xorns package-alist)))))
    "templates"))


(defun >>=-config-file-name ()
  "Return target location for `custom-file'."
  (let ((xdg (find-dir (getenv "XDG_CONFIG_HOME") (dir-join "~" ".config"))))
    (expand-file-name
      (if xdg "xorns" ".xorns")
      (or xdg "~"))))


(defun >>=-create-new (target)
  "Create new `custom-file' in TARGET from template."
  (let ((template (>>=-template-location)))
    (if (file-exists-p template)
      (progn
	(copy-file template target t)
	(message ">>= new `custom-file' '%s' has been created." target)
	template)
      ;; else
      (message ">>= `custom-file' template '%s' does not exist." template)
      nil)))


(provide 'xorns-config)
;;; xorns-config.el ends here
