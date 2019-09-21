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
      (if (or (file-exists-p config-file) (>>=-create-new config-file))
	(progn
	  (setq custom-file config-file)
	  (load custom-file (not init-file-debug))
	  (->? >>=settings/init))
	;; else -- this must be removed
	(>>=-use-old)))
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


(defun >>=-use-old ()
  "Use old style custom file."
  (require 'xorns-utils)
  (let ((file-name
	  (xorns-locate-emacs-file "custom-${USER}.el" "custom.el")))
    (setq custom-file file-name)
    (if (file-exists-p custom-file)
      (progn
	(load custom-file (not init-file-debug))
	(message "Loading `custom-file': %s" file-name))
      ;; else
      (message "Using new `custom-file': %s" file-name))))


(provide 'xorns-config)
;;; xorns-config.el ends here
