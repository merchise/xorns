;;; xorns-config.el --- Configure and load customization information file

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Load customization information file (`custom-file').  In `xorns', this file
;; is stored in the standard directory for user-specific configurations
;; (XDG_CONFIG_HOME), defaults to "~/.config/xorns".  If that folder does not
;; exist on your system, "~/.xorns" is used.

;; The first version of this file is copied from "templates/user-config".

;;; Code:

(require 'xorns-tools)


(with-eval-after-load 'xorns-config
  (if custom-file
    (warn ">>= `custom-file' already assigned: '%s'." custom-file)
    ;; else
    (require 'cus-edit)
    (setq custom-file (>>=-config-file-name))
    (let ((exists (file-exists-p custom-file))
	  save)
      (unless exists
	(setq exists (>>=-copy-from-template))
	(let ((old (>>=locate-user-emacs-file
		     "custom-${USER}.el" "custom.el")))
	  (when (file-exists-p old)
	    (message ">>= migrating old `custom-file' '%s'." old)
	    (load old (not init-file-debug))
	    (setq save t))))
      (when exists
	(load custom-file (not init-file-debug))
	(->? >>=settings/init))
      (if save
	(if exists
	  (let ((make-backup-files nil))
	    (message ">>= saving migrated variables.")
	    (custom-save-all))
	  ;; else
	  (warn (concat ">>= migrated variables not saved because a template "
		  "was not found to create the new style `custom-file'; "
		  "Fix config file manually.")))))))


(defun >>=-config-file-name ()
  "Return target location for `custom-file'."
  (let ((xdg (find-dir (getenv "XDG_CONFIG_HOME") (dir-join "~" ".config"))))
    (expand-file-name
      (if xdg "xorns" ".xorns")
      (or xdg "~"))))


(defun >>=-copy-from-template ()
  "Create new `custom-file' from template."
  (let ((template
	  (expand-file-name
	    "user-config"
	    (dir-join (bound-and-true-p >>=library-directory) "templates"))))
    (when (file-exists-p template)
      (copy-file template custom-file t)
      (message ">>= new `custom-file' '%s' has been created." custom-file)
      template)))


(provide 'xorns-config)
;;; xorns-config.el ends here
