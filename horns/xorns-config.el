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
  (if custom-file
    (warn ">>= `custom-file' already assigned with value '%s'." custom-file)
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
	    (message ">>= loading old custom-file '%s' this first time." old)
	    (load old (not init-file-debug))
	    (setq save t))))
      (when exists
	(load custom-file (not init-file-debug))
	(->? >>=settings/init))
      (if save
	(if exists
	  (progn
	    (message ">>= ensure saving imported old variables.")
	    (custom-save-all))
	  ;; else
	  (warn (concat ">>= old variables not saved because new custom-file "
		  "couldn't be configured from a template; this error must "
		  "be fixed by hand.")))))))


(defun >>=-package-user-dir ()
  "Directory containing installed xorns package."
  (dir-join
    package-user-dir
    (package-desc-full-name (cadr (assq 'xorns package-alist)))))


(defun >>=-template-location ()
  "Return base template location for `custom-file'."
  (require 'package)
  (expand-file-name
    "custom.el"
    (dir-join
      (or (bound-and-true-p >>=standalone-startup) (>>=-package-user-dir))
      "templates")))


(defun >>=-config-file-name ()
  "Return target location for `custom-file'."
  (let ((xdg (find-dir (getenv "XDG_CONFIG_HOME") (dir-join "~" ".config"))))
    (expand-file-name
      (if xdg "xorns" ".xorns")
      (or xdg "~"))))


(defun >>=-copy-from-template ()
  "Create new `custom-file' from template."
  (let ((template (>>=-template-location)))
    (when (file-exists-p template)
      (copy-file template custom-file t)
      (message ">>= new `custom-file' '%s' has been created." custom-file)
      template)))


(provide 'xorns-config)
;;; xorns-config.el ends here
