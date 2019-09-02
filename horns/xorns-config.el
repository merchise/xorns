;;; xorns-config.el --- Xorns Initialization Library

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; New-age (>>=) module.  This library defines several utility functions used
;; in Emacs main initialization process.  Including all definitions for user
;; custom configuration.

;;; Code:

(require 'xorns-tools)


(with-eval-after-load 'xorns-config
  (>>=user-config/load))




(defun >>=user-config/load ()
  "Load user private configuration init file if it exists.

Looks first for the configurations system directory ($XDG_CONFIG_HOME,
defaults to '~/.config/'), if it does not exist, use the user's $HOME
directory ('~').  The file-name in the destination folder will be
'xorns' (without the '.el' extension), but when the $HOME user directory is
used, it is prefixed with a dot ('.')."
  (let*
    ((xdg-config-home
       ; default directory for user configurations in POSIX systems
       (find-dir (getenv "XDG_CONFIG_HOME") (dir-join "~" ".config")))
     (config-home (or xdg-config-home "~"))
     (name (if xdg-config-home "xorns" ".xorns"))
     (config-file (expand-file-name name config-home)))
    (if (not (file-exists-p config-file))
      (>>=-user-config//create-from-template config-file))
    (if (not custom-file)
      (progn
	(setq custom-file config-file)
	(load custom-file)
	(->? >>=settings/init)
	config-file)
      ; else
      (warn ">>= custom-file '%s' was already assigned" custom-file))))


(defun >>=-user-config//create-from-template (target)
  "Ensure user configuration file properly exists in TARGET location."
  (let* ((dir (file-name-directory target))
	 (pkg-dir (or (bound-and-true-p >>=standalone-startup)
		    (>>=-package-directory)))
	 (template-location (file-expand "custom.el" pkg-dir "templates")))
    (if (file-exists-p template-location)
      (progn
	(if (not (file-directory-p dir))
	  (make-directory dir 'parents))
	(copy-file template-location target t)
	(message ">>= %s has been installed." target))
      ; else
      (error ">>= no template file in '%s'" template-location))))


(defun >>=-package-directory ()
  "Return xorns package installation."
  (require 'package)
  (let* ((pkg-desc (cadr (assq 'xorns package-alist)))
         (dirname (package-desc-full-name pkg-desc))
         (pkg-dir (dir-join package-user-dir dirname)))
    pkg-dir))


(provide 'xorns-config)
;;; xorns-config.el ends here
