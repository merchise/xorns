;;; xorns-config.el --- Configure and load customization information file  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file contains the code needed to configure and load user options.
;; These options are settled in a file (see `custom-file' variable) stored in
;; the operating system standard directory for user-specific configurations
;; (XDG_CONFIG_HOME), or in in the user home.  In the first case it defaults
;; to "~/.config/xorns", in the second "~/.xorns" is used.
;;
;; There are also tools to configure the 'xorn' package in standalone mode.

;;; Code:

(require 'xorns-tools)
(require 'warnings)



;;; configuration file

(with-eval-after-load 'xorns-config
  (if custom-file
    (warn ">>= `custom-file' already assigned: '%s'." custom-file)
    ;; else
    (require 'cus-edit)
    (setq custom-file (>>-config-file-name))
    (let ((exists (file-exists-p custom-file))
          save)
      (unless exists
        (setq exists (>>-copy-from-template))
        (let ((old (>>=locate-user-emacs-file
                     "custom-${USER}.el" "custom.el")))
          (when (file-exists-p old)
            (message ">>= migrating old `custom-file' '%s'." old)
            (>>=load old)
            (setq save t))))
      (when (and (not init-file-debug) (eq warning-minimum-level :warning))
        (setq
          warning-minimum-level :error
          warning-minimum-log-level :error))
      (when exists
        (>>=load custom-file)
        (->? >>=settings/init))
      (when save
        (if exists
          (let ((make-backup-files nil))
            (message ">>= saving migrated variables.")
            (custom-save-all))
          ;; else
          (warn (concat ">>= migrated variables not saved because a template "
                  "was not found to create the new style `custom-file'; "
                  "Fix config file manually.")))))))


(defsubst >>-xdg-config-home ()
  "Get XDG configuration directory."
  (>>=find-dir
    (getenv "XDG_CONFIG_HOME")
    (>>=dir-join "~" ".config")))


(defun >>-config-file-name ()
  "Return target location for `custom-file'."
  (let ((xdg (>>-xdg-config-home)))
    (expand-file-name
      (if xdg "xorns" ".xorns")
      (or xdg "~"))))


(defun >>-copy-from-template ()
  "Create new `custom-file' from template."
  (let ((template
          (expand-file-name
            "user-config"
            (>>=dir-join
              (>>=value-of >>=!xorns/lib-dir) "templates"))))
    (when (file-exists-p template)
      (copy-file template custom-file t)
      (message ">>= new `custom-file' '%s' has been created." custom-file)
      template)))


(provide 'xorns-config)
;;; xorns-config.el ends here
