;;; xorns.el --- Merchise extensions for Emacs

;; Copyright (c) Merchise Autrement [~º/~]
;; URL: https://github.com/merchise/xorns
;; Version: 0.5.4

;;; Commentary:

;; To use `xorns', just require its main module in the user’s initialization
;; file:
;;
;;   (require 'xorns)
;;
;; We advise to use our version of `user-init-file'.


;; Enjoy!


;;; Code:

(require 'xorns-preface)
(require 'xorns-packages)
(require 'use-package)


(let ((emacs-min-version "26.1"))
  (unless (version<= emacs-min-version emacs-version)
    (error "This `xorns' version requires Emacs >='%s'" emacs-min-version)))



;; Configuration Variables

(defvar >>=xorns-initialized nil
  "Whether or not Xorns has finished the startup process.
This is set to true when executing `emacs-startup-hook'.")


(defvar >>=|enable-server t
  "If non-nil, start an Emacs server if one is not already running.")



;; Configuration functions

(defun >>=xorns/init ()
  "General startup initialization."
  (require 'xorns-config)
  (->? >>=custom/user-init)
  ; TODO: load-default-theme
  (use-package xorns-ui
    :hook
    (after-init . spaceline-xorns-theme)
    :config
    (>>=frame-title-init))
  (use-package xorns-fonts
    :config
    (>>=configure-font))
  (->? >>=units/configuration)
  (use-package xorns-base)
  (>>=setup-emacs-startup-hook)
  (when >>=|enable-server
    (require 'server)
    (unless (server-running-p)
      (message ">>= starting server...")
      (server-start))))


(defun >>=setup-emacs-startup-hook ()
  "Add post initializtion processing."
  (add-hook
   'emacs-startup-hook
   (defun >>=startup-hook ()
     (->? >>=user-config)
     ; TODO: initialize-custom-file-sync
     (setq >>=xorns-initialized (emacs-init-time))
     (message ">>= xorns initialized in %s seconds." >>=xorns-initialized))))



;; Main initialization

(let ((gc-cons-threshold 134217728)    ; (* 128 1024 1024)
      (gc-cons-percentage 0.6)
      (file-name-handler-alist nil))
  ; these local variables speed boost during initialization
  (>>=xorns/init)
  (garbage-collect))





;; Basic initialization

(require 'xorns-utils)
(require 'xorns-tools)

(use-package xorns-start)
(use-package xorns-buffers)
(use-package xorns-dired)
(use-package xorns-simple)
(use-package xorns-term)
(use-package xorns-fci)
(use-package xorns-prog)        ;; This requires `xorns-text'
(use-package xorns-git)
(use-package xorns-project)
(use-package xorns-org)
(use-package xorns-xml)

;; Previously in xorns-extra
(use-package xorns-mail)
(use-package xorns-gud)
(use-package xorns-ido)




(defun xorns-manage-user-custom-files (&optional force)
  "Configure and load per-user custom initialization.

This is useful when a GIT repository for `~/.emacs.d/' folder is shared to be
used for several team members in order to each one could have his/her own
`custom-file' using as name the pattern `custom-${USER}.el'.

If `custom-file' variable is configured when this function runs, a proper
warning is issued and no file is configured unless optional argument FORCE
is given."
  (let* ((configured custom-file)
          (do-config (or (not configured) force)))
    (if configured
      (message
        "Warning: A `custom-file' \"%s\" is already configured!"
        custom-file))
    (if do-config
      (let ((file-name
              (xorns-locate-emacs-file "custom-${USER}.el" "custom.el")))
        (setq custom-file file-name)
        (if (file-exists-p custom-file)
          (progn
            (load custom-file 'noerror)
            (message "Loading `custom-file': %s" file-name))
                                        ;else
          (message "Using new `custom-file': %s" file-name))))))


(xorns-manage-user-custom-files)

;; TODO: (xorns-load-user-file "after-init-${USER}.el")


(provide 'xorns)
;;; xorns.el ends here
