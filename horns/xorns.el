;;; xorns.el --- Merchise extensions for Emacs

;; Copyright (c) Merchise Autrement [~º/~]
;; URL: https://github.com/merchise/xorns
;; Version: 0.5.3

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>
;; or type `C-h C-c' in Emacs.

;;; Commentary:

;; To use `xorns', and automatically include all its basic features,
;; just configure one of the standard initialization files (`~/.emacs'
;; or `~/.emacs.d/init.el') with the following body::
;;
;;     (package-initialize)
;;     (require 'xorns)
;;
;; There are some extra features that are not included in basic
;; `xorns'; if required, configure in the selected initialization
;; file::
;;
;;     (require 'xorns-extra)

;; Enjoy!


;;; Code:


(require 'package)
(require 'env)
(require 'xorns-utils)


;; TODO: (defvar xorns-version 'undefined
;; TODO: (defun xorns-version (&optional print-dest)

;;; TODO: Remove
;; (defun xorns-load-user-file (name)
;;   "Load user initialization file NAME."
;;   (let ((init-file
;;           (xorns-locate-emacs-file name nil)))
;;     (if init-file
;;       (load init-file 'noerror))))


;; TODO: (xorns-load-user-file "before-init-${USER}.el")


;; Basic initialization
(require 'use-package)
(require 'xorns-tools)
(require 'xorns-fonts)


(defvar >>=xorns-initialized nil
  "Whether or not Xorns has finished the startup process.
This is set to true when executing `emacs-startup-hook'.")


(defvar >>=|enable-server t
  "If non-nil, start an Emacs server if one is not already running.")


(defun >>=xorns/init ()
  "General startup initialization."
  ; (require 'xorns-config)
  (require 'xorns-migration)
  ; (->? >>=custom/user-init)
  ; TODO: load-default-theme
  (use-package xorns-ui
    :hook
    (after-init . spaceline-xorns-theme)
    :config
    (>>=frame-title-init))
  (>>=configure-font)
  ; (->? >>=units/configuration)
  (use-package xorns+base
    :config
    (>>=+base/init))
  ; (>>=setup-emacs-startup-hook)
  (when >>=|enable-server
    (require 'server)
    (unless (server-running-p)
      (message ">>= starting server...")
      (server-start)))
  )


(>>=xorns/init)

(use-package xorns-start)
(use-package xorns-buffers)
(use-package xorns-dired)
(use-package xorns-simple)
(use-package xorns-term)
(use-package xorns-fci)
; (use-package xorns-migration)
(use-package xorns-prog)        ;; This requires `xorns-text'
(use-package xorns-git)
(use-package xorns-project)
(use-package xorns-org)
(use-package xorns-xml)




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
