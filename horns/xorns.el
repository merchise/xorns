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
  (use-package xorns-ui
    :commands >>=frame-title-init
    :hook
    (after-init . spaceline-xorns-theme)
    :config
    (>>=frame-title-init))
  (use-package xorns-fonts
    :commands >>=configure-font
    :config
    (>>=configure-font))
  (->? >>=building-blocks/configuration)
  (use-package xorns-base)
  (add-hook
    'emacs-startup-hook
    (defun >>=startup-hook ()
      (->? >>=user-conde)
      ;; TODO: initialize-custom-file-sync
      (setq >>=xorns-initialized (emacs-init-time))
      (message ">>= xorns initialized in %s seconds." >>=xorns-initialized)))
  (when >>=|enable-server
    (require 'server)
    (unless (server-running-p)
      (message ">>= starting server...")
      (server-start))))



;; Main initialization

(let ((gc-cons-threshold 134217728)    ; (* 128 1024 1024)
      (gc-cons-percentage 0.6)
      (file-name-handler-alist nil))
  ; these local variables speed boost during initialization
  (>>=xorns/init)
  (garbage-collect))



;; Old initialization

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

;; Fix dead characters
;; https://wiki.archlinux.org/index.php/Emacs#Dead-accent_keys_problem:_.27.3Cdead-acute.3E_is_undefined.27

(use-package iso-transl
  :demand t
  :config
  (define-key key-translation-map (kbd "M-[") 'iso-transl-ctl-x-8-map))


;; Previously in xorns-extra

(use-package xorns-mail)
(use-package xorns-gud)


(provide 'xorns)
;;; xorns.el ends here
