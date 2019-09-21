;;; xorns.el --- Merchise extensions for Emacs

;; Copyright (c) Merchise Autrement [~º/~]
;; URL: https://github.com/merchise/xorns
;; Version: 0.5.5

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



;; Main initialization

(let ((gc-cons-threshold 134217728)    ; (* 128 1024 1024)
      (gc-cons-percentage 0.6)
      (file-name-handler-alist nil))
  ;; these local variables speed boost during initialization
  (require 'xorns-config)
  (use-package xorns-ui
    :commands >>=frame-title-init
    :hook
    (after-init . spaceline-xorns-theme)
    :config
    (>>=frame-title-init))
  (use-package xorns-fonts
    :commands >>=configure-font
    :init
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
      (server-start)))
  (garbage-collect))



(when (not (bound-and-true-p >>=standalone-startup))
  ;; Discover more of Emacs. See http://t.co/IwZnrqQBRO
  (require 'discover nil 'noerror)            ;
  (when (functionp 'global-discover-mode)
    (global-discover-mode))
  ;; Main old modules
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
  )


(provide 'xorns)
;;; xorns.el ends here
