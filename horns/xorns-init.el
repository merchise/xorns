;;; xorns-init.el --- Xorns initialization as a module

;;; Commentary:
;;
;; Xorns definitions to initialize a Emacs session.

;;; Code:

(if (not (boundp 'emacs-start-time))
  (defconst emacs-start-time (current-time)
    "Define this in 'init.el' file first statement."))

; Check required minimum Emacs version

(let ((emacs-min-version "26.1"))
  (if (not (version<= emacs-min-version emacs-version))
    (error "This xorns version requires Emacs version >='%s'"
      emacs-min-version)))


; Setup package-system

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
  '(("gnu" . "http://elpa.gnu.org/packages/")
    ("melpa" . "https://melpa.org/packages/")
    ("org" . "https://orgmode.org/elpa/")))
(package-initialize)


; TODO: templates folder: package-unpack, package-user-dir
; (pkg-desc (cadr (assq pkg package-alist)))


; Change some variables to speed boost during 'init'
(let ((gc-cons-threshold 134217728)  ; (* 128 1024 1024)
      (gc-cons-percentage 0.6)
      (file-name-handler-alist nil))
  ; General initialization
  (require 'xorns-startup)
  (>>=xorns/init)
  (when >>=|enable-server
    (require 'server)
    (if >>=|server-socket-dir
        (setq server-socket-dir >>=|server-socket-dir))
      (unless (server-running-p)
        (message ">>= starting server...")
        (server-start)))
  (garbage-collect))


(defconst xorns-init-elapsed-time
  (float-time (time-subtract nil emacs-start-time))
    "Time (seconds) spent in Emacs initialization process.")


(provide 'xorns-init)
;;; xorns-init.el ends here
