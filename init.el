;;; init.el --- The Emacs Initialization File for Xorns

;;; Commentary:
;; The file that starts all our own way to Emacs

;;; Code:

;; Without this comment emacs25 adds (package-initialize) here
;; (package-initialize)

(defconst emacs-start-time (current-time))

(defconst >>=!base-directory
  (file-name-directory (or load-file-name default-directory)))

(defconst >>=!code-directory
  (concat >>=!base-directory (file-name-as-directory "lisp")))

(add-to-list 'load-path >>=!code-directory)

(require 'xorns-versions)

; Change some variables to speed boost during 'init'
(let ((gc-cons-threshold 134217728)  ; (* 128 1024 1024)
      (gc-cons-percentage 0.6)
      (file-name-handler-alist nil))
  (require 'xorns-startup)
  (>>=xorns/init)
  (>>=setup-emacs-startup-hook)
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


(provide 'init)
;;; init.el ends here
