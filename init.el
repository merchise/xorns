;;; init.el --- The Emacs Initialization File for Xorns

;;; Commentary:
;; The file that starts all our own way to Emacs

;;; Code:

;; Without this comment emacs25 adds (package-initialize) here
;; (package-initialize)

(defconst emacs-start-time (current-time))

; Setup package-system

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
  '(("gnu" . "http://elpa.gnu.org/packages/")
    ("melpa" . "https://melpa.org/packages/")
    ("org" . "https://orgmode.org/elpa/")))
(package-initialize)


; Check required minimum Emacs version

(let ((emacs-min-version "26.1"))
  (if (not (version<= emacs-min-version emacs-version))
    (error "This xorns version requires Emacs version >='%s'"
      emacs-min-version)))


; Define how xorns is being used

(defconst >>=standalone-mode
  (not (package-installed-p 'xorns))
  "Not-nil when started in standalone mode.

If `xorns' is installed as a standard `package', it will be used without the
need to modify the `load-path' manually.  New-age version encourages to use
the standalone mode by cloning the repository into '~/.emacs.d/' folder.")

(if >>=standalone-mode
  (add-to-list 'load-path
    (concat (file-name-directory (or load-file-name default-directory))
      (file-name-as-directory "lisp"))))


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


(provide 'init)
;;; init.el ends here
