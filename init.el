;;; init.el --- Xorns version of user’s initialization file

;;; Commentary:
;;
;; The file that starts all our own way to Emacs

;;; Code:

;; This file could be loaded in two different scenarios: running Emacs session
;; using `xorns' as an installed ELPA package, or in STANDALONE mode.

;; Without this comment emacs25 adds (package-initialize) here
;; (package-initialize)


(defconst >>=!xorns/emacs-min-version "27.1"
  "Minimal version of Emacs.")


(unless (version<= >>=!xorns/emacs-min-version emacs-version)
  (error
    "Emacs version '%s' is too old, Xorns requires version '%s' or above"
    emacs-version
    >>=!xorns/emacs-min-version))


(defconst >>=!xorns/standalone-dir
  (let ((lib-dir (expand-file-name "horns" user-emacs-directory)))
    (if (file-directory-p lib-dir) lib-dir))
  "Non-nil if `xorns' is used in standalone mode.")


(defconst >>=!xorns/lib-dir
  (or
    >>=!xorns/standalone-dir
    (progn
      ;; `xorns' is used as an ELPA package
      (eval-and-compile (require 'package))
      (when-let ((pkg-info (assq 'xorns package-alist)))
        (expand-file-name
          (package-desc-full-name (cadr pkg-info))
          package-user-dir))))
  "Directory containing `xorns' library.
Valid in standalone mode or as an ELPA pacxkage.")


(when (version<= emacs-version "29.0")    ; some fallback compatibility
  (add-to-list 'load-path (expand-file-name "compat" >>=!xorns/lib-dir)))


(defsubst >>-startup-file-name-handler-alist ()
  "Safe value of `file-name-handler-alist' trying to improve startup time."
  (let ((option "--without-compress-install"))
    (when (string-search option system-configuration-options)
      file-name-handler-alist)))


(let ((file-name-handler-alist (>>-startup-file-name-handler-alist)))
  (when >>=!xorns/standalone-dir
    (add-to-list 'load-path >>=!xorns/standalone-dir))
  (require 'xorns))


(provide 'init)
;;; init.el ends here
