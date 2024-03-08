;;; init.el --- Xorns version of user’s initialization file

;;; Commentary:
;;
;; The file that starts all our own way to Emacs

;;; Code:

;; This file could be loaded in two different scenarios: running Emacs session
;; using `xorns' as an installed ELPA package, or in STANDALONE mode.

;; Without this comment emacs25 adds (package-initialize) here
;; (package-initialize)


(defconst >>=!xorns/emacs-min-version "29.1"
  "Minimal version of Emacs.")


(unless (version<= >>=!xorns/emacs-min-version emacs-version)
  (error
    "Xorns requires Emacs >='%s', not '%s'"
    >>=!xorns/emacs-min-version
    emacs-version))


(defconst >>=!xorns/lib-dir
  (let ((lib-dir (expand-file-name "horns" user-emacs-directory)))
    (when (file-directory-p lib-dir) lib-dir))
  "Directory containing `xorns' library.")


(defsubst >>-startup-file-name-handler-alist ()
  "Safe value of `file-name-handler-alist' trying to improve startup time."
  (let ((option "--without-compress-install"))
    (when (string-search option system-configuration-options)
      file-name-handler-alist)))


(let ((file-name-handler-alist (>>-startup-file-name-handler-alist)))
  (add-to-list 'load-path >>=!xorns/lib-dir)
  (require 'xorns))


(provide 'init)
;;; init.el ends here
