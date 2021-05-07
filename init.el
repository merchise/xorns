;;; init.el --- Xorns version of user’s initialization file

;;; Commentary:
;;
;; The file that starts all our own way to Emacs

;;; Code:

;; This file could be loaded in two different scenarios: running Emacs session
;; using `xorns' as an installed ELPA package, or in STANDALONE mode.

;; Without this comment emacs25 adds (package-initialize) here
;; (package-initialize)

(require 'package)


(when (version< emacs-version "27")
  (package-initialize))


(defconst >>=!init-mode/package
  (let ((pkg-info (assq 'xorns package-alist)))
    (if pkg-info
      (expand-file-name
        (package-desc-full-name (cadr pkg-info))
        package-user-dir)))
  "Non-nil if `xorns' is used as an ELPA installed package.")


(defconst >>=!init-mode/standalone
  (if (not >>=!init-mode/package)
    (expand-file-name "horns"
      ;; base-dir
      (if load-file-name
        (file-name-directory load-file-name)
        ;; else
        default-directory)))
  "Non-nil if `xorns' is used in standalone mode.")


(defconst >>=!library-directory
  (or >>=!init-mode/package >>=!init-mode/standalone)
  "Directory containing `xorns' library (valid in both modes).")


(let (file-name-handler-alist)    ; Improve startup time
  (if >>=!init-mode/standalone
    (add-to-list 'load-path >>=!init-mode/standalone))
  (require 'xorns))


(provide 'init)
;;; init.el ends here
