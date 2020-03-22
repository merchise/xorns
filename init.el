;;; init.el --- Xorns version of user’s initialization file

;;; Commentary:
;;
;; The file that starts all our own way to Emacs

;;; Code:

;; Without this comment emacs25 adds (package-initialize) here
;; (package-initialize)

(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)


(defconst >>=base-dir
  (concat
    (if load-file-name
      (file-name-directory load-file-name)
      ;; else
      default-directory))
  "Base `xorns' directory.")


(defconst >>=package-user-dir
  (let ((pkg-info (assq 'xorns package-alist)))
    (if pkg-info
      (expand-file-name
	(package-desc-full-name (cadr pkg-info))
	package-user-dir)))
  "Directory containing installed `xorns' package.")


(defconst >>=standalone-startup
  (if (not >>=package-user-dir)
    (expand-file-name "horns" >>=base-dir))
  "If nil, Emacs started up with `xorns' as an installed package.")


(defconst >>=library-directory
  (or >>=package-user-dir >>=standalone-startup)
  "Directory containing `xorns' library (valid in both modes).")


(load (expand-file-name "xorns.lock" >>=base-dir) nil (not init-file-debug))


(if >>=standalone-startup
  (add-to-list 'load-path >>=standalone-startup))


(require 'xorns)


(provide 'init)
;;; init.el ends here
