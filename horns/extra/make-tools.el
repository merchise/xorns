;;; make-tools.el --- Install Xorns in local ELPA directory

;;; Code:

(let ((pkg (intern (or (getenv "PKG") "xorns")))
      (pkg-dir (file-name-as-directory (or (getenv "TOP") default-directory))))
  (require 'package)
  (package-initialize)
  ;; (locate-user-emacs-file NEW-NAME &optional OLD-NAME)
  (let ((init-file (expand-file-name "init.el" pkg-dir)))
      (with-temp-file init-file
	(insert-file-contents init-file)
	(re-search-forward "(setq package-archives")
	(re-search-backward "(")
	(mark-sexp)
	(eval-region (point) (mark))))
  (princ package-archives)
  )

(provide 'make-tools)
;;; make-tools.el ends here
