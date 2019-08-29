;;; local-install --- Install Xorns in local ELPA directory

;;; Code:
(progn
  (require 'env)
  (require 'package)
  (package-initialize)
  (let* ((pkg (intern (or (getenv "PKG") "xorns")))
	 (pkg-dir (or (getenv "TOP") "./"))
	 (pkg-desc (cadr (assq pkg package-alist)))
	 (src (expand-file-name "horns" pkg-dir))
	 (tmp (expand-file-name (symbol-name pkg) temporary-file-directory)))
    (if pkg-desc
      (progn
	(message "Deleting old package: %s." pkg)
	(package-delete pkg-desc 'force 'nosave))
      ; else
      (message "Old package '%s' not installed." pkg))
    (message "Creating symbolic link: %s -> %s" src tmp)
    (make-symbolic-link src tmp 'ok-if-exists)
    (message "Installing new version of package.")
    (package-install-file (file-name-as-directory tmp))
    (message "Deleting symbolic link: %s" tmp)
    (delete-file tmp)))
(provide 'local-install)
;;; local-install.el ends here
