;;; local-install --- Install Xorns in local ELPA directory

;;; Code:

(progn
  (require 'env)
  (require 'package)
  (package-initialize)
  (let ((pkg (intern (or (getenv "PKG") "xorns")))
	(pkg-dir (file-name-as-directory (or (getenv "TOP") "."))))
    ;; delete old package
    (let ((pkg-desc (cadr (assq pkg package-alist))))
      (if pkg-desc
	(progn
	  (message "Deleting old package: %s." pkg)
	  (package-delete pkg-desc 'force 'nosave))
	;; else
	(message "Old package '%s' not installed." pkg)))
    ;; Install new version
    (let ((pkg-desc (cadr (assq pkg package-alist)))
	  (src (expand-file-name "horns" pkg-dir))
	  (tmp (expand-file-name (symbol-name pkg) temporary-file-directory)))
      (message "Creating symbolic link: %s -> %s" src tmp)
      (make-symbolic-link src tmp 'ok-if-exists)
      (message "Installing new version of package.")
      (package-install-file (file-name-as-directory tmp))
      (message "Deleting symbolic link: %s" tmp)
      (delete-file tmp))
    ;; Copy templates to installed package folder
    (let ((elpa-dir (file-name-as-directory package-user-dir))
	  (pkg-desc (cadr (assq pkg package-alist))))
      (if pkg-desc
	(let ((source (concat pkg-dir
			(file-name-as-directory "horns") "templates"))
	      (dest (file-name-as-directory
		       (concat elpa-dir (package-desc-full-name pkg-desc)))))
	  (message "Copying templates to installed package folder -- %s."
	    (concat "rsync -auv " source " " dest))
	  (shell-command (concat "rsync -auv " source " " dest)))
	;; else
	(message "Templates not copied because '%s' is not installed." pkg)))
    ))


(provide 'local-install)
;;; local-install.el ends here
