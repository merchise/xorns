;;; make-tools.el --- Tools intended to be used from a make-file

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; These tools must be called as:
;;
;;   $(EMACS) -Q --batch --debug --load=make-tools.el -f <TOOL-FUNCTION>
;;
;; The tool-function could be any function accepting zero parameters, see for
;; example `local-install'.

;;; Code:


(require 'env)

(require 'package)
(package-initialize)


(defconst pkg
  (intern (or (getenv "PKG") "xorns"))
  "Symbol with package identifier.")


(defconst pkg-dir
  (file-name-as-directory (or (getenv "TOP") "."))
  "Repository directory where package resides.")


(defconst elpa-dir
  (file-name-as-directory package-user-dir)
  "ELPA base directory.")


(defun >>=pkg-desc ()
  "Return package description."
  (cadr (assq pkg package-alist)))


(defun >>=package-delete ()
  "Delete installed ELPA package."
  (let ((pkg-desc (>>=pkg-desc)))
    (if pkg-desc
      (progn
	(message ">>= deleting old package: %s." pkg)
	(package-delete pkg-desc 'force 'nosave))
      ;; else
      (message ">>= old package '%s' not installed." pkg))))


(defun >>=package-install ()
  "Install new version of ELPA package."
  (let ((pkg-desc (>>=pkg-desc))
	(src (expand-file-name "horns" pkg-dir))
	(tmp (expand-file-name (symbol-name pkg) temporary-file-directory)))
    (message ">>= creating symbolic link: %s -> %s" src tmp)
    (make-symbolic-link src tmp 'ok-if-exists)
    (message ">>= installing new version of package.")
    (package-install-file (file-name-as-directory tmp))
    (message ">>= deleting symbolic link: %s" tmp)
    (delete-file tmp)))


(defun >>=copy-templates ()
  "Copy templates to installed package folder."
  (let ((pkg-desc (>>=pkg-desc)))
    (if pkg-desc
      (let ((source
	      (concat pkg-dir (file-name-as-directory "horns") "templates"))
	    (dest
	      (file-name-as-directory
		(concat elpa-dir (package-desc-full-name pkg-desc)))))
	(message ">>= copying templates to installed package folder.")
	(shell-command (concat "rsync -auv " source " " dest)))
      ;; else
      (message ">>= templates not copied, '%s' is not installed." pkg))))


(defun >>=update-init-el ()
  "Synchronize init.el file."
  (if (>>=pkg-desc)
    (let ((src (expand-file-name "init.el" pkg-dir))
	  (dst (locate-user-emacs-file "init.el")))
      (when
	(and (/= (shell-command (concat "diff " src " " dst)) 0)
	     (yes-or-no-p ">>= 'init.el' file is outdated, synchronize?"))
	(copy-file src dst 'ok-if-already-exists)))
    ;; else
    (message ">>= package not installed, 'init.el' not synchronized.")))


(defun local-install ()
  "Local install using ELPA directory as target."
  (>>=package-delete)
  (>>=package-install)
  (>>=update-init-el)
  (>>=copy-templates))


(provide 'make-tools)
;;; make-tools.el ends here
