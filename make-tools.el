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


(setq package-archives
  `(("melpa" . "https://melpa.org/packages/")
    ("org" . "https://orgmode.org/elpa/")
    ("gnu" . ,(format "http%s://elpa.gnu.org/packages/"
                (if (gnutls-available-p) "s" "")))))


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


(defun >>=rsync-dir (dir)
  "Rsync DIR to installed package folder."
  (let ((pkg-desc (>>=pkg-desc)))
    (if pkg-desc
      (let ((source
	      (concat pkg-dir (file-name-as-directory "horns") dir))
	    (dest
	      (file-name-as-directory
		(concat elpa-dir (package-desc-full-name pkg-desc)))))
	(message ">>= rsync '%s' dir to installed package folder." dir)
	(shell-command (concat "rsync -auv " source " " dest)))
      ;; else
      (message ">>= '%s' dir not copied, '%s' is not installed." dir pkg))))


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


(defun >>=update-file (file)
  "Synchronize initialization FILE."
  (if (>>=pkg-desc)
    (let ((src (expand-file-name file pkg-dir))
	  (dst (locate-user-emacs-file file)))
      (when
	(and
	  (/= (shell-command (concat "diff " src " " dst)) 0)
	  (yes-or-no-p (format ">>= outdated '%s' file, synchronize?" file)))
	(copy-file src dst 'ok-if-already-exists)))
    ;; else
    (message ">>= package not installed, '%s' not synchronized." file)))


(defun local-install ()
  "Local install using ELPA directory as target."
  (>>=package-delete)
  (>>=package-install)
  (>>=update-file "early-init.el")
  (>>=update-file "init.el")
  (>>=rsync-dir "templates")
  (>>=rsync-dir "snippets")
  ;; (>>=copy-templates)
  )


(provide 'make-tools)
;;; make-tools.el ends here
