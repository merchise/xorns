;;; xorns-packages.el --- Configure package system for Xorns

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module is installed just by calling `(require 'xorns-packages)' in the
;; initialization process, which is done automatically.

;;; Code:




(defun >>=setup-package-system ()
  "Initialize `package.el' and bootstrap `use-package' if needed."
  (require 'package)
  (setq package-enable-at-startup nil)
  (setq package-archives
    '(("gnu"   . "http://elpa.gnu.org/packages/")
      ("melpa" . "https://melpa.org/packages/")
      ("org"   . "https://orgmode.org/elpa/")))
  (package-initialize)
  ; Bootstrap 'use-package'
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))


(defmacro >>=-check-package-system ()
  "Check if package-system is properly initialized."
  (if (not (and (boundp 'package--initialized) package--initialized))
    (error ">>= package-system is not initialized")))


(defun >>=package-system/install-dependencies ()
  "Install required dependencies for `use-package' use."
  (>>=-check-package-system)
  (use-package diminish
    :ensure t)
  (use-package delight
    :ensure t)
  (use-package use-package-chords
    :ensure t
    :config
    (key-chord-mode 1))
  (use-package system-packages
    :ensure t
    :custom
    (system-packages-noconfirm t))
  (use-package use-package-ensure-system-package
    :ensure t)
  (use-package quelpa
    :ensure t
    :custom
    (quelpa-update-melpa-p nil "Don't update the MELPA git repo."))
  (use-package quelpa-use-package
    :ensure t))


(defun >>=ensure-package (pkg)
  "Ensure the package PKG is installed."
  (unless (package-installed-p pkg)
    (condition-case-unless-debug err
      (progn
	(when (assoc pkg (bound-and-true-p package-pinned-packages))
	  (package-read-all-archive-contents))
	(if (assoc pkg package-archive-contents)
	  (package-install pkg)
	  ; else
	  (package-refresh-contents)
	  (when (assoc pkg (bound-and-true-p package-pinned-packages))
	    (package-read-all-archive-contents))
	  (package-install pkg))
	pkg)
      (error
	(display-warning 'xorns-packages
	  (format "Failed to install %s: %s" name (error-message-string err))
	  :error)))))


(defun >>=package-install (pkg &optional archive)
  "Install the package PKG unless it is already installed.
If ARCHIVE is specified, force it instead of using all configured
`package-archives'.  In case of failure trying to install the package, this
function will refresh package contents and retry."
  (unless (package-installed-p pkg)
    (let ((package-archives
	    (if archive
	      (list (assoc-string archive package-archives))
	      ; else
	      package-archives)))
      (condition-case nil
	(package-install pkg)
        (error
	  (progn
	    (package-refresh-contents)
	    (condition-case err
	      (package-install pkg)
	      (error
		(message ">>= error installing %s: %s."
		  pkg (error-message-string err))))))))))


(defmacro >>=require (pkg)
  "Install the package PKG unless it is already installed and require it."
  `(progn
     (>>=package-install ',pkg)
     (require ',pkg)))


(provide 'xorns-packages)
;;; xorns-packages.el ends here
