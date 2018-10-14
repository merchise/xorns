;;; install --- Install the xorns package

;; Copyright (C) Merchise Autrement [~º/~]

;; Author: Medardo Rodriguez <med@merchise.org>
;; URL: http://dev.merchise.org/emacs/install
;; Keywords: initialization, merchise, convenience
;; Version: 20140315.1218

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>
;; or type `C-h C-c' in Emacs.

;;; Commentary:

;; To run this installation program, from the shell:
;;     `emacs --load=install.el --batch --debug'
;;
;; See `xorns-init.el' for more info to configure `xorns' in your
;; Emacs initialization file.

;; Enjoy!


;;; Code:

(require 'package)
(eval-when-compile
  (require 'cl))

(package-initialize)


(defun delete-old-package ()
  "Remove old package if installed."
  (let ((pkg 'xorns))
    (when (package-installed-p pkg)
      (let* ((pkg-desc (cadr (assq pkg package-alist)))
              (version (package-desc-version pkg-desc)))
	(message "Deleting old package: `%s', version: %s" pkg version)
	(package-delete pkg-desc)))))


(defun install-process ()
  "Execute all installation process."
  (let ((src (expand-file-name "elpa"))
	(pkg (expand-file-name "xorns" temporary-file-directory)))
    (message "Create symbolic link from `%s' to `%s'." src pkg)
    (make-symbolic-link src pkg 'ok-if-exists)
    (package-install-file (file-name-as-directory pkg))
    (message "Deleting symbolic link: %s" pkg)
    (delete-file pkg)))


;;; Execute main body
(delete-old-package)
(install-process)


(provide 'install)
;;; install.el ends here
