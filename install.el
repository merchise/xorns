;;; install --- Install the xorns package

;; Copyright (C) 2014-2016 Merchise Autrement [~º/~]

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
;;     `$ emacs --load=install.el --batch --debug'
;;
;; See `xorns-init.el' for more info to configure `xorns' in your
;; Emacs initialization file.

;; Enjoy!


;;; Code:

(require 'package)
(eval-when-compile
  (require 'cl))

;; Activate ELPA packages.
(load "~/.emacs.d/elpa/nxml-mode-20041004/rng-auto.el")

(package-initialize)

(defconst pkg 'xorns
  "Symbol for package identifier.")


(defun file-read-text (file-name)
  "Read text from FILE-NAME."
  (decode-coding-string
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (setq buffer-file-coding-system 'binary)
      (insert-file-contents-literally file-name)
      (buffer-substring-no-properties (point-min) (point-max)))
    'utf-8))


(defconst pkg-new-version
  (let* ((pkg-info (file-read-text (format "./elpa/%s-pkg.el" pkg)))
	 (version (nth 2 (read pkg-info))))
    version)
  "Configured new version for the package.")


(defun delete-old-package ()
  "Remove old package if already installed."
  (when (package-installed-p pkg)
    (let* ((pkg-desc
             (cadr (assq pkg package-alist)))
            (version
              (package-desc-version pkg-desc)))
      (message "Deleting old package: `%s', version: %s" pkg version)
      (package-delete pkg-desc))))


(defun main ()
  "Execute all installation process."
  (delete-old-package)
  (let* ((name-with-version (format  "%s-%s" pkg pkg-new-version))
	 (tar-file-name (concat name-with-version ".tar"))
	 (cur default-directory)
	 (full (concat cur "elpa"))
	 (tmp temporary-file-directory)
	  )
    (message "Create symbolic link from `elpa' to `%s'." name-with-version)
    (make-symbolic-link full (concat tmp name-with-version) 'ok-if-exists)
    (message "Create tar file `%s'." tar-file-name)
    (cd tmp)
    (shell-command
      (format "tar -cf %s %s/*" tar-file-name name-with-version))
    (message "Installing package: %s" pkg)
    (package-install-file tar-file-name)
    (message "Deleting tar file: %s" tar-file-name)
    (delete-file tar-file-name)
    (message "Deleting symbolic link: %s" name-with-version)
    (delete-file name-with-version)
    (cd cur)
    )
  )

;;; Execute main body

(main)


(provide 'install)
;;; install.el ends here
