;;; xorns-apt --- Extend `apt-utils.el' package

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Rodriguez <med@merchise.org>
;; URL: http://dev.merchise.org/emacs/xorns-apt
;; Keywords: initialization, merchise, convenience
;; Version: 20150516.1620

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

;; Configure all GIT preferences using `magit'.
;;
;; This module is automatically used when::
;;
;;     (require 'xorns)

;; Enjoy!


;;; Code:

(require 'xorns-utils nil 'noerror)
(require 'xorns-package nil 'noerror)
(require 'apt-utils nil 'noerror)


(defun xorns-apt-current-symbol ()
  "Return current APT package name."
  (save-excursion
    (let* ((cur (point))
	   (regex "[^a-z0-9.:-]")
	   (start
	     (1+ (or (re-search-backward regex nil 'noerror)
	             (1- (point-min)))))
	   (end
	     (1- (or (re-search-forward regex nil 'noerror 2)
		     (1+ (point-min)))))
	   (symbol (buffer-substring-no-properties start end)))
      (if (string= symbol "") nil (downcase symbol)))))


(defun xorns-apt-choose-package ()
  "Choose a Debian package name."
  (let ((package
	  (or (and (eq major-mode 'apt-utils-mode)
	           (cadr (member 'apt-package (text-properties-at (point)))))
	      (xorns-apt-current-symbol)))
        (PC-word-delimiters "-"))
    (when (not (stringp package))
      (setq package nil))
    (completing-read "Choose package: "
                     'apt-utils-choose-package-completion
                     nil t package)))

;;;###autoload
(defun xorns-apt-show (package)
  "Show information for a Debian PACKAGE.
A selection of known packages is presented.  See `apt-utils-mode'
for more detailed help."
  (interactive "P")
  (let ((package (xorns-apt-choose-package)))
    (when (> (length package) 0)
      (apt-utils-show-package-1 package t nil))))

(when (xorns-configure-p 'basic)
  (if (featurep 'apt-utils)
    (global-set-key "\C-xp" 'xorns-apt-show)
    ;else
    (xorns-missing-feature 'apt-utils)))

;;;###autoload
(defun xorns-apt-dependencies-install ()
  "Install all dependencies for `apt' modes."
  (xorns-dependency-install 'apt-utils))


(provide 'xorns-apt)
;;; xorns-apt.el ends here
