;;; xorns-package.el --- Merchise package repositories initialization

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Rodriguez <med@merchise.org>
;; Created: 2014-03-11
;; Keywords: merchise, extensions, tools
;; URL: http://dev.merchise.org/xorns/xorns-package
;; Version: 20150516.1620

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
;; or type `C-h C-c` in Emacs.

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;;; Commentary:

;; Add packages repositories preferred by Merchise:
;;  - elpa: Original Emacs Lisp Package Archive
;;  - marmalade: User-contributed repository
;;  - melpa: ?

;; This module is automatically used when::
;;
;;     (require 'xorns)

;; Enjoy!


;;; Code:

(require 'package)

(when (xorns-configure-p 'general)
  (add-to-list
    'package-archives
    '("elpa" . "http://tromey.com/elpa/"))
  (add-to-list
    'package-archives
    '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list
    'package-archives
    '("melpa" . "http://melpa.milkbox.net/packages/")))

;; TODO: Review next
;; In
;; https://emacs.stackexchange.com/questions/44788/error-use-package-cannot-load-magit
;; (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
;;              ("melpa" . "https://melpa.org/packages/")
;;              ("marmalade" . "https://marmalade-repo.org/packages/")
;;              ("melpa-stable" . "https://stable.melpa.org/packages/")
;;              ("elpy" . "https://jorgenschaefer.github.io/packages/")))


;;;###autoload
(defun xorns-dependency-install (feature)
  "Install a dependency FEATURE if not installed."
  (condition-case err
    (when (not (package-installed-p feature))
      (package-install feature))
    (error (message "error@dependency-install: %s" err))))


(provide 'xorns-package)
;;; xorns-package.el ends here
