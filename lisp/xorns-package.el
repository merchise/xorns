;;; xorns-package.el --- Merchise package repositories initialization

;; Copyright (c) Merchise Autrement [~º/~]

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

;; Add package archives preferred by us:
;;
;; - gnu (the system default): https://elpa.gnu.org/packages/
;; - melpa: https://melpa.org/packages/
;; - marmalade: https://marmalade-repo.org/packages/
;;
;; Extra package archives (see below) are configured while starting Emacs if
;; `xorns-extra-package-archives' variable is t, or at any time calling
;; `xorns-configure-extra-package-archives' function:
;;
;; - elpa: https://tromey.com/elpa/
;; - melpa-stable: https://stable.melpa.org/packages/
;; - elpy: https://jorgenschaefer.github.io/packages/

;; This module is automatically used when::
;;
;;     (require 'xorns)

;; Enjoy!


;;; Code:


(require 'package nil 'noerror)


(defcustom xorns-extra-package-archives nil
  "If t, extra package archives are configured while starting Emacs."
  :group 'xorns
  :type 'boolean)


(defun xorns-configure-extra-package-archives ()
  "Configure extra `package-archives'."
  (interactive)
  (add-to-list 'package-archives
    '("elpa" . "https://tromey.com/elpa/") t)
  (add-to-list 'package-archives
    '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives
    '("elpy" . "https://jorgenschaefer.github.io/packages/") t))


(defun xorns-dependency-install (feature)
  "Install a dependency FEATURE if not installed."
  (condition-case err
    (unless (package-installed-p feature)
      (package-install feature))
    (error (message "error@dependency-install: %s" err))))


(add-to-list 'package-archives    ;; "http://melpa.milkbox.net/packages/" ?
  '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
  '("marmalade" . "https://marmalade-repo.org/packages/") t)


(if xorns-extra-package-archives
  (xorns-configure-extra-package-archives))


(provide 'xorns-package)
;;; xorns-package.el ends here
