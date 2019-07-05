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
;; Extra package archives could be configured:
;; - elpa: https://tromey.com/elpa/
;; - melpa-stable: https://stable.melpa.org/packages/
;; - elpy: https://jorgenschaefer.github.io/packages/

;; This module is automatically used when::
;;
;;     (require 'xorns)

;; Enjoy!


;;; Code:


(require 'package nil 'noerror)

(add-to-list 'package-archives    ;; "http://melpa.milkbox.net/packages/" ?
  '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
  '("marmalade" . "https://marmalade-repo.org/packages/") t)


(provide 'xorns-package)
;;; xorns-package.el ends here
