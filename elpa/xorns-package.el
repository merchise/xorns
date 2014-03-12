;;; xorns-package.el --- Merchise package repositories initialization

;; Copyright (C) 2014  Merchise Autrement
;; All rights reserved.

;; Author: Medardo Rodriguez <med@merchise.org>
;; Created: 2014-03-11
;; Keywords: merchise, extensions, tools
;; URL: http://dev.merchise.org/xorns/xorns-package

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

;; Enjoy!


;;; Code:

(require 'package)


(add-to-list
  'package-archives
  '("elpa" . "http://tromey.com/elpa/"))

(add-to-list
  'package-archives
  '("marmalade" . "http://marmalade-repo.org/packages/"))

(add-to-list
  'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/"))


(provide 'xorns-package)
;;; xorns-package.el ends here
