;;; setup.el --- Merchise extensions for Emacs (setup program)

;; Copyright (c) 2014 Merchise Autrement

;; Author: Medardo Rodriguez <med@merchise.org>
;; Version: 0.1
;; Keywords: merchise, extensions, setup
;; URL: http://dev.merchise.org/xorns/setup

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

;; This program install all "Xorns" components and make them ready to
;; Emacs: a main ELPA package, a set of shell scripts that make better
;; experience from terminals and an improved collection of YAS snippets.

;; Customization for variable are registered in some special functions
;; that can be easily called from "~/.emacs" or "~/.emacs.d/init.el"
;; initialization files.

;; This program is executed inside `setup.py' by:
;;   $ emacs --load=setup.el --batch

;;
;; TODO: emacs --eval='(package-install-file "~/tmp/xorns-0.2.tar")' --batch


;;; Code:


;;; setup.el ends here
