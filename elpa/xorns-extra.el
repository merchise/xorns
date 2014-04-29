;;; xorns-extra --- Some extra features

;; Copyright (C) 2014 Merchise Autrement

;; Author: Medardo Rodriguez <med@merchise.org>
;; URL: http://dev.merchise.org/emacs/xorns-extra
;; Keywords: initialization, merchise, convenience
;; Version: 20140324.1132

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

;; Define features that could be not desired from most members, but
;; do needed for particular ones.  These features are not include when
;; require `xorns', so this one feature (`xorns-extra') must be
;; explicitly required in one of `~/.emacs' or `~/.emacs.d/init.el'
;; files.
;;
;; To use `xorns', and automatically include all its basic features,
;; just configure one of the standard initialization files (`~/.emacs'
;; or `~/.emacs.d/init.el') with the following body::
;;
;;     (package-initialize)
;;     (require 'xorns)
;;
;; These extra features that are not included in basic `xorns'; to
;; configure in the selected initialization file::
;;
;;     (require 'xorns-extra)

;; Enjoy!


;;; Code:

(require 'xorns-mail)
(require 'xorns-gud)


(provide 'xorns-extra)
;;; xorns-extra.el ends here
