;;; xorns-all --- Related to all `xorns' sub-modules

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Rodriguez <med@merchise.org>
;; URL: http://dev.merchise.org/emacs/xorns-all
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

;; Definitions that depends of all `xorns' sub-modules.

;; Enjoy!


;;; Code:

;; TODO: Deprecate or improve this

;;;###autoload
(defun xorns-all-dependencies-install ()
  "Install all dependencies of text modes."
  (xorns-text-dependencies-install)
  (xorns-git-dependencies-install))


(provide 'xorns-all)
;;; xorns-all.el ends here
