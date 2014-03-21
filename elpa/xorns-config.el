;;; xorns-config --- Merchise variables

;; Copyright (C) 2014 Merchise Autrement

;; Author: Medardo Rodriguez <med@merchise.org>
;; URL: http://dev.merchise.org/emacs/xorns-config
;; Keywords: initialization, merchise, convenience
;; Version: 20140320.2216

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

;; Customization for the Merchise `xorns' library.  Define some variables
;; that change the behaviour of configurable features.

;; Enjoy!


;;; Code:


;;;###autoload
(defcustom xorns-prefer-helm-buffer-list nil
   "Indicates if prefer helm buffer list over ibuffer.

Use the `helm-buffer-list' if `helm-mode' is active and
this variable is non nil; elsewhere use standard `ibuffer'."
   :group 'xorns
   :type 'boolean)

(defvar helm-mode nil
  "Non-nil if Helm mode is enabled.
This is a fake variable that is overwritten by `helm-config'.  This
definition is just in case `helm' is not installed.")



(provide 'xorns-config)
;;; xorns-config.el ends here
