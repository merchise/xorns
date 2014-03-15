;;; xorns-widgets --- Functions for creating and using widgets

;; Copyright (C) 2014 Merchise Autrement

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

;; Author: Medardo Rodriguez <med@merchise.org>
;; URL: http://dev.merchise.org/emacs/xorns-widgets
;; Keywords: initialization, merchise, convenience
;; Version: 0.1.0

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;;; Commentary:

;; Merchise extensions to creating and using widgets.

;; Enjoy!


;;; Code:

(declare-function widget-value "wid-edit.el")


(defun --required (widget)
   "Validate the WIDGET's value as required."
   (let ((value (widget-value widget)))
      (when (or (null value) (equal value ""))
	 (widget-put widget :error "This field is required.")
	 widget)))


(provide 'xorns-widgets)
;;; xorns-widgets.el ends here
