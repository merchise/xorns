;;; xorns-buffers.el --- Merchise extensions for Emacs (buffers)

;; Copyright (C) 2014  Merchise Autrement
;; All rights reserved.

;; Author: Medardo Rodriguez <med@merchise.org>
;; Created: 2014-03-10
;; Keywords: merchise, extensions, tools
;; URL: http://dev.merchise.org/xorns/xorns-buffers

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; "xorns-buffers" provides routines and variables to manipulate Emacs
;; buffers.
;;
;; Features:
;;
;; - Improved ...
;;
;; Enjoy!


;;; Code:

;;;###autoload
(defun xorns-force-switch-to-scratch ()
  "Switch to `*scratch*` buffer, creating a new one if needed."
  (interactive)
  (let ((buf (get-buffer-create "*scratch*")))
    (set-buffer-major-mode buf)
    (switch-to-buffer buf)
    (delete-other-windows)))


(provide 'xorns-buffers)
;;; xorns-buffers.el ends here
