;;; xorns-frames.el --- Merchise frame management for Emacs

;; Copyright (C) 2014  Merchise Autrement
;; All rights reserved.

;; Author: Medardo Rodriguez <med@merchise.org>
;; Created: 2014-02-05
;; Keywords: merchise, extensions, tools
;; URL: http://dev.merchise.org/emacs/xorns-frames

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

;; Include functions to change frame parameters and with the purpose of be
;; used easily in Emacs initialization processes.  See `xorns-init.el'.
;;
;; Enjoy!


;;; Code:


(defun xorns-frame-maximize ()
  "Maximize current FRAME."
  (when (not (null window-system))
    (run-with-idle-timer 0.1 nil
      (lambda ()
	(modify-frame-parameters nil '((fullscreen . maximized)))))))



(defun xorns-frame-fullscreen (&optional frame)
  "Put specified FRAME in full-screen if in a X display."
  (when (not (null window-system))
    (modify-frame-parameters frame
      '((fullscreen . fullboth)))))


(provide 'xorns-frames)
;;; xorns-frames.el ends here
