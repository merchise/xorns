;;; xorns-frames.el --- Merchise frame management for Emacs

;; Copyright (C) 2014-2016  Merchise Autrement [~º/~]

;; Author: Medardo Rodriguez <med@merchise.org>
;; Created: 2014-02-05
;; Keywords: merchise, extensions, tools
;; URL: http://dev.merchise.org/emacs/xorns-frames
;; Version: 20150516.1620

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
;; used easily in Emacs initialization processes.
;;
;; This module is not being used because is a better way to maximize
;; frames at its initialization (See `default-frame-alist' at
;; `xorns-startup.el').

;; Enjoy!


;;; Code:


;; (defun xorns-frame-maximize (&optional frame delay)
;;   "Maximize the specified FRAME.

;; If no frame is specified, current frame is assumed.  This functionality
;; can be executed with a DELAY specified in seconds; if none is given, `0.1'
;; is assumed."
;;   (interactive)
;;   (when (not (null window-system))
;;     (run-with-idle-timer (or delay 0.1) nil
;;       (lambda (frame)
;; 	  (modify-frame-parameters frame '((fullscreen . maximized))))
;;       (if frame frame (caar (cdr (current-frame-configuration))))))


(defun xorns-frame-fullscreen (&optional frame delay)
  "Put specified FRAME in full-screen if in a X display.

If no frame is specified, current frame is assumed.  This functionality
can be executed with a DELAY specified in seconds; if none is given, `0.1'
is assumed."
  (interactive)
  (when (not (null window-system))
    (run-with-idle-timer (or delay 0.1) nil
      (lambda (frame)
	(modify-frame-parameters frame '((fullscreen . fullboth))))
      frame)))


(provide 'xorns-frames)
;;; xorns-frames.el ends here
