;;; xorns-dired --- Merchise extensions for `dired'

;; Copyright (C) 2014 Merchise Autrement

;; Author: Medardo Rodriguez <med@merchise.org>
;; URL: http://dev.merchise.org/emacs/xorns-dired
;; Keywords: initialization, merchise, convenience
;; Version: 20140319.1548

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

;; Configure and extend all `dired' dependencies in the way of Merchise.
;;
;; Improve `dired-single' by remembering parent position for recovering it
;; when navigating up.
;;
;; This module is automatically used when::
;;
;;     (require 'xorns)

;; Enjoy!


;;; Code:

(require 'dired)
(require 'dired-single nil 'noerror)


(defun xorns-setup-dired-single ()
  "Customize `dired-single' key-bindings.

After this function is called;  Enter, Click and ^ reuse the buffer
instead of creating a new one.

If `dired-single' is not installed, does nothing."
  (when (featurep 'dired-single)
    (declare-function dired-single-buffer 'dired-single)
    (define-key dired-mode-map [return] 'dired-single-buffer)
    (define-key dired-mode-map [M-S-down] 'dired-single-buffer)
    (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
    (define-key dired-mode-map [M-S-up]
      #'(lambda () (interactive) (dired-single-buffer "..")))
    (define-key dired-mode-map "^"
      #'(lambda () (interactive) (dired-single-buffer "..")))))


(when (xorns-configure-p 'basic)
  (if (boundp 'dired-mode-map)
    (xorns-setup-dired-single)
    ; else
    (add-hook 'dired-load-hook 'xorns-setup-dired-single)))


;; ;; TODO: To preserve positions, use::
;; (set-register
;;   (intern
;;     (xorns-default-directory))
;;   (dired-save-positions))


(provide 'xorns-dired)
;;; xorns-dired.el ends here
