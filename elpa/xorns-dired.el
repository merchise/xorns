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

;; Configure all `dired' dependencies in the way of Merchise.

;; Improve `dired-single' by remembering parent position for recovering it
;; when navigating up.

;; Enjoy!


;;; Code:

(require 'dired)
(require 'dired-single)

;;; Modules customization

;; Use "dired-single"
;; TODO: Why ``(require 'dired-single)`` doesn't function?

;; (load-file "~/.emacs.d/el-get/dired-single/dired-single.el")

;; Use (set-register "/home/med/work/merchise" (dired-save-positions)) and
;; ````

(defun xorns-setup-dired-single ()
  "Customize `dired-single' key-bindings.

After this function is called;  Enter, Click and ^ reuse the buffer
instead of creating a new one."
  (define-key dired-mode-map [return] 'dired-single-buffer)
  (define-key dired-mode-map [M-S-down] 'dired-single-buffer)
  (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
  (define-key dired-mode-map [M-S-up]
    #'(lambda nil (interactive) (dired-single-buffer "..")))
  (define-key dired-mode-map "^"
    #'(lambda nil (interactive) (dired-single-buffer ".."))))


(provide 'xorns-dired)
;;; xorns-dired.el ends here
