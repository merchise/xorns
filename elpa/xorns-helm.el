;;; xorns-helm --- Use `helm' support

;; Copyright (C) 2014 Merchise Autrement

;; Author: Medardo Rodriguez <med@merchise.org>
;; URL: http://dev.merchise.org/emacs/xorns-helm
;; Keywords: initialization, merchise, convenience
;; Version: 20140324.1145

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

;; Add some `helm' features to `xorns' if installed.  This module is
;; not include in standard `xorns' requires, so use this in your
;; `xorns-extra' initialization file.

;; This module is not automatically used when require plain `xorns',
;; to use it::
;;
;;     (require 'xorns-extra)

;; Enjoy!


;;; Code:

(require 'helm nil 'noerror)
(require 'helm-config nil 'noerror)
(require 'helm-buffers nil 'noerror)


(defcustom xorns-prefer-helm-buffer-list nil
  "Indicates if prefer helm buffer list over `ibuffer'.

Use the `helm-buffer-list' if `helm-mode' is active and
this variable is non nil; elsewhere use standard `ibuffer'."
  :group 'xorns
  :type 'boolean)


(defun xorns-helm-buffers-list ()
  "Change configured `xorns' to list buffers using `helm' if preferred."
  (interactive)
  (cond
    ((and helm-mode xorns-prefer-helm-buffer-list)
      (helm-buffers-list))
    ((functionp 'ibuffer)
      (ibuffer))))


(global-set-key (kbd "C-x C-b") 'xorns-helm-buffers-list)


(provide 'xorns-helm)
;;; xorns-helm.el ends here
