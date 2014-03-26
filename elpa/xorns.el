;;; xorns --- Execute all Merchise preferred initialization

;; Copyright (C) 2014 Merchise Autrement

;; Author: Medardo Rodriguez <med@merchise.org>
;; URL: http://dev.merchise.org/emacs/xorns
;; Keywords: initialization, merchise, convenience
;; Version: 20140316.1200

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

;; To use `xorns', and automatically include all its basic features,
;; just configure one of the standard initialization files (`~/.emacs'
;; or `~/.emacs.d/init.el') with the following body::
;;
;;     (package-initialize)
;;     (require 'xorns)
;;
;; There are some extra features that are not included in basic
;; `xorns'; if required, configure in the selected initialization
;; file::
;;
;;     (require 'xorns-extra)

;; Enjoy!


;;; Code:

;; Basic initialization
(require 'xorns-startup)
(require 'xorns-buffers)
(require 'xorns-dired)
(require 'xorns-simple)
(require 'xorns-term)
(require 'xorns-prog)        ;; This requires `xorns-text'
(require 'xorns-project)
(require 'xorns-git)

;; Configure preferred package repositories
(require 'xorns-package)



;;; Finally, load all customized variables
;; TODO: Extend for MacOS
(let* ((user-custom-file
	 (locate-user-emacs-file
	   (concat "custom-" user-real-login-name ".el")))
       (user-custom-file
	 (if (file-exists-p user-custom-file)
	   user-custom-file
	   ; else
	   (locate-user-emacs-file "custom.el"))))
  (message "custom-file: %s" user-custom-file)
  (when (file-exists-p user-custom-file)
    (setq custom-file user-custom-file)
    (load custom-file 'noerror)))


(provide 'xorns)
;;; xorns.el ends here
