;;; xorns-org --- Manage miscellaneous organization stuffs.

;; Copyright (C) 2014-2016 Merchise

;; Author: Medardo Rodriguez <med@merchise.org>
;; URL: http://dev.merchise.org/emacs/xorns-project
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

;; Manage miscellaneous organization stuffs like:
;;
;; - Notes taking
;; - Dictionaries
;; - Calendar
;; - RFCs
;; - Planner
;; - WGet
;; - etc

;; This module is automatically used when::
;;
;;     (require 'xorns)

;; Enjoy!


;;; Code:



(require 'org nil 'noerror)
(require 'dict nil 'noerror)
(require 'dictionary nil 'noerror)
(require 'deft nil 'noerror)
(require 'rfcview nil 'noerror)
(require 'wget nil 'noerror)


(when (featurep 'calendar)
  (setq calendar-date-style 'iso)
  )


(when (featurep 'dict)
  (set-variable 'dict-servers '("localhost"))
  )

(when (featurep 'dictionary)
  (global-set-key (kbd "C-c w") 'dictionary-search)
  (setq dictionary-server "localhost")
  (setq dictionary-use-single-buffer t)
  )


(when (featurep 'deft)
  ; TODO: Remove all deft `.emacs.d' custom files
  (setq deft-auto-save-interval 60.0)
  (add-to-list 'deft-extensions "rst" 'append)
  (global-set-key (kbd "<f12>") 'deft))



(when (featurep 'rfcview)
  (add-hook 'rfcview-mode-hook
    (lambda ()
      (condition-case err
        (progn
          (define-key rfcview-mode-map (kbd "l") 'pop-to-mark-command))
        (error (message "error@rfcview-mode-hook: %s" err)))))
  (let ((rfc-path "/usr/share/doc/RFC/links/"))
    (if (file-directory-p rfc-path)
      (setq
        rfcview-rfc-location-pattern (concat rfc-path "rfc%s.txt.gz")
        rfcview-std-location-pattern (concat rfc-path "rfc%s.txt.gz"))
      ;; else
      (warn "RFCs folder '%s' doesn't exists!" rfc-path))))


(when (featurep 'wget)
  (setq wget-download-directory
    (xorns-preferred-directory "~/Downloads" "~/download" "~/softlib" "~")))


(provide 'xorns-org)
;;; xorns-org.el ends here
