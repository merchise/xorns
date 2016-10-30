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

(require 'dict nil 'noerror)
(require 'dictionary nil 'noerror)
(require 'ispell)
(require 'rfcview nil 'noerror)
(require 'wget nil 'noerror)
(require 'deft nil 'noerror)
(require 'org nil 'noerror)
(require 'calendar nil 'noerror)
(require 'xorns-text nil 'noerror)


;;;###autoload
(defun xorns-deft-open-file (&optional arg)
  "When the point is at a widget, open the file in a new buffer.
The argument ARG is passed to `deft-open-file' as SWITCH."
  (interactive "P")
  (let ((file (deft-filename-at-point)))
    (when file
      (deft-open-file file nil arg)
      (kill-buffer "*Deft*"))))


(when (featurep 'org)
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-cc" 'org-capture)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cb" 'org-iswitchb)
  (define-key org-mode-map "\C-cil" 'ispell-change-dictionary)
  (setq
    org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d!)")
	(sequence "FIX(f)" "BUG(b)" "|" "SOLVED(s!)")
	(sequence "DEVELOP(v)" "REVIEW(r!)" "TEST(p!)" "|" "DELIVERY(e@/!)")
	(sequence "WTF(w)" "XXX(x)" "|" "WORTHY(y!)")
	(sequence "|" "CANCELED(c@)"))
    org-todo-keyword-faces
      '(("TODO" . org-warning)
        ("BUG" . org-warning)
        ("WTF" . "black")
        ("CANCELED" . (:foreground "blue" :weight bold))))
  ; TODO: (setq org-enforce-todo-dependencies t)
  )

(when (featurep 'calendar)
  ;; Next is needed in order to use `diary-anniversary' without the year
  (setq calendar-date-style 'american)
  )


(when (featurep 'dict)
  (set-variable 'dict-servers '("localhost"))
  )


(when (featurep 'dictionary)
  (global-set-key (kbd "C-c w") 'dictionary-search)
  (setq
    dictionary-server "localhost"
    dictionary-use-single-buffer t)
  )


(when (featurep 'deft)
  ; TODO: Remove all deft `.emacs.d' custom files
  (setq deft-auto-save-interval 60.0)
  (add-to-list 'deft-extensions "rst" 'append)
  (global-set-key (kbd "<f12>") 'deft)
  (define-key deft-mode-map (kbd "M-RET") 'xorns-deft-open-file))



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


(add-hook 'org-mode-hook           ; run when entering org mode
  (lambda ()
    (condition-case err
      (progn
        (turn-on-auto-fill)
	(flyspell-mode nil)
        (setq ispell-parser 'tex)
        (xorns-fci-mode-on))
      (error (message "error@org-mode-hook: %s" err)))))

(provide 'xorns-org)
;;; xorns-org.el ends here
