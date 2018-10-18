;;; xorns-org --- Manage miscellaneous organization stuffs.

;; Copyright (c) Merchise Autrement [~º/~]

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


(defgroup xorns-org nil
  "Xorns outline-based organizer."
  :tag "xorns"
  :group 'xorns)


(defcustom xorns-avoid-load-dict-strategies t
  "Avoid initial loading of `dict-strategies'.

Defining `dict-strategies' custom variable, `dict-get-strategies' function is
executed for every host in `dict-servers'.

With a connection without Internet access, it will take a very long time.
This patch avoid this."
  :group 'xorns-org
  :type 'boolean)


(if xorns-avoid-load-dict-strategies
  (let ((dict-servers '("localhost")))
    (require 'dict nil 'noerror))
  ;else
  (require 'dict nil 'noerror))


(require 'dictionary nil 'noerror)
(require 'ispell)
(require 'rfcview nil 'noerror)
(require 'wget nil 'noerror)
(require 'org nil 'noerror)
(require 'calendar nil 'noerror)
(require 'ob-core nil 'noerror)
(require 'ob-shell nil 'noerror)
(require 'ob-python nil 'noerror)
(require 'xorns-text nil 'noerror)
(require 'xorns-utils nil 'noerror)


(use-package deft
  :custom
  (deft-extensions '("txt" "text" "md" "markdown" "org" "rst"))
  (deft-use-filter-string-for-filename t)
  (deft-use-filename-as-title t)
  (deft-auto-save-interval 60.0)
  (deft-directory "~/.pim/notes/")
  (deft-strip-summary-regexp "\\([
	]\\|=\\{3,\\}\\|-\\{3,\\}\\|^#\\+[[:upper:]_]+:.*$\\)")

  :config
  (eval-when-compile
    (declare-function deft-filename-at-point "deft.el")
    (declare-function deft-open-file "deft.el"))

  (defun xorns-deft-open-file (&optional arg)
    "When the point is at a widget, open the file in a new buffer.
The argument ARG is passed to `deft-open-file' as SWITCH."
    (interactive "P")
    (let ((file (deft-filename-at-point)))
      (when file
	(deft-open-file file nil arg)
	(kill-buffer "*Deft*"))))

  :bind
  ("<f12>" . deft)
  (:map deft-mode-map ("M-RET" . xorns-deft-open-file)))


(defcustom xorns-org-confirm-babel-evaluate
  nil
  "To use as an extra check in modified `org-confirm-babel-evaluate'."
  :group 'xorns-org
  :type '(choice boolean function))


(defun xorns-org-confirm-babel-evaluate (lang body)
  "Check if special security flag is set for LANG in the BODY.

This function will check if the code BODY contains the flag 'trusted = true';
if not; check for the function defined in `xorns-org-confirm-babel-evaluate'
variable.

The flag could be defined in a commented area in your code, but must be
surrounded with blanks."
  (let* ( (regex
            "\\(^\\|\\W\\)trusted[[:blank:]]*=[[:blank:]]*true\\(\\W\\|$\\)")
          (res
            (not (string-match regex body)))
          )
    (if (and res xorns-org-confirm-babel-evaluate)
      (if (functionp xorns-org-confirm-babel-evaluate)
        (funcall xorns-org-confirm-babel-evaluate lang body)
                                        ; else
        xorns-org-confirm-babel-evaluate
        )
                                        ; else
      res))
  )


(when (featurep 'calendar)
  ;; Needed in order to use `diary-anniversary' without the year
  (xorns-set-value 'calendar-date-style 'american))


(when (featurep 'org)
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-cc" 'org-capture)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cb" 'org-iswitchb)
  (define-key org-mode-map "\C-cil" 'ispell-change-dictionary)
  (xorns-set-values
    '(org-todo-keywords
       (quote
         ((sequence "TODO(t)" "|" "DONE(d!)")
           (sequence "FIX(f)" "BUG(b)" "|" "SOLVED(s!)")
           (sequence "DEVELOP(v)" "REVIEW(r!)" "TEST(p!)" "|" "DELIVERY(e@/!)")
           (sequence "WTF(w)" "XXX(x)" "|" "WORTHY(y!)")
           (sequence "|" "CANCELED(c@)"))))
    '(org-todo-keyword-faces
       (quote
         (("TODO" . org-warning)
           ("BUG" . org-warning)
           ("WTF" . "black")
           ("CANCELED" . (:foreground "blue" :weight bold))))
       '(org-confirm-babel-evaluate xorns-org-confirm-babel-evaluate)))
                                        ; TODO: (xorns-set-value 'org-enforce-todo-dependencies t)
  )


(when (featurep 'ob-shell)
  (when (not (assoc 'shell org-babel-load-languages))
    (xorns-set-value 'org-babel-load-languages
      (cons '(shell . t) org-babel-load-languages)))
  (xorns-set-value 'org-babel-default-header-args:shell
    (cons '(:results . "output")
      (assq-delete-all :results org-babel-default-header-args:shell)))
  (let*
    ((shebang
       (assoc :shebang org-babel-default-header-args:shell))
      (prefix
        "#!/bin/bash\n\nexport SUDO_ASKPASS=")
      (full-prefix
        (concat
          prefix
          (xorns-file-path-join (getenv "HOME") ".local/bin/asksp")
          "\n")))
    (if shebang
      (when (not (string-prefix-p prefix (cdr shebang)))
        (setcdr shebang (concat full-prefix (cdr shebang))))
                                        ; else
      (setq shebang (cons :shebang full-prefix)))
    (xorns-set-value 'org-babel-default-header-args:shell
      (cons shebang
        (assq-delete-all :shebang org-babel-default-header-args:shell)))))


(when (featurep 'ob-python)
  (when (not (assoc 'python org-babel-load-languages))
    (xorns-set-value 'org-babel-load-languages
      (cons '(python . t) org-babel-load-languages)))
  (xorns-set-value 'org-babel-default-header-args:python
    (cons '(:results . "output")
      (assq-delete-all :results org-babel-default-header-args:python)))
  (let*
    ((preamble
       (assoc :preamble org-babel-default-header-args:python))
      (prefix
        "\nfrom __future__ import ")
      (full-prefix
        (concat
          prefix
          "division, print_function, absolute_import\n")))
    (if preamble
      (when (not (string-prefix-p prefix (cdr preamble)))
        (setcdr preamble (concat full-prefix (cdr preamble))))
                                        ; else
      (setq preamble (cons :preamble full-prefix)))
    (xorns-set-value 'org-babel-default-header-args:python
      (cons preamble
        (assq-delete-all :preamble org-babel-default-header-args:python)))))


(when (featurep 'dictionary)
  (global-set-key (kbd "C-c w") 'dictionary-search)
  (xorns-set-values
    '(dictionary-server "localhost")
    '(dictionary-use-single-buffer t)))


(when (featurep 'rfcview)
  (add-hook 'rfcview-mode-hook
    (lambda ()
      (condition-case err
        (progn
          (define-key rfcview-mode-map (kbd "l") 'pop-to-mark-command))
        (error (message "error@rfcview-mode-hook: %s" err)))))
  (let ((rfc-path "/usr/share/doc/RFC/links/"))
    (if (file-directory-p rfc-path)
      (xorns-set-values
        '(rfcview-rfc-location-pattern (concat rfc-path "rfc%s.txt.gz"))
        '(rfcview-std-location-pattern (concat rfc-path "rfc%s.txt.gz")))
      ;; else
      (warn "RFCs folder '%s' doesn't exists!" rfc-path))))


(when (featurep 'wget)
  (xorns-set-value 'wget-download-directory
    (xorns-preferred-directory "~/Downloads" "~/download" "~/softlib" "~")))


(add-hook 'org-mode-hook           ; run when entering org mode
  (lambda ()
    (condition-case err
      (progn
        (turn-on-auto-fill)
        (flyspell-mode nil)
        (xorns-set-value 'ispell-parser 'tex)
        (xorns-fci-mode-on))
      (error (message "error@org-mode-hook: %s" err)))))

(provide 'xorns-org)
;;; xorns-org.el ends here
