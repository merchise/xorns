;;; xorns-misc.el --- Xorns Configuration for miscellanies

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Manage miscellaneous stuffs like:
;;
;; - Notes taking
;; - Dictionaries
;; - Calendar
;; - RFCs
;; - Planner
;; - etc


;;; Code:

(require 'use-package)
(require 'xorns-tools)
(require 'xorns-utils)
(require 'xorns-packages)


(defvar >>=|misc/packages
  '(dictionary deft
     )
  "List of miscellaneous packages to install.")


(defmacro >>=-misc/configure? (pkg)
  "True if an miscellaneous PKG must be configured."
  `(memq ',pkg >>=|misc/packages))


(when (>>=-misc/configure? dictionary)
  (>>=require dictionary))


(require 'calendar nil 'noerror)


(when (>>=-misc/configure? deft)
  (>>=ensure-packages deft)
  (use-package deft
    :defer t
    :commands (deft-filename-at-point deft-open-file)
    :custom
    (deft-extensions '("txt" "text" "md" "markdown" "org" "rst"))
    (deft-use-filter-string-for-filename t)
    (deft-use-filename-as-title t)
    (deft-auto-save-interval 60.0)
    (deft-directory "~/.pim/notes/")
    (deft-strip-summary-regexp "\\([
	  ]\\|=\\{3,\\}\\|-\\{3,\\}\\|^#\\+[[:upper:]_]+:.*$\\)")

    :init
    (defun >>=deft/open-file (&optional arg)
      "When the point is at a widget, open the file in a new buffer.
  The argument ARG is passed to `deft-open-file' as SWITCH."
      (interactive "P")
      (let ((file (deft-filename-at-point)))
	(when file
	  (deft-open-file file nil arg)
	  (kill-buffer "*Deft*"))))

    :bind
    (("<f12>" . deft)
     (:map deft-mode-map ("M-RET" . >>=deft/open-file)))))


(when (featurep 'calendar)
  ;; Needed in order to use `diary-anniversary' without the year
  (setq-default calendar-date-style 'american))


(when (featurep 'dictionary)
  (global-set-key (kbd "C-c w") 'dictionary-search)
  (setq-default
    dictionary-use-single-buffer t))


(provide 'xorns-misc)
;;; xorns-misc.el ends here
