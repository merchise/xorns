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
  '(dictionary deft)
  "List of miscellaneous packages to install.")


(defmacro >>=-misc/configure? (pkg)
  "True if an miscellaneous PKG must be configured."
  `(memq ',pkg >>=|misc/packages))



;;; Dictionary servers

(use-package dictionary
  :when (>>=-misc/configure? dictionary)
  :ensure t
  :bind
  ("C-c w" . dictionary-search)
  :custom
  (dictionary-use-single-buffer t))



;;; Processes and commands

(use-package proced
  :unless (eq system-type 'darwin)    ;; unavailable on OS-X
  :bind
  ("C-x p" . proced))



;;; Date and time

(use-package calendar
  :bind
  ("C-c a c" . calendar)
  :custom
  ;; Needed in order to use `diary-anniversary' without the year
  (calendar-date-style 'american))



;;; Edit plain text notes

(use-package deft
  :when (>>=-misc/configure? deft)
  :ensure t
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
    (:map deft-mode-map ("M-RET" . >>=deft/open-file))))


(provide 'xorns-misc)
;;; xorns-misc.el ends here
