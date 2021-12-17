;;; xorns-pim.el --- Configuration for Personal Information Management (PIM)  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module is intended for Personal Information Management (PIM) stuffs
;; like: Notes taking, Dictionaries, Calendar, Planner, Organizer, etc.
;;
;; For more information see:
;; https://en.wikipedia.org/wiki/Personal_information_management

;; Enjoy!


;;; Code:

(require 'use-package)
(require 'xorns-tools)
(require 'xorns-packages)


(defvar >>=|pim/packages
  '(dictionary deft)
  "List of extra PIM packages to configure.")


(defmacro >>=-pim/configure? (*pkg*)
  "True if a PIM *PKG* must be configured."
  `(memq ',*pkg* >>=|pim/packages))


(defconst >>=!pim/prefered-directory (>>=dir-join >>=!home-dir ".pim")
  "List of miscellaneous packages to install.")



;;; Dictionary servers

(use-package dictionary
  :when (>>=-pim/configure? dictionary)
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
  ;; TODO: needed in order to use `diary-anniversary' without the year
  (calendar-date-style 'american))



;;; Organizer

(defconst >>=!org/prefered-directory
  (>>=dir-join >>=!pim/prefered-directory "org")
  "Prefered default location to look for Org files.")


(defvar >>=|pim/ob-featured-languages
  '(shell python)
  "List of languages to be featured by `org-babel-load-languages'.
Valid only if `org' is included in `>>=|pim/packages'.")


(use-package org
  :commands (org-store-link org-agenda)
  :defer t
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c b" . org-switchb)
  (:map org-mode-map
    ("C-c o" . org-toggle-link-display))
  :config
  ;; Configure `org-babel' languages from our variable
  (dolist (lang >>=|pim/ob-featured-languages)
    (when (require (intern (format "ob-%s" lang)) nil 'noerror)
      (setf (alist-get lang org-babel-load-languages) t)))
  ;; Set some custom variables
  (>>:custom
    (org-imenu-depth 8)
    (org-log-done 'time)
    (org-startup-folded nil)
    (org-reverse-note-order t)
    (org-startup-with-inline-images t)
    (org-src-fontify-natively t)
    (org-directory (>>=ensure-dir >>=!org/prefered-directory org-directory))
    )
  )


;;; Edit plain text notes

(use-package deft
  :when (>>=-pim/configure? deft)
  :ensure t
  :defer t
  :custom
  (deft-extensions '("txt" "text" "md" "markdown" "org" "rst"))
  (deft-use-filter-string-for-filename t)
  (deft-use-filename-as-title t)
  (deft-auto-save-interval 60.0)
  ;; TODO: Don't remove the TAB in the next definition
  (deft-strip-summary-regexp "\\([
	  ]\\|=\\{3,\\}\\|-\\{3,\\}\\|^#\\+[[:upper:]_]+:.*$\\)")
  :preface
  (progn
    (declare-function deft-open-file 'deft)
    (declare-function deft-filename-at-point 'deft)

    (defun >>=deft/open-file (&optional switch)
      "Open file killing deft buffer; SWITCH is used in `deft-open-file'."
      (interactive "P")
      (let ((file (deft-filename-at-point)))
        (when file
          (deft-open-file file nil switch)
          (kill-buffer "*Deft*")))))
  :bind
  (("<f12>" . deft)
    (:map deft-mode-map ("M-RET" . >>=deft/open-file)))
  :config
  (>>=dir-set deft-directory
    (>>=dir-join >>=!pim/prefered-directory "notes")))


(provide 'xorns-pim)
;;; xorns-pim.el ends here
