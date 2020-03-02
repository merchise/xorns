;;; xorns-base.el --- Xorns Configuration for Base System

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is not part of GNU Emacs.

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


;;; Code:

(require 'use-package)
(require 'xorns-tools)
(require 'xorns-packages)


(defvar >>=|org/packages
  '(dictionary wget
     )
  "List of miscellaneous organization packages to install.")


(defmacro >>=-org/configure? (pkg)
  "True if an organization PKG must be configured."
  `(memq ',pkg >>=|org/packages))


(defgroup xorns-org nil
  "Xorns outline-based organizer."
  :tag "xorns"
  :group 'xorns)


(when (>>=-org/configure? dictionary)
  (>>=require dictionary))


(when (>>=-org/configure? wget)
  (>>=require wget))


(require 'org nil 'noerror)
(require 'calendar nil 'noerror)
(require 'ob-core nil 'noerror)
(require 'ob-shell nil 'noerror)
(require 'ob-python nil 'noerror)
(require 'xorns-text nil 'noerror)




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
  (defun xorns-deft-open-file (&optional arg)
    "When the point is at a widget, open the file in a new buffer.
The argument ARG is passed to `deft-open-file' as SWITCH."
    (interactive "P")
    (let ((file (deft-filename-at-point)))
      (when file
	(deft-open-file file nil arg)
	(kill-buffer "*Deft*"))))

  :bind (("<f12>" . deft)
	 (:map deft-mode-map ("M-RET" . xorns-deft-open-file))))


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
  (setq-default calendar-date-style 'american))


(when (featurep 'org)
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-cc" 'org-capture)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cb" 'org-iswitchb)
  (setq-default
    org-todo-keywords
       (quote
         ((sequence "TODO(t)" "|" "DONE(d!)")
           (sequence "FIX(f)" "BUG(b)" "|" "SOLVED(s!)")
           (sequence "DEVELOP(v)" "REVIEW(r!)" "TEST(p!)" "|" "DELIVERY(e@/!)")
           (sequence "WTF(w)" "XXX(x)" "|" "WORTHY(y!)")
           (sequence "|" "CANCELED(c@)")))
    org-todo-keyword-faces
       (quote
         (("TODO" . org-warning)
           ("BUG" . org-warning)
           ("WTF" . "black")
           ("CANCELED" . (:foreground "blue" :weight bold))))
       org-confirm-babel-evaluate xorns-org-confirm-babel-evaluate)
  ;; TODO: (setq-default org-enforce-todo-dependencies t)
  )


(when (featurep 'ob-shell)
  (unless (assoc 'shell org-babel-load-languages)
    (setq-default org-babel-load-languages
      (cons '(shell . t) org-babel-load-languages)))
  (setq-default org-babel-default-header-args:shell
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
          (expand-file-name "asksp" (dir-join (getenv "HOME") ".local" "bin"))
          "\n")))
    (if shebang
      (unless (string-prefix-p prefix (cdr shebang))
        (setcdr shebang (concat full-prefix (cdr shebang))))
                                        ; else
      (setq shebang (cons :shebang full-prefix)))
    (setq-default org-babel-default-header-args:shell
      (cons shebang
        (assq-delete-all :shebang org-babel-default-header-args:shell)))))


(when (featurep 'ob-python)
  (unless (assoc 'python org-babel-load-languages)
    (setq-default org-babel-load-languages
      (cons '(python . t) org-babel-load-languages)))
  (setq-default org-babel-default-header-args:python
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
      (unless (string-prefix-p prefix (cdr preamble))
        (setcdr preamble (concat full-prefix (cdr preamble))))
                                        ; else
      (setq preamble (cons :preamble full-prefix)))
    (setq-default org-babel-default-header-args:python
      (cons preamble
        (assq-delete-all :preamble org-babel-default-header-args:python)))))


(when (featurep 'dictionary)
  (global-set-key (kbd "C-c w") 'dictionary-search)
  (setq-default
    dictionary-use-single-buffer t))


(when (featurep 'wget)
  (setq-default wget-download-directory
    (xorns-preferred-directory "~/Downloads" "~/download" "~/softlib" "~")))


(add-hook 'org-mode-hook           ; run when entering org mode
  (lambda ()
    (turn-on-auto-fill)
    (flyspell-mode nil)
    (setq-default ispell-parser 'tex)))

(provide 'xorns-org)
;;; xorns-org.el ends here
