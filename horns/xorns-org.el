;;; xorns-org.el --- Xorns Configuration for miscellanies

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; TODO: Completely refactor this module


;;; Code:


(require 'org nil 'noerror)
(require 'ob-core nil 'noerror)
(require 'ob-shell nil 'noerror)
(require 'ob-python nil 'noerror)
(require 'calendar nil 'noerror)


(defgroup xorns-org nil
  "Xorns outline-based organizer."
  :tag "xorns"
  :group 'xorns)


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
	;; else
        xorns-org-confirm-babel-evaluate
        )
      ;; else
      res))
  )


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
          (expand-file-name "asksp" (>>=dir-join "~" ".local" "bin"))
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



(add-hook 'org-mode-hook           ; run when entering org mode
  (lambda ()
    (turn-on-auto-fill)
    (flyspell-mode nil)
    (setq-default ispell-parser 'tex)))


(provide 'xorns-org)
;;; xorns-org.el ends here
