;;; xorns-core.el --- Core lisp functions for xorns  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Core lisp-extension library.

;; Enjoy!


;;; Code:



;;; Lisp configuration files

(defun >>=read-lisp-config (file)
  "Read a Lisp configuration FILE."
  (if (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let (res)
	(ignore-errors
          (while (not (eobp))
	    (setq res (cons (read (current-buffer)) res))))
	(nreverse res)))))


(defun >>=write-lisp-config (file form)
  "Write a Lisp configuration FORM to a FILE."
  (with-temp-buffer
    (mapc (lambda (line) (insert (format "%S\n" line))) form)
    (write-file file)))


(provide 'xorns-core)
;;; xorns-core.el ends here
