;;; make-tools.el --- Install Xorns in local ELPA directory

;;; Code:


;; For example:
;;   (>>=eval-sexp
;;     (expand-file-name "init.el" pkg-dir)
;;     "(setq package-archives")
(defun >>=eval-sexp (lisp-file prefix)
  "Execute as Lisp code the sexp defined in a LISP-FILE using a PREFIX."
  (condition-case err
    (with-temp-file lisp-file
      (insert-file-contents lisp-file)
      (re-search-forward prefix)
      (re-search-backward "(")
      (mark-sexp)
      (eval-region (point) (mark)))
    (error
      (message "Error '%s' executing sexp '%s' in '%s'."
	err prexif lisp-file))))


(provide 'make-tools)
;;; make-tools.el ends here
