;;; xorns-git.el --- Integrate Emacs with GIT using `magit'

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;;; Commentary:

;; Configure all GIT preferences using `magit'.
;;
;; This module is automatically used when::
;;
;;     (require 'xorns)

;; Enjoy!


;;; Code:


(require 'magit nil 'noerror)
(require 'xorns-utils)


(if (featurep 'magit)
  (progn
    (global-set-key "\C-xg" 'magit-status)
    (global-set-key "\C-cg" 'magit-status)
    (add-hook 'git-commit-mode-hook  ; run when in `magit' mode
      (lambda ()
	(condition-case err
	  (progn
	    (turn-on-auto-fill)
	    (flyspell-mode nil))
	  (error (message "error@git-commit-mode-hook: %s" err))))))
  ;else
  (xorns-missing-feature 'magit))


(provide 'xorns-git)
;;; xorns-git.el ends here
