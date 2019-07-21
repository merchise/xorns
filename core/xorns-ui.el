;;; xorns-ui.el --- Xorns UI Library

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This library defines several utilities used to configure GUI stuffs.

;;; Code:

(require 'easy-mmode)


(defun >>=ui/remove-rubbish ()
  "Remove tool bar, menu bar, and scroll bars."
  (mapcar
    (lambda (name)
      (let ((mode (intern (format "%s-mode" name))))
	(when (and (fboundp mode) (symbol-value mode))
	  (funcall mode -1))))
    '(tool-bar menu-bar scroll-bar tooltip)))



;; http://bzg.fr/emacs-hide-mode-line.html

(defvar-local hidden-mode-line-mode nil)
(defvar-local hide-mode-line nil)


(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global t
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))


;; If you want to hide the mode-line in every buffer by default
;; (add-hook 'after-change-major-mode-hook 'hidden-mode-line-mode)


(provide 'xorns-ui)
;;; xorns-ui.el ends here
