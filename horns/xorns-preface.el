;;; xorns-preface.el --- Common Systems Tools

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module is loaded just once even before package system
;; (`xorns-packages') it is initialized.
;;
;; - `xorns-hide-mode-line' is an Emacs mode for hiding the mode-line.  This
;;   is a preface special feature to hide the mode-line during initialization
;;   process to improve load performance by smoothing UI.  This minor mode
;;   also helps switching mode-line on and off.
;;
;; - `>>=ui/remove-useless-bars' function to remove tool bar, menu bar, and
;;   scroll bars.
;;
;; - Set `prefer-coding-system' and `locale-coding-system' to utf-8.

;;; Code:

(with-eval-after-load 'xorns-preface
  (hidden-mode-line-mode)
  (>>=ui/remove-useless-bars)
  (prefer-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  )



;; Taken from: http://bzg.fr/emacs-hide-mode-line.html

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




(defun >>=ui/remove-useless-bars ()
  "Remove tool bar, menu bar, and scroll bars."
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (scroll-bar-mode 0)
  (tooltip-mode 0))


(provide 'xorns-preface)
;;; xorns-preface.el ends here
