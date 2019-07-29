;;; xorns-ui.el --- Xorns UI Library

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This library defines several utilities used to configure GUI stuffs.

;;; Code:

(require 'easy-mmode)


(defvar >>=|frame-title-format
  '(multiple-frames "%b"
     ("" invocation-name " -- "
       (:eval (abbreviate-file-name default-directory))))
  "Template for displaying the title bar of visible frames.")


(defun >>=configure-default-user-interface ()
  )


(defun >>=ui/header-mode-line ()
  (interactive)
  (if (not header-line-format)
      (setq header-line-format
	'(multiple-frames "%b"
     ("" invocation-name " -- "
       (:eval (abbreviate-file-name default-directory)))))
    ;; mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
    ;; (vc-mode vc-mode)
    ;;"  " mode-line-modes mode-line-misc-info mode-line-end-spaces))
    (setq header-line-format nil))
  (force-mode-line-update))


(defun >>=ui/remove-useless-bars ()
  "Remove tool bar, menu bar, and scroll bars."
  (mapcar
    (lambda (name)
      (let ((mode (intern (format "%s-mode" name))))
	(when (and (fboundp mode) (symbol-value mode))
	  (funcall mode -1))))
    '(tool-bar menu-bar scroll-bar tooltip)))


(defun >>=frame-title-init ()
  "Configure template for displaying the title bar of visible frames.
See `frame-title-format' variable."
  ;; TODO: Spacemacs uses a function to prepare variable value
  (when (and (display-graphic-p) >>=|frame-title-format)
    (require 'format-spec)
    (setq frame-title-format >>=|frame-title-format)))


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
