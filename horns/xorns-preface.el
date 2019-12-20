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
  (>>-ui/hidden-mode-line)
  (>>=ui/remove-useless-bars)
  (prefer-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8))


(defun >>-ui/hidden-mode-line ()
  "Hide the mode-line in the initial buffer."
  ;; Based on: http://bzg.fr/emacs-hide-mode-line.html
  (when mode-line-format
    (setq mode-line-format nil)
    (force-mode-line-update)))


(defun >>=ui/remove-useless-bars ()
  "Remove tool bar, menu bar, and scroll bars."
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (scroll-bar-mode 0)
  (tooltip-mode 0))


(provide 'xorns-preface)
;;; xorns-preface.el ends here
