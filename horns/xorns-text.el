;;; xorns-text.el --- Merchise text modes, and its idiosyncratic commands

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;;; Commentary:

;; Provides the fundamental text modes configuration.  Will include
;; all basic text modes, like `rst-mode'.

;; Because it is required in `xorns-prog', this module is automatically
;; used when::
;;
;;     (require 'xorns)

;; Enjoy!


;;; Code:

(require 'ispell)
(require 'rst)
(require 'outline)
(require 'paren)
(require 'linum)

(require 'auto-complete nil 'noerror)
(require 'google-translate nil 'noerror)
(require 'google-translate-smooth-ui nil 'noerror)
(require 'xorns-utils nil 'noerror)
(require 'xorns-buffers)


;;; Custom Variables and Settings

;; Turn ON parenthesis matching
(show-paren-mode t)
(setq-default show-paren-mode t)

(setq-default
  ;; Consecutive years replaced with range
  copyright-year-ranges t
  ;; Do not display continuation lines
  truncate-lines t
  ;; Parenthesis matching style
  show-paren-style 'mixed
  ;; Key to start auto-complete
  ac-trigger-key "TAB"
  )


;; Enable some disabled commands

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)


;; Typed text replaces the selection
(delete-selection-mode 1)


;; Fill Column Indicator parameters

(when (featurep 'ispell)
  (setq-default
    ispell-highlight-p t
    ispell-silently-savep t))



;;; Sub-tree utility
(defun xorns-toggle-subtree ()
  "Show or hide the current subtree depending on its current state."
  (interactive)
  (save-excursion
    (outline-back-to-heading)
    (if (not (outline-invisible-p (line-end-position)))
      (outline-hide-subtree)
      (outline-show-subtree)
      (outline-show-entry))))



;;; text-modes

(use-package text-mode
  :mode ("\\.log\\'" "/LICENSE\\'")
  :hook (text-mode . xorns-try-linum-mode))


(add-hook 'tex-mode-hook           ; run when entering generic-TeX mode
  (lambda ()
    (condition-case err
      (setq-default ispell-parser 'tex)
      (error (message "error@tex-mode-hook: %s" err)))))


(add-hook 'rst-mode-hook           ; run when entering reStructuredText mode
  (lambda ()
    (condition-case err
      (progn
        (turn-on-auto-fill)
        (flyspell-mode nil)             ; When used flyspell-prog-mode I
                                        ; can't see the errors while typing
        (setq-default
	  ispell-parser 'tex
	  rst-new-adornment-down t))
      (error (message "error@rst-mode-hook: %s" err)))))


(defun xorns-auto-complete-mode ()
  "Turn ON `auto-complete' mode in a safe way.

If this feature is not installed don't fail and just report a message."
  (if (featurep 'auto-complete)
    (auto-complete-mode t)
    ;; else
    (xorns-missing-feature 'auto-complete)))



;;; Key-bindings

;; It only function in RST major mode and if `ispell' is enabled.
(define-key rst-mode-map "\C-cil" 'ispell-change-dictionary)
(global-set-key (kbd "C-|") 'google-translate-smooth-translate)


;; For outline minor modes
;; TODO: This is defined by standard mode inner "C-c@'
(define-key outline-minor-mode-map (kbd "C-=") 'outline-show-subtree)
(define-key outline-minor-mode-map (kbd "M-=") 'outline-hide-subtree)
(define-key outline-minor-mode-map (kbd "C-+") 'xorns-toggle-subtree)


(provide 'xorns-text)
;;; xorns-text.el ends here
