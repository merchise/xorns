;;; xorns-text.el --- Merchise text modes, and its idiosyncratic commands

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>
;; or type `C-h C-c' in Emacs.

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
(require 'iso-transl nil 'noerror)
(require 'google-translate nil 'noerror)
(require 'google-translate-smooth-ui nil 'noerror)
(require 'xorns-utils nil 'noerror)


;;; Custom Variables and Settings

;; Turn ON parenthesis matching
(show-paren-mode t)
(xorns-set-value 'show-paren-mode t)

(xorns-set-values
  ; Consecutive years replaced with range
  '(copyright-year-ranges t)
  ; Add a newline automatically at the end of the file
  '(require-final-newline t)    ; >>=
  ; Do not display continuation lines
  '(truncate-lines t)
  ; Parenthesis matching style
  '(show-paren-style 'mixed)
  )


;; Typed text replaces the selection
(delete-selection-mode 1)


;; Fill Column Indicator parameters
(when (featurep 'linum)
  (global-linum-mode t)
  )


(when (featurep 'ispell)
  (xorns-set-values
    '(ispell-highlight-p t)
    '(ispell-silently-savep t)))









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



;;; Hooks

;;(add-hook 'before-save-hook 'copyright-update)
(add-hook 'before-save-hook 'delete-trailing-whitespace)    ; >>=
;; TODO: (add-hook 'before-save-hook 'time-stamp)


(add-hook 'text-mode-hook
  (lambda ()
    (condition-case err
      (xorns-try-linum-mode)
      (error (message "error@text-mode-hook: %s" err)))))


(add-hook 'tex-mode-hook           ; run when entering generic-TeX mode
  (lambda ()
    (condition-case err
      (xorns-set-value 'ispell-parser 'tex)
      (error (message "error@tex-mode-hook: %s" err)))))


(add-hook 'rst-mode-hook           ; run when entering reStructuredText mode
  (lambda ()
    (condition-case err
      (progn
        (turn-on-auto-fill)
        (flyspell-mode nil)             ; When used flyspell-prog-mode I
                                        ; can't see the errors while typing
        (xorns-set-value 'ispell-parser 'tex)
	(xorns-set-value 'rst-new-adornment-down t))
      (error (message "error@rst-mode-hook: %s" err)))))


(defun xorns-auto-complete-mode ()
  "Turn ON `auto-complete' mode in a safe way.

If this feature is not installed don't fail and just report a message."
  (if (featurep 'auto-complete)
    (auto-complete-mode t)
                                        ;else
    (xorns-missing-feature 'auto-complete)))



;;; Key-bindings

;; It only function in RST major mode and if `ispell' is enabled.
;; TODO: Check which other modes needs this definition.
(define-key key-translation-map (kbd "M-[") 'iso-transl-ctl-x-8-map)
(define-key rst-mode-map "\C-cil" 'ispell-change-dictionary)
(global-set-key (kbd "C-|") 'google-translate-smooth-translate)


;; For outline minor modes
;; TODO: This is defined by standard mode inner "C-c@'
(define-key outline-minor-mode-map (kbd "C-=") 'outline-show-subtree)
(define-key outline-minor-mode-map (kbd "M-=") 'outline-hide-subtree)
(define-key outline-minor-mode-map (kbd "C-+") 'xorns-toggle-subtree)


(provide 'xorns-text)
;;; xorns-text.el ends here
