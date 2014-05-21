;;; xorns-text --- Merchise text modes, and its idiosyncratic commands

;; Copyright (C) 2014 Merchise Autrement

;; Author: Medardo Rodriguez <med@merchise.org>
;; URL: http://dev.merchise.org/emacs/xorns-text
;; Keywords: initialization, merchise, convenience
;; Version: 20140322.0937

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
(require 'fill-column-indicator)

(require 'auto-complete nil 'noerror)



;;; Custom Variables and Settings

;; Turn ON parenthesis matching
(show-paren-mode t)

(custom-set-variables
  ; Consecutive years replaced with range
  '(copyright-year-ranges t)
  ; Add a newline automatically at the end of the file
  '(require-final-newline t)
  ; Parenthesis matching style
  '(show-paren-style 'mixed)
  )


;; Typed text replaces the selection
(delete-selection-mode 1)


;; Fill Column Indicator parameters
(when (featurep 'fill-column-indicator)
  (custom-set-variables
    '(fci-rule-width 1)
    '(fci-rule-color "#cccccc")))


(defun xorns-fci-mode-on ()
  "Set `fci-mode' on.

Don't fail if `'fill-column-indicator' is not available."
  (if (featurep 'fill-column-indicator)
    (fci-mode t)
    ;else
    (xorns-missing-feature 'fill-column-indicator)))



;;; Hooks

(add-hook 'before-save-hook 'copyright-update)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; TODO: (add-hook 'before-save-hook 'time-stamp)


(add-hook 'text-mode-hook
  (lambda ()
    (condition-case err
      (linum-mode 1)
      (error (message "error@text-mode-hook: %s" err)))))


(add-hook 'tex-mode-hook           ; run when entering generic-TeX mode
  (lambda ()
    (condition-case err
      (setq ispell-parser 'tex)
      (error (message "error@tex-mode-hook: %s" err)))))


(add-hook 'rst-mode-hook           ; run when entering reStructuredText mode
  (lambda ()
    (condition-case err
      (progn
	(turn-on-auto-fill)
	(flyspell-mode)			; When used flyspell-prog-mode I
					; can't see the errors while typing
	(setq ispell-parser 'tex)
	(xorns-fci-mode-on))
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
(define-key rst-mode-map "\C-cil" 'ispell-change-dictionary)


;; For outline minor modes
;; TODO: This is defined by standard mode inner "C-c@'
(define-key outline-minor-mode-map (kbd "C-=") 'show-subtree)
(define-key outline-minor-mode-map (kbd "M-=") 'hide-subtree)



;;; Install text dependencies

;;;###autoload
(defun xorns-text-dependencies-install ()
  "Install all dependencies of text modes."
  (xorns-dependency-install 'fill-column-indicator)
  (xorns-dependency-install 'auto-complete)
  )


(provide 'xorns-text)
;;; xorns-text.el ends here
