;;; xorns-prog --- Programming language source code editing

;; Copyright (C) 2014 Merchise Autrement

;; Author: Medardo Rodriguez <med@merchise.org>
;; URL: http://dev.merchise.org/emacs/xorns-prog
;; Keywords: initialization, merchise, convenience
;; Version: 20140319.2129

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

;; Generic definitions for editing programming language source code.

;; This module is automatically used when::
;;
;;     (require 'xorns)

;; Enjoy!


;;; Code:


(require 'outline)
(require 'dash nil 'noerror)   ; facilities like -when-let and -mapcat
(require 'flycheck nil 'noerror)
(require 'yasnippet nil 'noerror)
(require 'python nil 'noerror)
(require 'jedi nil 'noerror)

(require 'xorns-text)


;;; Hooks

(if (featurep 'flycheck)
  (add-hook 'after-init-hook         ; run after loading the init files
    (lambda ()
      (global-flycheck-mode)))
  ;else
  (xorns-missing-feature 'flycheck))


(if (featurep 'yasnippet)
  (add-hook 'after-init-hook         ; run after loading the init files
    (lambda ()
      (yas-global-mode 1)))
  ;else
  (xorns-missing-feature 'yasnippet))


(add-hook 'prog-mode-hook          ; run for all programming modes
  (lambda ()
    (condition-case err
      (progn
	(xorns-fci-mode-on)
	(xorns-auto-complete-mode)
	(flyspell-prog-mode)
	(turn-on-auto-fill)
	(ispell-change-dictionary "english")
	(subword-mode nil))
      (error (message "error@prog-mode-hook: %s" err)))))


(add-hook 'python-mode-hook        ; run when editing python source code
  (lambda ()
    (condition-case err
      (progn
	(define-key python-mode-map "\C-m" 'newline-and-indent)
	(xorns-jedi-setup)
	(outline-minor-mode))
      (error (message "error@python-mode-hook: %s" err)))))


;;; Python

;;;###autoload
(defun xorns-jedi-setup ()
  "Setup `jedi' for current buffer."
  (if (featurep 'jedi)
    (progn
      (jedi:setup)
      (define-key python-mode-map "\C-ch" 'jedi:show-doc))
    ;else
    (xorns-missing-feature 'jedi)))




;;;###autoload
(defun xorns-prog-dependencies-install ()
  "Install all dependencies of text modes."
  (xorns-dependency-install 'flycheck)
  (xorns-dependency-install 'yasnippet)
  (xorns-dependency-install 'jedi))


(provide 'xorns-prog)
;;; xorns-prog.el ends here
