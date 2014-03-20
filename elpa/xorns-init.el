;;; xorns-init --- Execute all Merchise members common initialization

;; Copyright (C) 2014 Merchise Autrement

;; Author: Medardo Rodriguez <med@merchise.org>
;; URL: http://dev.merchise.org/emacs/xorns-init
;; Keywords: initialization, merchise, convenience
;; Version: 20140316.1200

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

;; Write your commentary here in only one line in this moment...

;; Enjoy!


;;; Code:

;; Start server
(require 'server)
(unless (server-running-p)
  (server-start))


;;; Requires and theirs configuration

;; TODO: Include all ELPA from next packages as dependencies

(require 'outline nil 'noerror)
(require 'ispell nil 'noerror)
(require 'ibuffer nil 'noerror)
(require 'rst nil 'noerror)
(require 'python nil 'noerror)
(require 'auto-complete nil 'noerror)
(require 'xorns nil 'noerror)
(require 'xorns-prog nil 'noerror)
(require 'xorns-frames nil 'noerror)

(require 'ido nil 'noerror)                 ; Interactively Do Things
(when (functionp 'ido-mode)
  (ido-mode t))

(require 'projectile nil 'noerror)          ; Project support
(when (functionp 'projectile-global-mode)
  (projectile-global-mode t)
  (add-to-list 'projectile-project-root-files "setup.py"))

(when (not (version< emacs-version "24.3"))   ; Discover more of Emacs
  (require 'discover nil 'noerror)            ; See http://t.co/IwZnrqQBRO
  (when (functionp 'global-discover-mode)
    (global-discover-mode)))

(require 'xorns-dired nil 'noerror)         ; Directory-browsing commands
(if (boundp 'dired-mode-map)
  (xorns-setup-dired-single)
  ; else
  (add-hook 'dired-load-hook 'xorns-setup-dired-single))



;;; Hooks

;; General major mode

(add-hook 'after-init-hook         ; run after loading the init files
  (lambda ()
    (condition-case err
      (progn
	(xorns-frame-maximize)
	(add-to-list 'exec-path "~/.local/bin")
	(add-to-list 'exec-path "~/bin")
	(global-flycheck-mode)
	(yas-global-mode 1))
      (error (message "error@after-init-hook: %s" err)))))


(add-hook 'ibuffer-mode-hook       ; run upon entry into `ibuffer-mode'
  (lambda ()
    (condition-case err
      (ibuffer-switch-to-saved-filter-groups "ibuffer-groups")
      (error (message "error@ibuffer-mode-hook: %s" err)))))


(add-hook 'tex-mode-hook           ; run when entering generic-TeX mode
  (lambda ()
    (condition-case err
      (setq ispell-parser 'tex)
      (error (message "error@tex-mode-hook: %s" err)))))


(add-hook 'rst-mode-hook           ; run when entering reStructuredText mode
  (lambda ()
    (condition-case err
      (progn
	(define-key rst-mode-map "\C-cil" 'ispell-change-dictionary)
	(turn-on-auto-fill)
	(flyspell-mode)			; When used flyspell-prog-mode I
					; can't see the errors while typing
	(setq ispell-parser 'tex)
	(fci-mode t))
      (error (message "error@rst-mode-hook: %s" err)))))



;;; Programming Mode Hooks

(add-hook 'prog-mode-hook          ; run for all programming modes
  (lambda ()
    (condition-case err
      (progn
	(fci-mode t)
	(auto-complete-mode t)
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
	(xorns-project-jedi-setup)
	(xorns-exec-path-setup)
	(jedi:setup)
	(define-key python-mode-map "\C-ch" 'jedi:show-doc)
	(outline-minor-mode)
	(define-key outline-minor-mode-map (kbd "C-=") 'show-subtree)
	(define-key outline-minor-mode-map (kbd "M-=") 'hide-subtree))
      (error (message "error@python-mode-hook: %s" err)))))


;; 'emacs-lisp-mode-hook 'coffee-mode-hook


;;; Git

(unless (require 'magit-install nil 'noerror)
  (add-hook 'git-commit-mode-hook  ; run when in `magit' mode
    (lambda ()
      (condition-case err
	(progn
	  (turn-on-auto-fill)
	  (flyspell-mode)
	  ;; TODO: Use .dir-locals.el
	  (ispell-change-dictionary "english"))
      (error (message "error@git-commit-mode-hook: %s" err))))))



;;; General miscellanies

;; Maximize frame
(when (functionp 'xorns-frame-maximize)
  (add-hook 'after-make-frame-functions 'xorns-frame-maximize))


(provide 'xorns-init)
;;; xorns-init.el ends here
