;;; setup.el --- Merchise extensions for Emacs (setup program)

;; Copyright (c) 2014 Merchise Autrement

;; Author: Medardo Rodriguez <med@merchise.org>
;; Version: 0.1
;; Keywords: merchise, extensions, setup
;; URL: http://dev.merchise.org/xorns/setup

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
;; or type `C-h C-c` in Emacs.

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;;; Commentary:

;; This program install all "Xorns" components and make them ready to
;; Emacs: a main ELPA package, a set of shell scripts that make better
;; experience from terminals and an improved collection of YAS snippets.

;; Customization for variable are registered in some special functions
;; that can be easily called from "~/.emacs" or "~/.emacs.d/init.el"
;; initialization files.

;; This program is executed inside `setup.py' by:
;;   $ emacs --load=setup.el --batch

;;
;; TODO: emacs --eval='(package-install-file "~/tmp/xorns-0.2.tar")' --batch


;;; Code:


;; Server
;;
;; Start the server, so "emacsclient" could automatically communicate.
;; In project "xemacs" there are two scripts to use instead "emacsclient":
;; "emc" and "emacs-nw".
(require 'server)
(unless (server-running-p)
  (server-start))


;; Load Merchise util modules
;; Refactors xars and xelp into a ELPA-compatible package.
(load-file (locate-user-emacs-file "xars.el"))
(load-file (locate-user-emacs-file "xelp.el"))


;;; Default (Hard) requirements.  Soft requirements must be done with the
;;; ``(require 'whatever nil 'noerror)`` pattern.
(require 'auto-complete)



;; Interactively Do Things (highly recommended, but not strictly required)
(when (require 'ido nil 'noerror)
   (ido-mode t))



;;; Project support with projectile
(when (require 'projectile nil 'noerror)
   (projectile-global-mode t)
   (add-to-list 'projectile-project-root-files "setup.py"))



;; Globally discover Emacs functions with discover.el.
;; See http://t.co/IwZnrqQBRO
(if (not (version< emacs-version "24.3"))
  (when (require 'discover nil 'noerror)
    (global-discover-mode)))



;;; Modules customization

;; Use "dired-single"
;; TODO: Why ``(require 'dired-single)`` doesn't function?

;; (load-file "~/.emacs.d/el-get/dired-single/dired-single.el")

;; Use (set-register "/home/med/work/merchise" (dired-save-positions)) and
;; ````

;; Customize dired-single so that enter, click and ^ reuse the buffer instead
;; of creating a new one.
(when (require 'dired-single nil 'noerror)
   (defun xemax-setup-dired-single ()
      (define-key dired-mode-map [return] 'dired-single-buffer)
      (define-key dired-mode-map [M-S-down] 'dired-single-buffer)
      (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
      (define-key dired-mode-map [M-S-up]
	 #'(lambda nil (interactive) (dired-single-buffer "..")))
      (define-key dired-mode-map "^"
	 #'(lambda nil (interactive) (dired-single-buffer ".."))))
      (if (boundp 'dired-mode-map)
	 (xemax-setup-dired-single)
	 (add-hook 'dired-load-hook 'xemax-setup-dired-single)))


;;; Special merchise hooks
(add-hook
   'find-file-hook
   ;;; renames the buffer to project-name:.../file.py if needed/possible
   'xelp-find-better-unique-buffer-name)



;;; General major mode hooks
(add-hook
   'after-init-hook
   (lambda ()
      ;; (run-with-idle-timer 0.1 nil 'toggle-x-maximize)
      (global-flycheck-mode)
      (yas-global-mode 1)
      (toggle-x-maximize)

      ;; Set the ~/.local/bin and ~/bin into exec-pat
      (add-to-list 'exec-path "~/.local/bin")
      (add-to-list 'exec-path "~/bin")
      )
   )

(add-hook
   'after-make-frame-functions
   ;; (run-with-idle-timer 0.1 nil 'toggle-x-maximize)
   (lambda (frame)
      (run-with-idle-timer 0.1 nil 'toggle-x-maximize)
      )
   )

(add-hook
   'ibuffer-mode-hook
   (lambda ()
      (ibuffer-switch-to-saved-filter-groups "ibuffer-groups")
      )
   )


;;; Programming Mode Hooks

(add-hook
  'emacs-lisp-mode-hook
  (lambda ()
    (turn-on-auto-fill)
    (auto-complete-mode t)
    (fci-mode t)
    (flyspell-prog-mode)
    )
  )


(add-hook
   'rst-mode-hook
   (lambda ()
      (define-key rst-mode-map "\C-cil" 'ispell-change-dictionary)
      (turn-on-auto-fill)
      (flyspell-mode)			; When used flyspell-prog-mode I
					; can't see the errors while typing
      (flyspell-ignore-tex)
      (fci-mode t)))

(add-hook
   'python-mode-hook
   (lambda ()
      ;; General stuff
      (define-key python-mode-map "\C-m" 'newline-and-indent)
      (change-dict-to-english)

      ;; JEDI stuff
      (xars-project-jedi-setup)
      (xars-exec-path-setup)
      (jedi:setup)
      (define-key python-mode-map "\C-ch" 'jedi:show-doc)

      ;; Various minor modes
      (turn-on-auto-fill)
      (flyspell-prog-mode)  		; This is best for Python
      (fci-mode t)
      (auto-complete-mode)
      (subword-mode nil)
      (outline-minor-mode)
      (define-key outline-minor-mode-map (kbd "C-=") 'show-subtree)
      (define-key outline-minor-mode-map (kbd "M-=") 'hide-subtree)
      )
   )

(add-hook
   'coffee-mode-hook
   (lambda ()
      (turn-on-auto-fill)
      (flyspell-prog-mode)  		; This is best for Python
      (change-dict-to-english)
      (fci-mode t)
      (subword-mode nil)
      (auto-complete-mode)
      )
   )

(unless (require 'magit-install nil 'noerror)
   (add-hook
      'git-commit-mode-hook
      (lambda ()
	 (turn-on-auto-fill)
	 (flyspell-mode)
	 ;; TODO: Use .dir-locals.el
	 (change-dict-to-english))))


;;; Custom keybindings

(unless (require 'magit-install nil 'noerror)
   (global-set-key "\C-xg" 'magit-status))

(global-set-key (kbd "C-x <f2>") 'rename-buffer)
(global-set-key (kbd "C-x <f5>") 'revert-buffer)
(global-set-key (kbd "C-c w") 'dictionary-search)
(global-set-key (kbd "C-c r") 'rgrep)
(global-set-key (kbd "C-M-g") 'xelp-next-grep-result)

(defcustom xars-prefer-helm-buffer-list nil
   "Indicates if prefer helm buffer list over ibuffer."
   :group 'xars
   :type 'boolean)


;; Use the helm-buffer-list if helm-mode is active [and
;; xars-prefer-helm-buffer-list] is non nil.
(global-set-key (kbd "C-x C-b")
   (lambda ()
      (interactive)
      (cond
	 ((and helm-mode xars-prefer-helm-buffer-list) (helm-buffers-list))
	 ((functionp 'ibuffer) (ibuffer)))))




;;; Enable some disabled commands

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)



;;; Environment settings
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)

(setq query-replace-highlight t)
(setq search-highlight t)
(setq font-lock-maximum-decoration t)
(fset 'yes-or-no-p 'y-or-n-p)
;; (setq confirm-kill-emacs 'yes-or-no-p)
(setq require-final-newline t)
;; (setq c-basic-offset 2)
(setq tab-width 4)
(setq major-mode 'text-mode)
(delete-selection-mode 1)

;; turn on paren matching
(when (show-paren-mode t)
   (setq show-paren-style 'mixed))

;; Get rid of the startup screen
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;; Get back font antialiasing
(push '(font-backend xft x) default-frame-alist)

;(global-font-lock-mode t t)
(setq font-lock-maximum-decoration t)

(mouse-wheel-mode t)

;; Set shift-(left, right, up, down) to move between windows
(windmove-default-keybindings)

;; Fill Column Indicator
(setq fci-rule-width 1)
(setq fci-rule-color "darkblue")



;;; Finally, load all customized variables
;; TODO: Extend for MacOS
(let* ((user-custom-file
	  (locate-user-emacs-file
	     (concat "custom-" user-real-login-name ".el")))
       (user-custom-file
	  (if (file-exists-p user-custom-file)
	     user-custom-file
	     (locate-user-emacs-file "custom.el"))))
   (when (file-exists-p user-custom-file)
      (progn
	 (setq custom-file user-custom-file)
	 (load custom-file 'noerror))))



;;; Load gnus settings
(add-hook 'gnus-load-hook
   (lambda ()
      (let* ((user-gnus-file
		(locate-user-emacs-file
		   (concat "gnus-" user-real-login-name ".el")))
	       (user-gnus-file
		  (if (file-exists-p user-gnus-file)
		     user-gnus-file
		     (locate-user-emacs-file "gnus.el"))))
	 (when (file-exists-p user-gnus-file)
	    (message "Loading gnus configuration file %s" user-gnus-file)
	    (load-file user-gnus-file)))))


;;; setup.el ends here
