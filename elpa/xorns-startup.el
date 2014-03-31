;;; xorns-startup --- Basic initialization

;; Copyright (C) 2014 Merchise Autrement

;; Author: Medardo Rodriguez <med@merchise.org>
;; URL: http://dev.merchise.org/emacs/xorns-startup
;; Keywords: initialization, merchise, convenience
;; Version: 20140324.1017

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

;; This module configure all Merchise preferences on how to use Emacs
;; in a generic way: frames, windows, menus, tool-bars, initial
;; messages, etc.

;; This module is automatically used when::
;;
;;     (require 'xorns)

;; Enjoy!


;;; Code:

(require 'server)
(require 'font-lock)
(require 'mule)


;; Allow this Emacs process to be a server for client processes
(if (not (server-running-p))
  (server-start))


;; Maximize each new frame including the initial one
(push '(fullscreen . maximized) default-frame-alist)


;; Get back font anti-aliasing
(push '(font-backend xft x) default-frame-alist)


;; Show current directory in title bar
(setq frame-title-format
  ; Original value was::
  ;    '(multiple-frames "%b" ("" invocation-name "@" system-name))
  '(multiple-frames "%b"
     (""
       invocation-name
       " -- "
       (:eval (abbreviate-file-name default-directory)))))


;; Maximum decoration level for fontification
;; (different font for each structure).
(setq font-lock-maximum-decoration t)


;; Configure coding for terminal
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)


;; Replace `yes|not' commands for simpler `[yn]'
(fset 'yes-or-no-p 'y-or-n-p)


;; Mouse wheel support
(mouse-wheel-mode t)



;;; Other standard or ELPA packages initialization

;; Set shift-(left, right, up, down) to move between windows

(require 'windmove)
(when (featurep 'windmove)
  (windmove-default-keybindings)
  (setq windmove-wrap-around t))


;; Interactively do things with buffers and files
(require 'ido)
(ido-mode t)


(when (not (version< emacs-version "24.3"))   ; Discover more of Emacs
  (require 'discover nil 'noerror)            ; See http://t.co/IwZnrqQBRO
  (when (functionp 'global-discover-mode)
    (global-discover-mode)))



;;; Custom key-bindings

(global-set-key (kbd "C-x <f2>") 'rename-buffer)
(global-set-key (kbd "C-x <f5>") 'revert-buffer)
(global-set-key (kbd "C-c r") 'rgrep)
(global-set-key (kbd "C-c d") 'xorns-pwd)
(global-set-key (kbd "C-c m") 'man)

(require 'dictionary nil 'noerror)
(if (featurep 'dictionary)
  (global-set-key (kbd "C-c w") 'dictionary-search)
  ;else
  )


(require 'rfcview nil 'noerror)
(when (featurep 'rfcview)
  (add-hook 'rfcview-mode-hook
    (lambda ()
      (condition-case err
	(progn
	  (define-key rfcview-mode-map (kbd "l") 'pop-to-mark-command))
	(error (message "error@rfcview-mode-hook: %s" err))))))



(require 'deft nil 'noerror)
(when (featurep 'deft)
  (custom-set-variables
    '(deft-auto-save-interval 60.0)
    ; TODO: Remove all deft `.emacs.d' custom files
    )
  (global-set-key (kbd "<f12>") 'deft))



;;; Enable some disabled commands

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)


;;; Some generic custom values

(custom-set-variables
  '(calendar-date-style 'iso)
  )




;;; Hooks

(add-hook 'after-init-hook
  (lambda ()
    (condition-case err
      (progn
	(add-to-list 'exec-path "~/.local/bin")
	(add-to-list 'exec-path "~/bin"))
      (error (message "error@after-init-hook: %s" err)))))


(provide 'xorns-startup)
;;; xorns-startup.el ends here
