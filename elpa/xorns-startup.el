;;; xorns-startup --- Basic initialization

;; Copyright (C) 2014-2016 Merchise

;; Author: Medardo Rodriguez <med@merchise.org>
;; URL: http://dev.merchise.org/emacs/xorns-startup
;; Keywords: initialization, merchise, convenience
;; Version: 20150516.1620

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
(require 'scroll-bar)
(require 'mule)
(require 'tramp)
(require 'auto-complete nil 'noerror)

;; See https://wiki.archlinux.org/index.php/emacs#Dead-accent_keys_problem:_.27.3Cdead-acute.3E_is_undefined.27
(require 'iso-transl)


;; Allow this Emacs process to be a server for client processes
(if (not (server-running-p))
  (server-start))


;; Usability Interface Configuration
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(when (functionp 'mouse-wheel-mode)
  (mouse-wheel-mode t))    ; Mouse wheel support
(fset 'yes-or-no-p 'y-or-n-p)   ; Replace `yes|not' commands for simpler `[yn]'


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


;; Cutting and pasting uses the clipboard
(setq x-select-enable-clipboard t)

;; Key to start auto-complete
(setq  ac-trigger-key "TAB")



;;; Other standard or ELPA packages initialization

;; Set shift-(left, right, up, down) to move between windows

(require 'windmove nil)
(when (featurep 'windmove)
  (windmove-default-keybindings 'ctrl)
  (setq windmove-wrap-around t))


;; Interactively do things with buffers and files
(require 'ido)
(ido-mode t)

(global-set-key (kbd "C-x b") 'ido-switch-buffer)  ;; Just to make sure a
                                                   ;; previous binding is
                                                   ;; properly set.


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



;;; Enable some disabled commands

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)



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
