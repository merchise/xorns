;;; xorns-buffers --- Buffers management

;; Copyright (C) 2014 Merchise Autrement

;; Author: Medardo Rodriguez <med@merchise.org>
;; URL: http://dev.merchise.org/emacs/xorns-buffers
;; Keywords: initialization, merchise, convenience
;; Version: 20140324.1244

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

;; This module main features are:
;;
;; - Inhibits the start-up screen and initial message for `*scratch*'
;;   buffer.
;;
;; - Configure `C-x C-b' to list buffers using `ibuffer' instead
;;   standard `list-buffers'.  This can be change in `xorns' by
;;   requiring `xorns-helm' and set `xorns-prefer-helm-buffer-list'.
;;
;; - Set `ibuffer' groups.
;;
;; - Functionality to force `*scratch*' buffer.

;; This module is automatically used when::
;;
;;     (require 'xorns)

;; Enjoy!


;;; Code:

(eval-when-compile
  (require 'cl))

(require 'ibuf-ext nil 'noerror)

(eval-when-compile
  (require 'xorns-utils))

;; Externals to avoid warnings
(defvar xorns-home-dir)
(defvar xorns-prefered-default-directory)
(declare-function xorns-default-directory "xorns-utils.el")


;; Get rid of the startup screen and `*scratch*' buffer message
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)



;;; IBuffer

(global-set-key (kbd "C-x C-b") 'ibuffer)


;; Set `ibuffer' to loads some preferred groups.
(custom-set-variables
  '(ibuffer-saved-filter-groups
     '(("xorns-ibuffer-groups"
	 ("Dired" (or (mode . dired-omit-mode) (mode . dired-mode)))
	 ("RST" (mode . rst-mode))
	 ("XML" (mode . nxml-mode))
	 ("Emacs Lisp"
	   (or
	     (mode . emacs-lisp-mode)
	     (mode . lisp-interaction-mode)
	     (mode . lisp-mode)))
	 ("Python" (mode . python-mode))))))


(add-hook 'ibuffer-mode-hook
  (lambda ()
    (condition-case err
      (ibuffer-switch-to-saved-filter-groups "xorns-ibuffer-groups")
      (error (message "error@ibuffer-mode-hook: %s" err)))))


;;; Buffers

(defun xorns-force-scratch (&optional arg)
  "Switch to `*scratch*` buffer, creating a new one if needed.

An optional argument ARG could be given to delete other windows; if
`0' also reset `default-directory' to `xorns' default."
  (interactive "P")
  (let ((buf (get-buffer-create "*scratch*")))
    (set-buffer-major-mode buf)
    (switch-to-buffer-other-window buf)
    (if (or
	  (= (prefix-numeric-value arg) 0)
	  (equal (xorns-default-directory) xorns-home-dir))
      (setq default-directory xorns-prefered-default-directory))
    (if arg (delete-other-windows))))


(global-set-key (kbd "C-c s") 'xorns-force-scratch)



;;; Hooks

(add-hook 'after-init-hook
  (lambda ()
    (condition-case err
      ;; Set initial default directory for `*scratch*' buffer
      (if (equal (xorns-default-directory) xorns-home-dir)
	(setq default-directory xorns-prefered-default-directory))
      (error (message "error@after-init-hook: %s" err)))))


(provide 'xorns-buffers)
;;; xorns-buffers.el ends here
