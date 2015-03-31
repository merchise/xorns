;;; xorns-dired --- Merchise extensions for `dired'

;; Copyright (C) 2014, 2015 Merchise Autrement

;; Author: Medardo Rodriguez <med@merchise.org>
;; URL: http://dev.merchise.org/emacs/xorns-dired
;; Keywords: initialization, merchise, convenience
;; Version: 20140319.1548

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

;; Configure and extend all `dired' dependencies in the way of Merchise.
;;
;; Improve `dired-single' by remembering parent position for recovering it
;; when navigating up.
;;
;; This module is automatically used when::
;;
;;     (require 'xorns)

;; Enjoy!


;;; Code:

(require 'xorns-utils nil 'noerror)
(require 'dired)
(require 'dired-single nil 'noerror)



(defun -mac-os ()
  "My current version of Mac OS X shell isn't bash with all is goodies."
  (require 'ns nil 'noerror))


(setq
  dired-dwim-target t
  ;; Next is because `ls' used in Mac Os X is a BSD version and it doesn't
  ;; have any extended option (those starting with `--'), particularly
  ;; Merchise preferred `group-directories-first'.
  dired-listing-switches
    (if (-mac-os)
      "-lah"
      "-la --group-directories-first -h")
  )


(defcustom xorns-recursive-dired-ignore-switches
  '(".git/*" "*.py?" "*.elc" "*.so" ".gitignore")
  "Switches passed to `ls' for extended recursive Dired.

Each one will be passed as a `--ignore' parameter.  When Direct is called with
this function, `-R' and `-B' will be passed automatically to `ls' in addition
to all the value defined in `dired-listing-switches'."
  :type '(repeat string)
  :group 'dired
  :group 'xorns)


(defun xorns-clean-recursive-dired-switches (params)
  "Remove all invalid PARAMS to apply ignore patterns in recursive Dired.

To allow applying `--ignore' switches in recursive `ls' command, there are
options that must be disabled in order this work properly.

See also `bash' `ls' command and `xorns-recursive-dired' function."
  (let ((switches dired-listing-switches))
    (setq switches
      (replace-regexp-in-string " -[b-z0-9]*\\(a\\)" ""
	switches t t 2)
      )
    (replace-regexp-in-string REGEXP "" STRING &optional FIXEDCASE LITERAL SUBEXP)
    )
  (format "%s -BR %s"
    dired-listing-switches
    ;; string-match
    ;; match-string
    ;; replace-match
    ;; replace-regexp-in-string
    (mapconcat
      #'(lambda (arg) (format "--ignore='%s'" arg))
      xorns-recursive-dired-ignore-switches
      " ")))


(defun xorns-get-recursive-dired-switches ()
  "Calculate  `ls' ignore arguments to be used in `xorns-recursive-dired'."
  (format "%s -BR %s"
    dired-listing-switches
    (mapconcat
      #'(lambda (arg) (format "--ignore='%s'" arg))
      xorns-recursive-dired-ignore-switches
      " ")))


;; TODO: This function won't work in MacOsX or other Unixes.
(defun xorns-recursive-dired (&optional arg)
  "Refresh the Dired buffer using recursive switch.
Switches defined in `xorns-recursive-dired-ignore-switches' are used in
addition to those in `dired-actual-switches'.  To configure prefix standard
switches, customize `dired-listing-switches' variable.
With a prefix ARG, print a message with ctual parameters."
  (interactive "P")
  (when dired-sort-inhibit
    (error "Cannot sort this dired buffer"))
  (let ((params (xorns-get-recursive-dired-switches)))
    (if arg
      (message "Recursive Dired: %s" params))
    (dired-sort-other params)))


(defun xorns-setup-dired-single ()
  "Customize `dired-single' key-bindings.

After this function is called;  Enter, Click and ^ reuse the buffer
instead of creating a new one.

If `dired-single' is not installed, does nothing."
  (when (featurep 'dired-single)
    (declare-function dired-single-buffer 'dired-single)
    (define-key dired-mode-map [return] 'dired-single-buffer)
    (define-key dired-mode-map [M-S-down] 'dired-single-buffer)
    (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
    (define-key dired-mode-map "r" 'xorns-recursive-dired)
    (define-key dired-mode-map [M-S-up]
      #'(lambda () (interactive) (dired-single-buffer "..")))
    (define-key dired-mode-map "^"
      #'(lambda () (interactive) (dired-single-buffer "..")))))


(when (xorns-configure-p 'basic)
  (if (boundp 'dired-mode-map)
    (xorns-setup-dired-single)
    ; else
    (add-hook 'dired-load-hook 'xorns-setup-dired-single)))


;; ;; TODO: To preserve positions, use::
;; (set-register
;;   (intern
;;     (xorns-default-directory))
;;   (dired-save-positions))


(provide 'xorns-dired)
;;; xorns-dired.el ends here
