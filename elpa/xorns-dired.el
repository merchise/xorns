;;; xorns-dired --- Merchise extensions for `dired'

;; Copyright (C) 2014-2017 Merchise Autrement [~º/~]

;; Author: Medardo Rodriguez <med@merchise.org>
;; URL: http://dev.merchise.org/emacs/xorns-dired
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

;; TODO: Create a function to toggle visibility of ignored files, see
;; `xorns-dired-recursive-ignore-switches' variable.

(require 'xorns-utils nil 'noerror)
(require 'dired)
(require 'dired-x)
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


(defcustom xorns-dired-recursive-ignore-switches
  '(".git/*" "*.py?" "*.elc" "*.so" ".gitignore")
  "Switches passed to `ls' for extended recursive Dired.

Each one will be passed as a `--ignore' parameter.  When Direct is called with
this function, `-R' and `-B' will be passed automatically to `ls' in addition
to all the value defined in `dired-listing-switches'."
  :type '(repeat string)
  :group 'dired
  :group 'xorns)


(defun -dired-define-keys (keys def)
  "In define several key sequences KEYS with the same function DEF.
Always use `dired-mode-map' as the keymap.

See `define-key' function for more information."
  (mapcar
      #'(lambda (key)
	  (define-key dired-mode-map key def))
      keys))


(defun xorns-dired-clean-recursive-switches (params)
  "Remove all invalid PARAMS to apply ignore patterns in recursive Dired.

To allow applying `--ignore' switches in recursive `ls' command, there are
options that must be disabled in order this work properly.

See also `bash' `ls' command and `xorns-dired-recursive' function."
  (let ((switches dired-listing-switches))
    (setq switches
      (replace-regexp-in-string " -[b-z0-9]*\\(a\\)" ""
	switches t t 2)
      )
    ;; TODO: Finish and use this function
    )
  (format "%s -BR %s"
    dired-listing-switches
    ;; string-match
    ;; match-string
    ;; replace-match
    ;; replace-regexp-in-string
    (mapconcat
      #'(lambda (arg) (format "--ignore='%s'" arg))
      xorns-dired-recursive-ignore-switches
      " ")))


(defun xorns-dired-get-recursive-switches ()
  "Calculate  `ls' ignore arguments to be used in `xorns-dired-recursive'."
  (format "%s -BR %s"
    dired-listing-switches
    (mapconcat
      #'(lambda (arg) (format "--ignore='%s'" arg))
      xorns-dired-recursive-ignore-switches
      " ")))


;; TODO: This function could not work in MacOsX or other Unixes, this is
;; because old 'bash' versions not allowing `--ignore' or
;; `--group-directories-first' options.
(defun xorns-dired-recursive (&optional arg)
  "Refresh the Dired buffer using recursive switch.
Switches defined in `xorns-dired-recursive-ignore-switches' are used in
addition to those in `dired-actual-switches'.  To configure prefix standard
switches, customize `dired-listing-switches' variable.
With a prefix ARG, print a message with ctual parameters."
  (interactive "P")
  (when dired-sort-inhibit
    (error "Cannot sort this dired buffer"))
  (let ((params (xorns-dired-get-recursive-switches)))
    (if arg
      (message "Recursive Dired: %s" params))
    (dired-sort-other params)))


;;;###autoload
(defun xorns-dired-single-buffer (&optional default-dirname)
  "Visit selected directory in current buffer.
Improve default `dired-single-buffer' by remembering parent position for
recovering it when navigating up.

Optional argument DEFAULT-DIRNAME specifies the directory to visit; if not
specified, the directory or file on the current line is used (assuming it's a
dired buffer).  If the current line represents a file, the file is visited in
another window."
  (interactive)
  (let ((org (dired-current-directory)))
    (declare-function dired-single-buffer 'dired-single)    ;; FIX: review
    (dired-single-buffer default-dirname)
    (let ((dst (dired-current-directory)))
      (if (string-prefix-p dst org)
	(let* ((targets (split-string (substring org (length dst)) "/"))
	       (aux (car targets))
	       (target (if (string= aux "") (cadr targets) aux)))
	  (left-char 1)
	  (search-forward (concat " " target "\n") nil t)
	  (left-char (1+ (length target))))))))


(defun xorns-dired-single-buffer-mouse (click)
  "Mouse-initiated version of `xorns-dired-single-buffer' (which see).

Argument CLICK is the mouse-click event."
  (interactive "e")
  ;; Next code could be generalized in `xorns-utils'.
  (let* ( (start (event-start click))
	  (window (car start))
	  (pos (car (cdr start))) )
    (select-window window)
    (goto-char pos))
  (xorns-dired-single-buffer))

(defun xorns-dired-single-setup ()
  "Customize `dired-single' key-bindings.

After this function is called;  Enter, Click and ^ reuse the buffer
instead of creating a new one.

If `dired-single' is not installed, does nothing."
  (when (featurep 'dired-single)
    (declare-function dired-single-buffer 'dired-single)
    (define-key dired-mode-map "r" 'xorns-dired-recursive)
    (-dired-define-keys `([return] [M-S-down] ,(kbd "RET"))
      'xorns-dired-single-buffer)
    (-dired-define-keys `([mouse-1] [mouse-2])
      'xorns-dired-single-buffer-mouse)
    (-dired-define-keys `(,(kbd "M-P") [M-S-up] "^")
      #'(lambda () (interactive) (xorns-dired-single-buffer "..")))))


(when (xorns-configure-p 'basic)
  (if (boundp 'dired-mode-map)
    (xorns-dired-single-setup)
    ; else
    (add-hook 'dired-load-hook 'xorns-dired-single-setup))
  (add-hook 'dired-load-hook
    (lambda () (load "dired-x"))))



;; ;; TODO: To preserve positions, use::
;; (set-register
;;   (intern
;;     (xorns-default-directory))
;;   (dired-save-positions))


(provide 'xorns-dired)
;;; xorns-dired.el ends here
