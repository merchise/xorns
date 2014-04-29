;;; xorns-term --- Terminal support

;; Copyright (C) 2014 Merchise Autrement

;; Author: Medardo Rodriguez <med@merchise.org>
;; URL: http://dev.merchise.org/emacs/xorns-term
;; Keywords: initialization, merchise, convenience
;; Version: 20140325.1247

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

;; Configure use of `ansi-term' with a system shell or a python shell.
;; Define the key\-binding `C-c t' to launch the terminal shell.
;;
;; This module is automatically used when::
;;
;;     (require 'xorns)

;; Enjoy!


;;; Code:

(require 'xorns-utils)


(defgroup xorns-term nil
   "Running `xorns' shell from within Emacs buffers."
   :prefix "xorns-term-"
   :group 'xorns
   :group 'shell)


(setenv "PATH" (concat (getenv "PATH") ":/home/med/.local/bin"))
;; TODO: (setq eshell-path-env (concat eshell-path-env ":/home/med/.local/bin"))

;; eshell-load-hook
;; (eshell/addpath "/home/med/.local/bin")


(defun -safe-cmd (cmd)
  "Private function to test if CMD is a valid executable or nil."
  (or (null cmd) (executable-find cmd)))


(defcustom xorns-system-shell nil
  "System shell command name.
Preferred system shell command.  The definitive command to execute,
is calculated by the function of equal name."
  :type 'string
  :safe '-safe-cmd
  :require 'xorns-term
  :group 'xorns-term)


(defcustom xorns-python-shell nil
  "Python shell command name.
Preferred python shell command.  The definitive command to execute,
is calculated by the function of equal name."
  :type 'string
  :safe '-safe-cmd
  :require 'xorns-term
  :group 'xorns-term)


(defun xorns-system-shell ()
  "Command to use as system shell.

To calculate the value, test first the custom value of equal name and
if not valid, looks up in a list of alternatives (in order):
environment variables `ESHELL' and `SHELL', custom Emacs variable
`shell-file-name', any of [`bash' `sh' `ksh' `zsh' `tclsh' `csh'
`tcsh']."
  (let ((variants
	  (list
	    (getenv "ESHELL")
	    (getenv "SHELL")
	    (xorns-get-value 'shell-file-name)
	    "bash" "sh" "ksh" "zsh" "tclsh" "csh" "tcsh")))
    (apply 'xorns-executable-find xorns-system-shell variants)))


(defun xorns-python-shell ()
  "Command to use as python shell.

To calculate the value, test first the custom value of equal name and
if not valid, looks up in a list of alternatives (in order):
`ipython', custom Emacs variable `python-command', environment
variable `PYTHON' and custom variables `python-python-command' and
`python-jython-command'."
  (let ((variants
	  (list
	    (xorns-get-value 'python-shell-interpreter)
	    "ipython"
	    "python"
	    (getenv "PYTHON"))))
    (apply 'xorns-executable-find xorns-python-shell variants)))


(defun xorns-python3-shell ()
  "Command to use as python\-3 shell.

In this case there is not a paired custom variable.  To calculate the
value to return, this function tests first two alternatives:
`ipython3' and `python3'.  If none is valid, use the logic for the
python shell defined in function `xorns-python-shell'."
  (let ((py3 (xorns-executable-find "ipython3" "python3")))
    (or py3 (xorns-python-shell))))


;;;###autoload
(defun xorns-ansi-term (&optional arg)
  "Start a terminal\-emulator in a new buffer.

The meaning of optional argument ARG depends of `major-mode' value.
Non nil means alternative shell, if `major-mode' is not `python-mode'
*base* is a system shell and *alternative* is a python shell;
otherwise the logic is inverted.  If ARG is number `3' (independently
of `major-mode') try to run a `python-3' shell if installed.

The base shell to execute is defined in the custom variable
`xorns-base-shell'; if it is nil, use the function
`xorns-default-shell'.

The python shell to execute is defined in the custom variable
`xorns-python-shell'; if it is nil, use the function
`xorns-default-python-shell'."
  (interactive "P")
  (let*
    ((in-python (eq major-mode 'python-mode))
     (shell
       (cond
	 ((null arg) (if in-python 'Python 'System))
	 ((= (prefix-numeric-value arg) 3) 'Python-3)
	 ((if in-python 'System 'Python))))
     (cmd
       (cond
	 ((eq shell 'System) (xorns-system-shell))
	 ((eq shell 'Python)  (xorns-python-shell))
	 ((xorns-python3-shell))))
      (buf-name (format "%s Shell" shell))
      (starred (format "*%s*" buf-name))
      (cur-buf (get-buffer starred))
      (cur-proc (get-buffer-process cur-buf)))
    (if cur-buf
      (if cur-proc
	(progn
	  (setq cmd nil)
	  (switch-to-buffer cur-buf))
	;else
	(message ">>> Killing buffer: %s" starred)
	(kill-buffer cur-buf)))
    (when cmd
      (message ">>> Opening: %s" starred)
      (ansi-term cmd buf-name))))


(global-set-key (kbd "C-c t") 'xorns-ansi-term)


(provide 'xorns-term)
;;; xorns-term.el ends here
