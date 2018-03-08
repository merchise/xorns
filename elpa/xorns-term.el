;;; xorns-term --- Terminal support

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Rodriguez <med@merchise.org>
;; URL: http://dev.merchise.org/emacs/xorns-term
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

;; Configure use of `ansi-term' with a system shell or a python shell.
;; Define the key\-binding `C-c t' to launch the terminal shell.
;;
;; This module is automatically used when::
;;
;;     (require 'xorns)

;; Enjoy!


;;; Code:

(require 'term nil 'noerror)
(require 'xorns-utils nil 'noerror)


(defgroup xorns-term nil
   "Running `xorns' shell from within Emacs buffers."
   :prefix "xorns-term-"
   :group 'xorns
   :group 'shell)


(defcustom xorns-term-shells nil
  "Shell definition list to be managed by `xorns-ansi-term'.

Configuration to manage ansi\-terminal\-emulators.  The value is an
association list of the form `((IDENTIFIER . DEFINITION) ...)'.

IDENTIFIER is used to select the shell kind (for example, with a prefix
argument).  The semantic of `0' is reserved for system shell; we encourage you
use `1' for your main programmer language shell (Python, for example), and `2'
for your working project (like 'xoeuf' in Merchise).

DEFINITION is a list with the following components:

- The COMMAND to execute when creating a new terminal\-emulator, for example
  `/bin/bash'.

- A NAME\-TEMPLATE to format the initial buffer name.  The `xorns-format'
  function will be used with `{id}' for the identifier, and `{cmd}' for the
  command.

- How to PASTE (send) content to the shell process.  The PASTE method could be
  *Standard*, to use the content literally; a string containing '%s', to
  format the content using the given *template*; any other string: will yank
  the content to the clipboard and then send the given value to the shell
  process (useful in shells like 'IPython' using '%paste' magic macro); and a
  function, to format the content in a custom defined way.

- A list of MAJOR\-MODES to select a preferred shell by default.

It is not checked that there is a unique identifier for each definition.  This
is the responsibility of the user."
  :type '(repeat
	   (cons
	     (integer :tag "Identifier")
	     (choice :tag "Command"
	       (string :tag "Executable")
	       (cons :tag "With Arguments"
		 (string :tag "Executable")
		 (string :tag "Arguments")))
	       (string :tag "Name Template")
	       (choice :tag "Paste"
		 (const :tag "Standard" nil)
		 (string :tag "Template")
		 (function :tag "Custom"))
	       (repeat :tag "Major modes" symbol)))
  :group 'xorns-term)


(defvar xorns-term-preferred-shell nil
  "Mapping between major modes and preferred shells.

How to manage which ansi\-terminal\-emulator to favor in each `major-mode'.
An association list with the form `((MAJOR-MODE . IDENTIFIER) ...)' and it is
calculated from the field 'Major modes' in `xorns-term-shells'.")


(defvar xorns-term-shell-identifier nil
  "Selected `xorns-term-shells' IDENTIFIER any `ansi-term' buffer.

This variable must be buffer local.")


(defvar xorns-term-last-shell nil
  "Last shell issued.")


(defun xorns-system-shell ()
  "Command to use as system shell.

To calculate the value, test first the custom value of equal name and
if not valid, looks up in a list of alternatives (in order):
environment variables `ESHELL' and `SHELL', custom Emacs variable
`shell-file-name', any of [`bash' `sh' `ksh' `zsh' `tclsh' `csh'
`tcsh']."
  (xorns-executable-find
    (getenv "SHELL")
    (xorns-get-value 'shell-file-name)
    (xorns-get-original-value 'shell-file-name)
    (getenv "ESHELL")
    "bash" "sh" "ksh" "zsh" "tclsh" "csh" "tcsh"))


(defun xorns-get-ansi-term-shell-name (&optional arg)
  "Get the shell name for a terminal\-emulator."
  (let*
    ((in-python (eq major-mode 'python-mode))
     (shell
       (cond
	 ((null arg) (if in-python 'Python 'System))
	 ((= (prefix-numeric-value arg) 3) 'Python-3)
	 ((if in-python 'System 'Python)))))
  shell))


(defun xorns-update-running-shells ()
  "Update information in `xorns-session-current-shells'."
  (dolist (item xorns-term-shells)
    (let* ((identifier (nth 0 item))
	   (command (nth 1 item))    ; cmd | (cmd args)
	   (name (nth 2 item))
	   (modes (nth 3 item))
	   (paste (nth 4 item)))      ; nil | template | function

      ; (bufferp buffer)
      ; (buffer-live-p buffer)
      ; (term-check-proc buffer)
      (get-buffer "*ansi-term*")
      )))


(defun xorns-ansi-term-get-buffer (arg)
  "Return the `ansi-term' buffer associated with the semantics of ARG.

ARG is related to the IDENTIFIER definition in `xorns-term-shells'.

One orphan shell (not configured in `xorns-term-shells') can be issued by:

- Issuing an integer argument not yet configured or used.

- By typing a plain `universal-argument' (\\[universal-argument]); in this
  case, the new identifier is the first not used and positive.

- By typing a plain `negative-argument' (\\[negative-argument]); in this case,
  the new identifier is the first not used and negative.

- Not using the prefix argument (nil); the last command shell is reused; the
  first time, the system shell (identifier = 0).

The parameters of orphan shells are deduced from the current `major-mode'."
  (let ((buffers (buffer-list frame)))
    (cond
      ((integerp arg))
      )
    ))


;;;###autoload
(defun xorns-ansi-term* (&optional arg)
  "Start a terminal\-emulator in a new buffer.

The selected shell command is launched and hosted in an `ansi-term' buffer.

See `xorns-ansi-term-get-buffer' function for details in the meaning of ARG
parameter.


Return the buffer hosting the shell."
  (interactive "P")
  (let* ()
    ))


;;;###autoload
(defun xorns-ansi-term (&optional arg)
  "Start a terminal\-emulator in a new buffer.

The selected shell command is launched and hosted in an `ansi-term' buffer.

The meaning of ARG is the same as the identifier in the custom variable
`xorns-term-shells'.

When an unregistered value is issued, command shell data will be asked; a
new (not yet used) identifier will be generated with a plain
`universal-argument' \\[universal-argument] (or `negative-argument'
\\[negative-argument]) with no numeric argument; default values will be taken
from the current buffer context.

Return the buffer hosting the shell."
  (interactive "P")
  (let*
    ((shell (xorns-get-ansi-term-shell-name arg))
     (cmd
       (cond
	 ((eq shell 'System)
	   (xorns-system-shell))
	 ((eq shell 'Python)
	   (xorns-python-shell))
	 (;else
	   (xorns-python3-shell))))
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
    (if cmd
      (progn
	(message ">>> Opening: %s" starred)
	(ansi-term cmd buf-name))
      ;else
      cur-buf)))


;; (defsubst ibuffer-get-region-and-prefix ()
;;   (let ((arg (prefix-numeric-value current-prefix-arg)))
;;     (if (use-region-p) (list (region-beginning) (region-end) arg)
;;       (list nil nil arg))))




;;;###autoload
(defun xorns-toggle-term-mode ()
  "Toggle term-mode between \"term-line-mode\" and \"term-char-mode\"."
  (interactive)
  (if (term-in-char-mode)
    (term-line-mode)
    ;else
    (term-char-mode)))


(global-set-key (kbd "C-c t") 'xorns-ansi-term)

(add-hook 'term-mode-hook
  (lambda ()
    (condition-case err
      (progn
	(define-key term-mode-map (kbd "C-c C-t") 'xorns-toggle-term-mode)
	(define-key term-raw-map (kbd "C-c C-t") 'xorns-toggle-term-mode)

        )
      (error (message "error@term-mode-hook: %s" err)))))


(provide 'xorns-term)
;;; xorns-term.el ends here
