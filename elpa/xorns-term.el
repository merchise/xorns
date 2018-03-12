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

;; xorns-term is our interface to the `ansi-term' (general command
;; interpreter).  Shell categories are configured in `xorns-term-shells'; the
;; command `xorns-ansi-term' is used to launch (or select) a
;; terminal\-emulator; `xorns-ansi-term-paste' paste selected content into one
;; using `comint-mode'.  Define user key\-bindings configured in
;; `xorns-term-launch-keys' to launch the terminal shells; in
;; `xorns-term-paste-keys' to paste content into a shell; and in
;; `xorns-term-toggle-mode-key' to to toggle `ansi-term' mode between
;; `term-line-mode' and `term-char-mode'.
;;
;; This module is automatically used when::
;;
;;     (require 'xorns)

;; Enjoy!


;;; Code:

(require 'term nil 'noerror)
(require 'xorns-utils nil 'noerror)



;;; Remove in future version


(defun xorns-get-ansi-term-shell-name (&optional arg)
  "Get the shell name for a `ansi-term' (based in ARG)."
  (let*
    ((in-python (eq major-mode 'python-mode))
     (shell
       (cond
	 ((null arg) (if in-python 'Python 'System))
	 ((= (prefix-numeric-value arg) 3) 'Python-3)
	 ((if in-python 'System 'Python)))))
  shell))


;; (bufferp buffer)
;; (buffer-live-p buffer)
;; (term-check-proc buffer)
;; (get-buffer "*ansi-term*")

;

;;; Misc

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


;;; Ansi Terminal


(defgroup xorns-term nil
   "Running `xorns' shell from within Emacs buffers."
   :prefix "xorns-term-"
   :group 'xorns
   :group 'shell)


(defcustom xorns-term-shells nil
  "Shell definition list to be managed by terminal shells.

The value is an association list `((IDENTIFIER . DEFINITION) ...)'.

IDENTIFIER is an integer, it's used to select the shell kind (for example,
with a prefix argument).  `0' is reserved for system shell; we advice you use
each value in levels of priorities, for example `1' for your main programmer
language shell, and `2' for your working project (like 'xoeuf' in Merchise).

DEFINITION has the following components:

- The COMMAND to execute when creating a new terminal shell, for example
  `/bin/bash'.

- A NAME definition, used to format the buffer name, and to identify the shell
  kind with a human readable symbol.  nil: the default name will be used; a
  symbol (recommended): the template '*<ID> - <SYMBOL>*' will be used for the
  buffer name, and '<SYMBOL>-mode' will be added to the mode mappings; string:
  the buffer name will be calculated using `xorns-format' function, `{id}' and
  `{cmd}' could be used for the identifier and for the command respectively.

- PASTE method, specify how to process the content to be sent to the shell
  process.  nil: the content will be used literally (standard); a string
  containing '%s': the content will be formatted using that value; any
  other string: will yank the content to the clipboard and then send the given
  value to the shell process (useful in shells like 'IPython' using '%paste'
  magic macro); a function: the content will be processed in a custom defined
  way, and the sent the result to the process.

- A list of MAJOR\-MODES to select a preferred shell by default.

Identifiers are not checked to be unique, this user responsibility."
  :type '(repeat
	   (cons
	     (integer :tag "Identifier")
	     (list :tag "Definition"
	       (choice :tag "Command"
		 (string :tag "Executable")
		 (cons :tag "With Arguments"
		   (string :tag "Executable")
		   (string :tag "Arguments")))
	       (choice :tag "Name"
		 (const :tag "Standard" nil)
		 (string :tag "Template")
		 (symbol :tag "Symbol"))
	       (choice :tag "Paste"
		 (const :tag "Standard" nil)
		 (string :tag "Template")
		 (function :tag "Custom"))
	       (repeat :tag "Major modes" symbol))))
  :group 'xorns-term)


(defcustom xorns-term-launch-keys (list (kbd "C-c t"))
  "A list of key\-bindings to launch terminal shells."
  :type '(repeat  key-sequence)
  :group 'xorns-term)


(defcustom xorns-term-paste-keys (list (kbd "C-c C-t"))
  "A list of key\-bindings to paste content into terminal shells."
  :type '(repeat key-sequence)
  :group 'xorns-term)


(defcustom xorns-term-toggle-mode-key (kbd "C-c C-t")
"A key\-binding to toggle between `term-line-mode' and `term-char-mode'.

This could be the same as the main definition for `xorns-term-paste-keys'."
  :type 'key-sequence
  :group 'xorns-term)


(defvar xorns-term-shell-identifier nil
  "Selected `xorns-term-shells' IDENTIFIER any `ansi-term' buffer.

This variable is defined local in each buffer.")


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


(defvar xorns-term-mode-shell-mapping nil
  "Cache variable for `xorns-term-mode-shell-mapping'.")


(defun xorns-term-mode-shell-mapping ()
  "Mapping between major modes and preferred shells.

Manage which `ansi-term' kind to favor in each `major-mode'.  Return an
association list `((MAJOR-MODE . SHELL-IDENTIFIER) ...)', and it is calculated
from the field 'Major modes' in `xorns-term-shells'."
  (or xorns-term-mode-shell-mapping
    (let (res)
      (dolist (shell xorns-term-shells)
	(let ((id (nth 0 shell))
	       (modes (nth 4 shell)))
	  (dolist (mode modes)
	    (let ((pair (assq mode res)))
	      (if (null pair)
		(add-to-list 'res (cons mode id) 'append)
		;else
		(message
		  "Error: major mode '%s' repeated shells (%s, %s)"
		  mode id (cdr pair)))))))
      (setq xorns-term-mode-shell-mapping res))))


(defun xorns-ansi-term-get-by-mode ()
  "Obtain the registered shell that fits the current `major-mode'.

If none fits, the system shell (`0') is returned."
  (let* ((shell-id (alist-get major-mode (xorns-term-mode-shell-mapping)))
	 (shell (assq shell-id xorns-term-shells)))
    (or shell (list 0 (xorns-system-shell) "Default Shell" nil nil)))
  )


(defun xorns-ansi-term-get-buffer (arg)
  "Return the `ansi-term' buffer or the shell information to launch one.

See `xorns-ansi-term' command for more information about the ARG parameter
semantics."
  (let ((buffers (buffer-list)))
    (cond
      ((null arg)
	(xorns-ansi-term-get-by-mode))
      ((integerp arg)
	)
      ((listp arg)
	)
      ((symbolp arg)    ; -
	)
      )
    ))


;;;###autoload
(defun xorns-ansi-term* (&optional arg)
  "Start or reuse a terminal\-emulator shell in a buffer.

ARG is either related to the IDENTIFIER definition in `xorns-term-shells', or
to some special functions when launching or reusing the shell.  The prefix ARG
could be:

- Not used (nil); a default shell is launched depending on the current
  `major-mode' (resulting in the system shell when no one is found).

- Any integer; either the configured shell with that
  IDENTIFIER, or an orphan shell, is launched.  The parameters for an orphan
  shell are deduced from the current `major-mode', or the last shell
  launched (the system shell by default).

- A plain `universal-argument' (\\[universal-argument]) launches an orphan
  shell asking the user for the parameters.

- A plain `negative-argument' (\\[negative-argument]) selects a registered
  shell to lauch it.

- Repeated plain `universal-argument' (\\[universal-argument]) selects a live
  shell buffer.

For numeric arguments (integers), you must use any combination of
`digit-argument' (never use plain `universal-argument', nor plain
`universal-argument', that are interpreted for different semantics in this
command)."
  (interactive "P")
  (let* ()
    ))


;;;###autoload
(defun xorns-ansi-term (&optional arg)
  "Start a terminal\-emulator in a new buffer (based in ARG)."
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


;;;###autoload
(defun xorns-ansi-term-paste (&optional arg)
  "Paste content into a `ansi-term' shell (based in ARG)."
  (interactive "P")
  (message ">>>> PASTE-ARG= %s" arg)
  )


;; (defsubst ibuffer-get-region-and-prefix ()
;;   (let ((arg (prefix-numeric-value current-prefix-arg)))
;;     (if (use-region-p) (list (region-beginning) (region-end) arg)
;;       (list nil nil arg))))




;;;###autoload
(defun xorns-toggle-term-mode ()
  "Toggle term-mode between `term-line-mode' and `term-char-mode'."
  (interactive)
  (if (term-in-char-mode)
    (term-line-mode)
    ;;else
    (term-char-mode)))


(dolist (key xorns-term-launch-keys)
  (global-set-key key 'xorns-ansi-term))
(dolist (key xorns-term-paste-keys)
  (global-set-key key 'xorns-ansi-term-paste))


(if xorns-term-toggle-mode-key
  (add-hook 'term-mode-hook
    (lambda ()
      (condition-case err
	(progn
	  (define-key term-mode-map
	    xorns-term-toggle-mode-key 'xorns-toggle-term-mode)
	  (define-key term-raw-map
	    xorns-term-toggle-mode-key 'xorns-toggle-term-mode))
	;; handlers
	(error (message "error@term-mode-hook: %s" err))))))


(provide 'xorns-term)
;;; xorns-term.el ends here
