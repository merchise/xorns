;;; xorns-term.el --- Terminal support  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;;; Commentary:

;; xorns-term is our interface to the `ansi-term' (general command
;; interpreter).

;; Enjoy!


;;; Code:

(require 'use-package)
(require 'term)



;;; Common setup

(defun >>-shell-file-name (&optional base)
  "Calculate the file name to load as inferior shells for terminals.
Argument BASE is prioritized using `executable-find' function, and getting the
value of environment variable with the uppercase value.  If no value is found
for BASE, the default system shell is returned."
  (or
    (and
      base
      (let ((aux (format "%s" base)))
	(or
	  (executable-find aux)
	  (getenv (upcase aux)))))
    explicit-shell-file-name
    (getenv "ESHELL")
    (getenv "SHELL")
    shell-file-name))


(use-package eshell
  :commands (eshell eshell-command)
  :preface
  (progn
    (eval-when-compile
      (require 'em-term)
      (declare-function eshell-cmpl-initialize 'em-cmpl))

    (defun >>-eshell/first-time ()
      "Run the first time `eshell-mode' is entered.a"
      (add-to-list 'eshell-visual-commands "htop"))

    (defun >>-eshell/init ()
      "Initialize eshell."
      (eshell-cmpl-initialize)
      (define-key eshell-mode-map
	(kbd "C-c C-d") 'quit-window)
      (when (bound-and-true-p helm-mode)
	(require 'helm-eshell)
	(define-key eshell-mode-map
	  [remap eshell-pcomplete] 'helm-esh-pcomplete)
	(define-key eshell-mode-map
	  [remap eshell-list-history] 'helm-eshell-history)))
    )
  :custom
  (eshell-history-size 1024)
  (eshell-hist-ignoredups t)
  :hook
  (eshell-first-time-mode . >>-eshell/first-time)
  (eshell-mode . >>-eshell/init)
  :config
  (progn
    (require 'esh-io)
    (require 'em-alias)
    (eshell/alias "l" "ls -lh $1")
    (eshell/alias "ll" "ls -alhF $1")
    (eshell/alias "la" "ls -A $1")))


(use-package comint
  :ensure helm
  :preface
  (defun >>-comint/init ()
    "Initialize comint."
    (when (bound-and-true-p helm-mode)
      (require 'helm-eshell)
      (define-key comint-mode-map
	(kbd "C-c C-l") 'helm-comint-input-ring)
      (define-key comint-mode-map
	(kbd "M-s f") 'helm-comint-prompts-all)))
  :hook
  (comint-mode . >>-comint/init))



;;; ANSI Terminal

(defmacro >>=define-terminal-trigger (&optional name program)
  "Define NAME as a new trigger to manage an `ansi-term' based terminal.

The NAME of the resulting command will be '>>=<NAME>-term'; when the name is
omitted, it will be '>>=ansi-term'.

to manage multiple shell sessions,
equivalent to tabs in standard system terminals.  The

Each instance is identified
by a positive integer, or zero (the default value).

Calling a trigger will start a new shell session or reuse an existing one.
When prefixed with a negative value, the selected region (or current line if
none) will be yanked to the instance identified by the absolute value.  To
yank to the default session use the `universal-argument' (`C-u') prefix.

PROGRAM argument must be a string that specifies the file name to be loaded as
inferior shell.  When omitted, the value is calculated with the function
`>>-shell-file-name'."
  (let* (id shell fun-name bn-prefix doc)
    (if name
      (if (or (symbolp name) (stringp name))
	(setq
	  id name
	  bn-prefix (format "%s-" name))
	;; else
	(error ">>= terminal-trigger name must be a symbol or nil, not %s"
	  (type-of name)))
      ;; else
      (setq
	id 'ansi
	bn-prefix ""))
    (setq
      fun-name (intern (format ">>=%s-term" id))
      doc (format "Command to trigger '%s' terminal." id))
    (if (or (null program) (stringp program))
      (setq shell (or program (>>-shell-file-name name)))
      ;; else
      (error ">>= terminal-trigger program must be a string if given, not %s"
	(type-of program)))
    `(progn
       (defun ,fun-name (&optional arg)
	 ,doc
	 (interactive "P")
	 (let* ((command ,shell)
		(std (or (null arg) (eq arg 0)))
		(bn-suffix (if std "" (format " - %s" arg)))
		(buf-name (format "%sterminal%s" ,bn-prefix bn-suffix))
		(starred (format "*%s*" buf-name))
		(buffer (get-buffer starred))
		(process (get-buffer-process buffer)))
	   (if buffer
	     (if process
	       (progn
		 (setq command nil)
		 (switch-to-buffer buffer))
	       ;; else
	       (message ">>= killing '%s' because process was finished." starred)
	       (kill-buffer buffer)))
	   (if command
	     (ansi-term command buf-name)
	     ;; else
	     buffer))))
    ))


(>>=define-terminal-trigger)        ; define `>>=ansi-term'


(use-package term
  :preface
  (progn
    (declare-function term-send-raw-string 'term)

    (defun >>-term/raw-kill-line ()
      "Kill the rest of the current line in `term-char-mode'."
      (interactive)
      (term-send-raw-string "\C-k")
      (kill-line))

    (defun >>=term-main-shell ()
      "Command to execute ANSI terminal."
      (interactive)
      (let* ((command (>>-shell-file-name))
	      (buf-name "Terminal")
	      (starred (format "*%s*" buf-name))
	      (buffer (get-buffer starred))
	      (process (get-buffer-process buffer)))
	(if buffer
	  (if process
	    (progn
	      (setq command nil)
	      (switch-to-buffer buffer))
	    ;; else
	    (message ">>= killing '%s' because process was finished." starred)
	    (kill-buffer buffer)))
	(if command
	  (ansi-term command buf-name)
	  ;; else
	  buffer)))
    )
  :bind
  (("C-c t" . ansi-term)
   ("s-M-t" . >>=term-main-shell)
   (:map term-mode-map
     ("C-c C-t" . term-char-mode))
   (:map term-raw-map
     ("C-c C-t" . term-line-mode)
     ("C-y" . term-paste)
     ("C-k" . >>-term/raw-kill-line)))
  :custom
  (term-input-autoexpand t))


(provide 'xorns-term)
;;; xorns-term.el ends here
