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

(defun >>-normalize-trigger-argument (prefix)
  "Normalize a trigger PREFIX argument.

Convert a trigger PREFIX argument to a (TAB-INDEX . PASTE) pair.  The first
value will be an integer greater than or equal to zero, or nil for the default
tab.  The second value will be a boolean specifying if the selected text in
the current buffer must be pasted to that terminal shell.

The `universal-argument' \(or `C-u') without any further digits, means paste
to the default tab, identified with nil.  The `negative-argument' \(or `C--')
without any further digits, means paste to tab with index 0."
  (cond
    ((null prefix)
      nil)
    ((consp prefix)
      '(nil . t))
    ((integerp prefix)
      (cons (abs prefix) (< prefix 0)))
    (t
      '(0 . t))))


(defmacro >>=define-terminal-trigger (&optional name program)
  "Define a new trigger to manage an `ansi-term' based terminal.

Arguments of this macro can be used as follow:

If both are missing, the default trigger `>>=ansi-term' is created.

NAME could be a symbol or a string, it will identify the trigger name using
'>>=<NAME>-term' as format, and it will be used as a prefix in each buffer
name.

When given, PROGRAM must be a string specifying the file-name to be loaded as
inferior shell for this trigger.  If omitted, this value is calculated with
the function `>>-shell-file-name' using NAME as its argument.

A trigger is an `interactive' command that could be called in three different
scenarios: to start a new shell session, to reuse an existing one, or to yank
the selected region in the current buffer to a designated shell.

The prefix argument in a trigger is used to identify the shell session tab.  A
TAB-INDEX is a positive integer, or zero for the default session.  A negative
value, or the `universal-argument' (`C-u') prefix, executes a yank scenario in
the `abs' value TAB."
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
      doc (format "Command to trigger '%s' terminals." id))
    (if (or (null program) (stringp program))
      (setq shell (or program (>>-shell-file-name name)))
      ;; else
      (error ">>= terminal-trigger program must be a string, not %s"
	(type-of program)))
    `(progn
       (defun ,fun-name (&optional arg)
	 ,doc
	 (interactive "P")
	 (let* ((command ,shell)
		(prefix (>>-normalize-trigger-argument arg))
		(tab-index (car prefix))
		(yank (cdr prefix))
		(bn-suffix (if tab-index (format " - %s" tab-index) ""))
		(buf-name (format "%sterminal%s" ,bn-prefix bn-suffix))
		(starred (format "*%s*" buf-name))
		(buffer (get-buffer starred))
		(process (get-buffer-process buffer)))
	   (message "%s(%s %s)" (symbol-name ',fun-name) tab-index yank)
	   (if buffer
	     (if process
	       (progn
		 (setq command nil)
		 (switch-to-buffer buffer))
	       ;; else
	       (message ">>= killing '%s' terminal, process was finished."
		 starred)
	       (kill-buffer buffer)))
	   (if command
	     (ansi-term command buf-name)
	     ;; else
	     buffer))))
    ))


(>>=define-terminal-trigger)        ; define `>>=ansi-term'


(use-package term
  :preface
  (defun >>-term/raw-kill-line ()
    "Kill the rest of the current line in `term-char-mode'."
    (interactive)
    (term-send-raw-string "\C-k")
    (kill-line))
  :bind
  (("C-c t" . ansi-term)    ;; TODO: migrate to `>>=ansi-term'
   ("s-M-t" . >>=ansi-term)
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
