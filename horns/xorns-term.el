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
(require 'xorns-tools)
(require 'xorns-simple)
(require 'xorns-keywords)


;;; Common setup

(defsubst >>-getenvshell (variable &optional format)
  "Get the value of environment VARIABLE using a FORMAT string for `getenv'.
Resulting value will be checked with `executable-find'"
  (executable-find
    (or
      (getenv (format (or format "%sSHELL") (upcase variable)))
      variable)))


(defun >>-default-shell-file-name ()
  "Get the system default-shell-file-name."
  (or
    explicit-shell-file-name
    shell-file-name
    (>>-getenvshell "ESHELL" "%s")
    (>>-getenvshell "SHELL" "%s")))


(defun >>-shell-file-name (&rest options)
  "Calculate a file name valid to load as inferior shell in a terminal.

The result is the first item in OPTIONS that can be validated with `getenv' or
with `executable-find'."
  (if (null options)
    (>>-default-shell-file-name)
    ;; else
    (setq options (delq nil (>>=cast-list options)))
    (let (res)
      (while (and options (not res))
	(let ((id (car options)))
	  (setq options (cdr options))
	  (if (eq id 'ansi)
	    (setq res (>>-default-shell-file-name))
	    ;; else
	    (if (symbolp id)
	      (setq id (symbol-name id)))
	    (setq res (>>-getenvshell id)))))
      res)))


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

(defconst >>-!trigger/docstring-format
  "Command to trigger '%s' terminals.
See `>>=define-terminal-trigger' for more information."
  "Documentation format-string to be used with a trigger ID as argument.")


(defun >>-trigger/adjust-argument (prefix)
  "Adjust a trigger PREFIX argument.

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


(defun >>-trigger/adjust-string (string)
  "Adjust a STRING to paste it into a terminal."
  (when string
    (let ((res (string-trim-right string)))
      (unless (string-empty-p res)
	(concat res "\n")))))


(defun >>-trigger/paste-get (&optional adapter)
  "Get current buffer focused-text and adjust it with the given ADAPTER."
  (let ((res (>>-trigger/adjust-string (>>=buffer-focused-text))))
    (if (and res adapter) (funcall adapter res) res)))


(defmacro >>=trigger/define-paste-magic (&optional magic)
  "Create an IPython like MAGIC paste-adapter."
  (if (null magic)
    (setq magic "%paste"))
  `(lambda (chars)
     (kill-new chars)
     (>>-trigger/adjust-string ,magic)))


(defmacro >>=define-terminal-trigger (id &optional docstring &rest keywords)
  "Define a new trigger to manage `ansi-term' based terminals.

A trigger is an `interactive' command to manage shell terminals in three
different scenarios: create, reuse, or paste.

Argument ID is used to form the command name as '>>=<ID>-term', the prefix for
buffer names, and to calculate the value of :PROGRAM if this keyword is
missing.

DOCSTRING is the trigger documentation, it may contain a format string
placeholder '%s' which will be replaced by the ID value.

The following KEYWORDS are meaningful:

:program STRING
    The file-name to be loaded as inferior shell.  When omitted, it is
    calculated with `>>-shell-file-name' using ID as its argument.

:paste-adapter FUNCTION

    When a paste operation is invoked, the text is wrapped with this function
    before calling `term-send-raw-string'.  The macro
    `>>=trigger/define-paste-magic' can be used to create adapters using the
    IPython magic way.

The optional `interactive' prefix argument of a trigger is used to identify
both the shell session tab and whether to execute a paste operation.  See
`>>-trigger/adjust-argument' to understand this logic."
  (declare (doc-string 2) (indent 1) (debug t))
  (unless (symbolp id)
    (error ">>= terminal-trigger ID must be a symbol, not %s" (type-of id)))
  (when (and docstring (not (stringp docstring)))
    (setq
      keywords (cons docstring keywords)
      docstring nil))
  (setq docstring (format (or docstring >>-!trigger/docstring-format) id))
  (setq keywords
    (>>=normalize-alist '>>-trigger
      (append `((id . ,id) (docstring . ,docstring))
	(apply '>>=keywords->alist keywords))
      ;; defaults
      :program '(:eval id)
      ))
  (let* ((program (>>=kw-get :program keywords))
	 (paste-adapter (>>=kw-get :paste-adapter keywords))
	 (fun-name (intern (format ">>=%s-term" id)))
	 (bn-prefix (if (eq id 'ansi) "" (format "%s-" id))))
    (setq program (>>-shell-file-name (or program id)))
    `(defun ,fun-name (&optional arg)
       ,docstring
       (interactive "P")
       (setq arg (>>-trigger/adjust-argument arg))
       (let* ((command ,program)
	      (tab-index (car arg))
	      (paste (cdr arg))
	      (bn-suffix (if tab-index (format " - %s" tab-index) ""))
	      (buf-name (format "%sterminal%s" ,bn-prefix bn-suffix))
	      (starred (format "*%s*" buf-name))
	      (buffer (get-buffer starred))
	      (process (get-buffer-process buffer)))
	 (when paste
	   (setq paste (>>-trigger/paste-get ,paste-adapter)))
	 (if buffer
	   (if process
	     (progn
	       (setq command nil)
	       (switch-to-buffer buffer))
	     ;; else
	     (message ">>= killing '%s' terminal, process was finished."
	       starred)
	     (kill-buffer buffer)))
	 (when command
	   (setq buffer (ansi-term command buf-name)))
	 (when paste
	   (message ">>= term-send-raw-string %s" paste)
	   (term-send-raw-string paste))
	 buffer))
    ))



(>>=define-terminal-trigger ansi)        ; define `>>=ansi-term'

;; (>>=define-terminal-trigger python
;;   :program "ipython"
;;   :paste-adapter (>>=trigger/define-paste-magic)
;;   )


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
