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


;;; Common setup

(defconst >>=!default-shell-file-name
  (purecopy
    (or
      explicit-shell-file-name
      shell-file-name
      (>>=executable-find (getenv "ESHELL") (getenv "SHELL") "bash" "sh")))
  "System default shell file-name.")


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
	  [remap eshell-list-history] 'helm-eshell-history))))
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

(defconst >>-!term/docstring-format
  "Command to manage '%s' terminals.
See `>>=define-terminal' for more information."
  "Documentation format-string to be used with a terminal ID as argument.")


(defun >>-term/adjust-argument (prefix)
  "Adjust a terminal PREFIX argument.

Convert a terminal command PREFIX argument to a (TAB-INDEX . PASTE) pair.  The
first value will be an integer greater than or equal to zero, or nil for the
default tab.  The second value will be a boolean specifying if the selected
text in the current buffer must be pasted to that terminal shell.

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


(defun >>-term/adjust-string (string)
  "Adjust a STRING to paste it into a terminal."
  (let ((res (and string (string-trim-right string))))
    (if (and res (not (string-empty-p res)))
      res)))


(defun >>-term/paste-get ()
  "Get current buffer focused-text and adjust it for a terminal shell."
  (>>-term/adjust-string (>>=buffer-focused-text)))


(defun >>-term/paste-send (text)
  "Send TEXT to a selected terminal shell session."
  (term-send-raw-string (concat text "\n")))


(defun >>=term/define-paste-magic (&optional magic)
  "Create a ':paste-send' adapter like IPython's MAGIC '%paste' command."
  (if (null magic)
    (setq magic "%paste"))
  (lambda (text)
    (when text
      (if (string-match-p "\n" text)
	(progn
	  (kill-new text)
	  (>>-term/paste-send magic)
	  (current-kill 1))
	;; else
	(>>-term/paste-send text)))))


(defun >>-term-normalize/:program (value &optional keywords)
  "Adjust VALUE for ':program' using KEYWORDS environment."
  (cond
    ((eq value 'ansi)
      (plist-put keywords :program-id "ansi")
      >>=!default-shell-file-name)
    ((symbolp value)
      (>>-term-normalize/:program (symbol-name value) keywords))
    (t    ; string, or sequence of options
      (let ((pair (>>=find-env-executable "%sSHELL" value)))
	(if pair
	  (progn
	    (plist-put keywords :program-id (car pair))
	    (cdr pair))
	  ;; else
	  (error ">>= invalid ':program' value '%s'" value))))))


(defun >>-term-normalize/:paste-get (value &optional _keywords)
  "Adjust VALUE for keyword ':paste-get'."
  (>>=cast-function value 'validate))


(defun >>-term-normalize/:paste-send (value &optional keywords)
  "Adjust ':paste-send' VALUE using KEYWORDS environment."
  (if (stringp value)
    (>>-term-normalize/:paste-send
      (>>=term/define-paste-magic value) keywords)
    ;; else
    (or
      (>>=cast-function value)
      (if (consp value)
	(let* ((options (>>=cast-list value))
	       (program (plist-get keywords :program-id))
	       (res (assoc program options 'string-equal)))
	  (if res
	    (>>-term-normalize/:paste-send (cdr res))
	    #'>>-term/paste-send))
	;; else
	(error ">>= invalid ':paste-send' value '%s'" :paste-send)))))


(defmacro >>=define-terminal (id &optional docstring &rest keywords)
  "Define a command to manage `ansi-term' based terminals.

This macro defines a command to manage shell terminals in two scenarios:
trigger/reuse a shell session, or paste.

ID must be a symbol, it will be used to form the command NAME using
'>>=<ID>-term' format, and for default values of KEYWORDS :program and
:buffer-name.

DOCSTRING is the terminal command documentation.

The following KEYWORDS are meaningful:

:program
	The file-name to be loaded as inferior shell.  The value must be a
	string or a list of choices to find the first valid value.  It
	defaults to the `symbol-name' result of ID.

:program-id
	Virtual value, must not be given as a formal argument.  It storage the
	original value of ':program'.

:buffer-name
	A string prefix for buffer names, the suffix will be the TAB-INDEX,
	see below.  It defaults to 'terminal' if ID is 'ansi', or '<ID>-term'
	otherwise.

:paste-get
	A function to get the focused-text in the current buffer when a paste
	operation is invoked.  It defaults to `>>-term/paste-get'.

:paste-send
	A function to send selected text into a terminal shell when a paste
	operation is invoked.  If a string is given, it is converted to a
	function using `>>=term/define-paste-magic', if a `cons' or an
	assotiation-list is given, ':program-id' will be searched in a `car'
	value, the `cdr' value will be the definitive value.  It defaults to
	`>>-term/paste-send'.

Given KEYWORDS are normalized using `>>=plist-normalize', the CLASS will be
'>>-term', and the NAME will be used but replacing '>>=' prefix for '>>-'.

The defined terminal command takes one optional prefix argument.  It is used
to identify both the terminal shell session TAB-INDEX and whether to execute a
paste operation.

The command prefix argument is converted to a \(TAB-INDEX . PASTE) pair using
the function `>>-term/adjust-argument'.  TAB-INDEX is nil, for the default
tab, or any integer greater than or equal to zero.  PASTE specifies if the
selected text in the current buffer must be pasted to the corresponding
terminal shell.

The `universal-argument', or `C-u' without any further digits, means paste to
the default tab, identified with nil.  The `negative-argument' or `C--'
without any further digits, means paste to tab with index 0."
  (declare (doc-string 2) (indent 1) (debug t))
  (unless (symbolp id)
    (error ">>= terminal ID must be a symbol, not %s" (type-of id)))
  (when (and docstring (not (stringp docstring)))
    (setq
      keywords (cons docstring keywords)
      docstring nil))
  (when (null docstring)
    (setq docstring (format >>-!term/docstring-format id)))
  (setq keywords
    (>>=plist-normalize '>>-term (format ">>-%s-term" id) keywords
      ;; defaults
      :id id
      :function-name (intern (format ">>=%s-term" id))
      :buffer-name (if (eq id 'ansi) "terminal" (format "%s-term" id))
      :program id
      :paste-get #'>>-term/paste-get
      :paste-send #'>>-term/paste-send))
  `(defun ,(plist-get keywords :function-name) (&optional arg)
     ,docstring
     (interactive "P")
     (setq arg (>>-term/adjust-argument arg))
     (let* ((command ,(plist-get keywords :program))
	    (paste-get ',(plist-get keywords :paste-get))
	    (paste-send ',(plist-get keywords :paste-send))
	    (tab-index (car arg))
	    (paste (cdr arg))
	    (buffer-name ',(plist-get keywords :buffer-name))
	    (buf-name-index (if tab-index (format " - %s" tab-index) ""))
	    (buf-name (concat buffer-name buf-name-index))
	    (starred (format "*%s*" buf-name))
	    (buffer (get-buffer starred))
	    (process (get-buffer-process buffer)))
       (when paste
	 (setq paste (funcall paste-get)))
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
	 (funcall paste-send paste))
       buffer)))


(>>=define-terminal ansi)        ; define `>>=ansi-term'


;; (>>=define-terminal python
;;   :program "ipython" "python"
;;   :paste-send ("ipython" . "%paste")
;;   :mode python
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
