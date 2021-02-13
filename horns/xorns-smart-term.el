;;; xorns-smart-term.el --- Building-block for smart terminals  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;;; Commentary:

;; A new smart terminal can be defined with the macro `>>=define-terminal'
;; using several optional keyword arguments: the command to execute as a
;; shell, a function to paste text between ordinary buffers and terminals, a
;; list of `major-mode' symbols to associate a terminal kind with buffers
;; under these modes.  Example of terminals are `>>=main-term' (the default
;; terminal), and `>>=python-term' (for Python modes).

;; The function `>>=xterminal' orchestrates all terminal kinds based on their
;; associations with major modes.  Each terminal kind can trigger several
;; tabs, each one is identified with a zero (the default) or positive integer.
;; To select a terminal tab a prefix argument is used.  A negative value is
;; used to execute a paste operation from the current buffer to a target
;; terminal (using the absolute value).  The `universal-argument' (`C-u') is
;; used to paste to the default tab, and `negative-argument' (`C--') for the
;; tab with index zero.

;; Every time a terminal is triggered, a reference to the current buffer is
;; linked to that tab, executing a command to select an active tab will
;; switch, or paste, to the linked buffer.

;; Enjoy!


;;; Code:

(require 'term)
(require 'xorns-tools)


;;; Variables

(defvar >>=|default-shell-file-name
  (purecopy
    (or
      explicit-shell-file-name
      shell-file-name
      (>>=executable-find (getenv "ESHELL") (getenv "SHELL") "bash" "zsh")))
  "System default shell file-name.")


(defvar >>-xterm/state nil
  "Terminal state (local variable in terminal buffers).")


(defvar >>-xterm/linked nil
  "Linked tab-index (local variable in buffers that trigger a terminal).")


(defvar >>-xterm-modes nil
  "Association-list mapping major modes to smart terminals.")



;;; Keyword value checkers

(defun >>-xterm/check-paster (paster)
  "Check a PASTER definition and convert it to a valid function."
  (or
    (when (stringp paster)
      (>>=xterm/define-paste-magic paster))
    (>>=cast-function paster)
    (if (and (symbolp paster) (not (booleanp paster)))
      paster
      ;; else
      (error ">>= wrong :paster definition '%s'" paster))))


(defun >>-xterm/fix-keywords (keywords)
  "Fix raw terminal KEYWORDS parameters."
  (>>=map-pair
    (lambda (key value)
      (pcase key
	(:program
	  (mapcar
	    (lambda (item)
	      (or
		(when (consp item)
		  (cons
		    (>>=str (car item) :program)
		    (>>-xterm/check-paster (cdr item))))
		(>>=str item :program)))
	    (>>=cast-list value)))
	(:paster
	  (>>-xterm/check-paster value))
	(:buffer-name
	  (>>=str value :buffer-name))
	(:mode
	  (mapcar (lambda (mode) (>>=str mode :mode)) (>>=cast-list value)))
	(_
	  (error ">>= unexpected keyword '%s' with value '%s'" key value))))
    (>>=plist-fix keywords)))


(defsubst >>-xterm/default-value (term key)
  "Get TERM default KEY value."
  (pcase key
    (:program >>=|default-shell-file-name)
    (:paster '>>-xterm/paster)
    (:buffer-name (replace-regexp-in-string "^>>=" "" (symbol-name term)))))


(defun >>-xterm/key (term key)
  "Get a KEY value for a given TERM."
  (or
    (get term key)
    (when-let ((keywords (get term :keywords)))
      (if (memq key '(:program :paster))
	(let ((program (>>=executable-find (plist-get keywords :program)))
	      paster res)
	  (when (consp program)
	    (setq
	      paster (cdr program)
	      program (car program)))
	  (unless paster
	    (setq paster (plist-get keywords :paster)))
	  (when program
	    (put term :program program)
	    (when (eq key :program)
	      (setq res program)))
	  (when paster
	    (put term :paster paster)
	    (when (eq key :paster)
	      (setq res paster)))
	  res)
	;; else
	(when-let ((res (plist-get keywords key)))
	  (put term key res)
	  res)))
    (>>-xterm/default-value term key)
    (error ">>= unexpected term key '%s'" key)))


(defsubst >>-xterm/buffer-name (term &optional tab-index)
  "Get a buffer name for a given TERM and and TAB-INDEX."
  (concat
    ;; prefix
    (>>-xterm/key term :buffer-name)
    ;; suffix
    (cond
      ((or (null tab-index) (zerop tab-index))
	"")
      ((> tab-index 0)
	(format " - %s" tab-index))
      (t
	(error ">>= invalid tab-index: %s" tab-index)))))


(defun >>-xterm/get-buffer (buffer-name)
  "Get the terminal buffer for a given BUFFER-NAME.
If a buffer is found that does not have a live process associated, it is
killed and nil is returned."
  (let ((target (get-buffer (format "*%s*" buffer-name))))
    (when target
      (if (get-buffer-process target)
	target
	;; else
	(kill-buffer target)
	nil))))


(defun >>-xterm/get-or-create-buffer (term tab-index)
  "Get or create a TERM buffer for a given TAB-INDEX."
  (let* ((buffer-name (>>-xterm/buffer-name term tab-index))
	 (target (>>-xterm/get-buffer buffer-name)))
    (unless target
      (let ((command (>>-xterm/key term :program)))
	(save-window-excursion
	  (with-current-buffer
	    (setq target (ansi-term command buffer-name))
	    (set (make-local-variable '>>-xterm/state) `(:term ,term))))))
  target))


(defun >>-xterm/get-alt-buffer (term)
  "Get alternative buffer for a TERM."
  (let ((file-name (plist-get >>-xterm/state :file-name)))
    (if (and file-name (file-exists-p file-name))
      (find-file-noselect file-name)
      ;; else
      (or
	(>>=find-buffer
	  :mode (car (rassq term >>-xterm-modes)))
	(>>=find-buffer
	  :mode (plist-get >>-xterm/state :mode))
	(>>=scratch/get-buffer-create)))))


(defun >>-xterm/get-default-term ()
  "Get the default term for the current buffer."
  (if >>-xterm/state    ; called while already in a terminal
    (or
      (plist-get >>-xterm/state :term)
      (error ">>= :term was not found in `>>-xterm/state': %s" >>-xterm/state))
    ;; else
    (or
      (cdr (assq major-mode >>-xterm-modes))
      '>>=main-term)))


(defsubst >>-xterm/get-default-tab-index ()
  "Get the default tab-index for the current buffer."
  (if >>-xterm/state
    (plist-get >>-xterm/state :tab-index)
    ;; else
    (or >>-xterm/linked 0)))


(defun >>-xterm/search-new (term &optional tab-index)
  "Get a new TAB-INDEX for the given TERM."
  (setq tab-index (or tab-index 0))
  (let (res)
    (while (not res)
      (if (>>-xterm/get-buffer (>>-xterm/buffer-name term tab-index))
	(setq tab-index (1+ tab-index))
	;; else
	(setq res tab-index)))
    res))


(defun >>=xterminal (term &optional prefix add-new)
  "Trigger a smart terminal.

TERM must be a command generated by `>>=define-terminal', or nil to determine
the value from the `major-mode' of the current buffer or from a current
running terminal.

PREFIX argument encapsulates two concepts in a single value: tab-index, and
whether to invoke a paste operation.  An integer PREFIX value sets the
tab-index to its absolute value, a paste operation is issued with negative
values.  The zero paste counterpart is a plain `C--', or `negative-argument'.

A default value to the tab-index is represented by a nil PREFIX.  This is the
last used to trigger a terminal from the current buffer, or zero if none have
been saved yet.  The counterpart paste operation for this is issued by a
`cons' value generated by a plain `C-u', or `universal-argument'.

ADD-NEW is used internally from `>>=add-new-terminal' to create a new instance
using the first available tab-index.  In this case PREFIX is used as the
minimum index to search."
  (interactive "i\nP")
  (unless term
    (setq term (>>-xterm/get-default-term)))
  (let (tab-index paste
	(paster (>>-xterm/key term :paster))
	default
	(source (current-buffer))
	target)
    (when prefix
      (cond
	((consp prefix)
	  (setq paste t))
	((integerp prefix)
	  (setq
	    tab-index (abs prefix)
	    paste (< prefix 0)))
	(t    ; `negative-argument'
	  (setq
	    tab-index 0
	    paste t))))
    (when add-new
      (setq tab-index (>>-xterm/search-new term tab-index)))
    (unless tab-index
      (setq
	default t
	tab-index (>>-xterm/get-default-tab-index)))
    (when paste
      (setq paste (>>-xterm/get-paste-text)))
    (setq target (>>-xterm/get-or-create-buffer term tab-index))
    (when (eq target source)
      (setq
	target (plist-get >>-xterm/state :source)
	paster nil
	default t)
      (unless (buffer-live-p target)
	(setq target (>>-xterm/get-alt-buffer term))))
    (unless default
      (set (make-local-variable '>>-xterm/linked) tab-index))
    (switch-to-buffer-other-window target)
    (when >>-xterm/state
      (>>=plist-update >>-xterm/state
	:source source
	:file-name (buffer-file-name source)
	:term term
	:tab-index tab-index
	:mode (buffer-local-value 'major-mode source)))
    (when paste
      (unless paster
	(setq paster
	  (or
	    (get (plist-get >>-xterm/state :term) :paster)
	    (when (eq major-mode 'term-mode) '>>-xterm/paster)
	    (unless buffer-read-only 'insert))))
      (if paster
	(funcall paster paste)
	;; else
	(warn (concat ">>= no valid paster found, "
		"maybe target buffer is read only.\n"
		"	Paste text:\n%s") paste)))
    target))


(defun >>=add-new-terminal (term &optional prefix)
  "Call `>>=xterminal' using TERM, PREFIX as given, and t for 'ADD-NEW'."
  (interactive "i\nP")
  (funcall '>>=xterminal term prefix 'add-new))


(provide 'xorns-smart-term)
;;; xorns-smart-term.el ends here
