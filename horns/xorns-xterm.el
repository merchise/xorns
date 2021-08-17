;;; xorns-xterm.el --- Building-block for smart terminals  -*- lexical-binding: t -*-

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

(defvar >>=|default-shell-file-name nil
  "Default shell file-name (see `>>-xterm/get-default-shell-file-name').")
(make-obsolete-variable
  '>>=|default-shell-file-name
  " It will no longer exists.  Customize `shell-file-name' instead."
  "0.9")


(defvar >>-xterm/state nil
  "Terminal state (local variable in terminal buffers).")


(defvar >>-xterm/linked nil
  "Linked tab-index (local variable in buffers that trigger a terminal).")


(defvar >>-xterm-modes nil
  "A mapping of '(major-mode . xterm)' pairs.")



;;; Utility functions

(defsubst >>-xterm/get-default-shell-file-name ()
  "Adjust a STRING to paste it into a terminal."
  (purecopy
    (or
      >>=|default-shell-file-name
      explicit-shell-file-name
      shell-file-name
      (>>=executable-find (getenv "ESHELL") (getenv "SHELL") "bash" "zsh"))))


(defun >>-xterm/adjust-string (string)
  "Adjust a STRING to paste it into a terminal."
  (let ((res (and string (string-trim-right string))))
    (if (and res (not (string-empty-p res)))
      res)))


(defun >>-xterm/get-paste-text ()
  "Get current buffer focused-text and adjust it for a terminal shell."
  (>>-xterm/adjust-string (>>=buffer-focused-text)))


(defun >>-xterm/paster (text)
  "Send TEXT to a selected terminal shell session."
  (term-send-raw-string (concat text "\n")))


(defun >>=xterm/define-paste-magic (&optional magic)
  "Create a ':paster' adapter like IPython's MAGIC '%paste' command."
  (if (null magic)
    (setq magic "%paste"))
  (lambda (text)
    (when text
      (if (string-match-p "\n" text)
        (progn
          (kill-new text)
          (>>-xterm/paster magic)
          (current-kill 1))
        ;; else
        (>>-xterm/paster text)))))


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


(defun >>-xterm/command-name (&optional id)
  "Create a command name for a terminal from the given ID.
If ID is a whole word, it is formated using '>>=<ID>-term'.  It defaults to
`>>=main-term'"
  (if id
    (if (string-match-p "^[[:alnum:]]+$" (symbol-name id))
      (intern (format ">>=%s-term" id))
      ;; else
      id)
    ;; else
    '>>=main-term))


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
    (:program (>>-xterm/get-default-shell-file-name))
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


(defsubst >>-xterm/check-buffer (buffer &optional term)
  "Check that BUFFER is a valid smart terminal.
When TERM is given, only check buffers of that kind."
  (when buffer
    (when-let ((state (buffer-local-value '>>-xterm/state buffer)))
      (if (get-buffer-process buffer)
        (when (or (null term) (eq term (plist-get state :term)))
          buffer)
        ;; else
        (>>=kill-buffer-and-window buffer)
        nil))))


(defun >>-xterm/get-buffer (buffer-name)
  "Get the terminal buffer for a given BUFFER-NAME.
If a buffer is found that does not have a live process associated, it is
killed and nil is returned."
  (let ((target (get-buffer (format "*%s*" buffer-name))))
    (when target
      (>>-xterm/check-buffer target))))


(defun >>-xterm/current-buffer ()
  "Return the `current-buffer'.
When it is a finished terminal, re-create it."
  (let ((res (current-buffer)))
    (when (and >>-xterm/state (not (get-buffer-process res)))
      (let* ((state >>-xterm/state)
             (term (plist-get state :term))
             (command (>>-xterm/key term :program))
             (tab-index (plist-get state :tab-index))
             (buffer-name (>>-xterm/buffer-name term tab-index)))
        (kill-buffer res)
        (save-window-excursion
          (with-current-buffer
            (setq res (ansi-term command buffer-name))
            (set (make-local-variable '>>-xterm/state) state))))
      (switch-to-buffer res nil 'force-same-window))
    res))


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


(defsubst >>-xterm/switch-to-buffer (target)
  "Select the smart terminal TARGET buffer."
  (switch-to-buffer-other-window target))


(defun >>-xterm/select (&optional term)
  "Return an assotiation-list of all valid buffers for the given TERM kind."
  (let ((source (current-buffer)))
    (nreverse
      (delq nil
        (mapcar
          (lambda (buffer)
            (unless (eq source buffer)
              (when (>>-xterm/check-buffer buffer term)
                (cons (buffer-name buffer) buffer))))
          (buffer-list))))))


(defun >>-xterm/get-default-term ()
  "Get the default term for the current buffer."
  (if >>-xterm/state    ; called while already in a terminal
    (or
      (plist-get >>-xterm/state :term)
      (error ">>= :term was not found in `>>-xterm/state': %s" >>-xterm/state))
    ;; else
    (or
      (cdr (assq major-mode >>-xterm-modes))
      (>>-xterm/command-name))))


(defsubst >>-xterm/get-implicit-tab-index ()
  "Get the implicit tab-index for the current buffer."
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



;;; Main functions

(defun >>=xterminal (term &optional prefix)
  "Trigger a smart terminal.

TERM must be a command generated by `>>=define-terminal'.  If nil its actual
value is determined using the `major-mode' of the current buffer or from a
running terminal property.

PREFIX argument encapsulates three concepts: tab-index, whether to invoke a
paste operation, and if a new tab must be added.  The default tab-index is
zero, and the implicit one is the last triggered saved in the context of the
local buffer.

An integer PREFIX value sets the tab-index to its absolute value, a paste
operation is invoked by using a negative value.  To paste to the default tab
use a plain `negative-argument', or `C--'.

A nil PREFIX means the implicit tab-index, a paste operation to it by a plain
`universal-argument', or `C-u'.  A double `C-u' issues the add new tab
condition."
  (interactive "i\nP")
  (unless term
    (setq term (>>-xterm/get-default-term)))
  (let (tab-index paste add-new
        (paster (>>-xterm/key term :paster))
        implicit
        (source (>>-xterm/current-buffer))
        target)
    (when prefix
      (cond
        ((consp prefix)
          (if (<= (prefix-numeric-value prefix) 5)
            (setq paste t)
            ;; else
            (setq add-new t)))
        ((integerp prefix)
          (setq
            tab-index (abs prefix)
            paste (< prefix 0)))
        (t    ; `negative-argument'
          (setq
            tab-index 0
            paste t))))
    (when add-new
      (setq tab-index (>>-xterm/search-new term)))
    (unless tab-index
      (setq
        implicit t
        tab-index (>>-xterm/get-implicit-tab-index)))
    (when paste
      (setq paste (>>-xterm/get-paste-text)))
    (setq target (>>-xterm/get-or-create-buffer term tab-index))
    (when (eq target source)
      (setq
        target (plist-get >>-xterm/state :source)
        paster nil
        implicit t)
      (unless (buffer-live-p target)
        (setq target (>>-xterm/get-alt-buffer term))))
    (unless implicit
      (set (make-local-variable '>>-xterm/linked) tab-index))
    (>>-xterm/switch-to-buffer target)
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
                "        Paste text:\n%s") paste)))
    target))


(defun >>=xterminal-add (&optional term)
  "Add a new tab for a TERM smart terminal."
  (interactive)
  (>>=xterminal term '(16)))    ; Double `C-u' is (16), add-new indicator


(defun >>=xterm-select (&optional term)
  "Select a smart terminal buffer from the given TERM.
When called interactively, a TERM given as a prefix argunment will calculate
the default value for the current buffer."
  (interactive "P")
  (when (and term (not (functionp term)))
    (setq term (>>-xterm/get-default-term)))
  (let* ((add-new ">>= add new tab.")
         (buffers (>>-xterm/select term))
         (collection (cons `(,add-new) buffers))
         (name (completing-read ">>= terminal: " collection nil t)))
    (if (string-equal name add-new)
      (>>=xterminal-add term)
      ;; else
      (let ((pair (assoc-string name buffers)))
        (when-let ((buffer (>>-xterm/check-buffer (cdr pair))))
          (let* ((state (buffer-local-value '>>-xterm/state buffer))
                 (term (plist-get state :term))
                 (tab-index (plist-get state :tab-index)))
            (>>=xterminal term tab-index)))))))



;;; Smart terminal generator

(defmacro >>=define-xterminal (id &optional docstring &rest keywords)
  "Define a command to manage smart terminals (xterms).

ID must be a symbol, this macro will generate two commands:
`>>-xterm/command-name'.  DOCSTRING is the terminal command documentation.

The following KEYWORDS can be used:

:program -- Executable command to be loaded as shell.  The value could be a
        string or a list of choices to find the first valid option.

:paster -- Function to paste text into a terminal.  A string is converted to a
        function using `>>=term/define-paste-magic'.  Could be specified as
        part of a form '(command . paster)' for a :program choice.  Its
        default value is `>>-xterm/paster'.

:mode -- Sequence of one or more identifiers to be added to `>>-xterm-modes'.

:buffer-name -- A string to be used as `buffer-name' for terminal tabs.  It
        defauls to the command name.

The defined command is a wrapper around `>>=xterminal'."
  (declare (doc-string 2) (indent 1) (debug t))
  (unless (symbolp id)
    (error ">>= invalid xterm ID '%s', must be a symbol" id))
  (when (and docstring (not (stringp docstring)))
    (setq
      keywords (cons docstring keywords)
      docstring nil))
  (unless docstring
    (setq docstring
      (format "Command for '%s' terminals (see `>>=xterminal')." id)))
  (setq keywords (>>-xterm/fix-keywords keywords))
  (let ((term (>>-xterm/command-name id))
        (modes (>>=cast-list (plist-get keywords :mode))))
    (macroexp-progn
      (delq nil
        `(
           ,(if keywords
              `(put ',term :keywords '(,@keywords)))
           ,(if modes
              `(setq >>-xterm-modes
                 (>>=mode-command-alist >>-xterm-modes ',term '(,@modes))))
           (defun ,term (&optional prefix)
             ,docstring
             (interactive "P")
             (>>=xterminal ',term prefix))
           )))))


(>>=define-xterminal main)


(>>=define-xterminal python
  :program (ipython . "%paste") python
  :mode python)


(>>=global-set-keys
  "C-c t" '>>=main-term
  "s-M-t" '>>=main-term
  [?\C-`] '>>=xterminal
  [?\C-~] '>>=xterminal-add
  "s-/" '>>=xterminal
  "s-?" '>>=xterminal-add
  "C-M-`" '>>=xterm-select)


(when >>=!emacs-as-wm
  ;; Like on i3 window manager
  (>>=global-set-keys "<s-return>" '>>=main-term))


(provide 'xorns-xterm)
;;; xorns-xterm.el ends here
