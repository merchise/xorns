;;; xorns-term.el --- Basic terminal support  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;;; Commentary:

;; In this module are configured all the features related to terminals.
;; Several terminal modules are configured: `eshell', `comint', `term', and
;; `vterm'.

;; Customizable 256 colors are configured by default for `term' and
;; `ansi-term', to disable it set `>>=|term/install-customizable-colors'
;; variable to nil.

;; Object Oriented Programming is used to configure Managed Terminals.  A
;; managed terminal can be defined with the macro `>>=term/define' using
;; several keyword arguments as defined in the class `>>=term/class'.
;; Examples of terminal class instances are `>>=main-term' (the default
;; terminal), and `>>=python-term' (for Python modes).

;; The function `>>=term/launch' orchestrates all terminal kinds based on
;; their associations with major modes.  Each terminal kind can launch several
;; terminal tabs, each one is identified with a zero (the default) or positive
;; integer.  To select a terminal tab a prefix argument is used.  A negative
;; value is used to execute a paste operation from the current buffer to a
;; target terminal (using the absolute value).  The `universal-argument'
;; (`C-u') is used to paste to the default tab, and `negative-argument'
;; (`C--') for the tab with index zero.

;; Every time a terminal is launched, a reference to the `current-buffer' is
;; linked to that terminal tab, executing a command to select an active tab
;; will switch, or paste, to the linked buffer.

;; Enjoy!


;;; Code:

(eval-and-compile
  (require 'esh-io)
  (require 'em-alias)    ; eshell/alias
  (require 'term)
  (require 'use-package)
  (require 'xorns-tools)
  (require 'xorns-window))



;;; Common setup

(eval-and-compile
  (defvar >>=|term/default-buffer-name "*TERMINAL*"
    "Default buffer name when creating a new terminal.")

  (defvar >>=|term/emulator-class nil
    "Terminal emulator class, sub-class of `>>=term/emulator'.
Could be `>>=term/ansi' (the default) or `>>=term/vt' (the recommended).")

  (defvar >>=|term/install-customizable-colors t
    "Add customizable 256 color support to `term' and `ansi-term' .")

  (defsubst >>-term/vt-p ()
    "Check if `vterm' should be activated."
    (memq >>=|term/emulator-class '(>>=term/vt t)))

  (defun >>-term/emulator-class ()
    "Get the configured value for `>>=|term/emulator-class'."
    (cond
      ((null >>=|term/emulator-class)
        '>>=term/ansi)
      ((>>-term/vt-p)
        '>>=term/vt)
      (t
        >>=|term/emulator-class)))

  (defun >>=term/shell-file-name ()
  "Get the executable file name to load inferior shells from."
  (purecopy
    (>>=executable-find
      explicit-shell-file-name
      shell-file-name
      (getenv "ESHELL")
      (getenv "SHELL")
      "bash"
      "zsh"
      "sh"))))



;;; Common tools

(defalias '>>-term/handle-exit '>>=safe-kill-buffer-and-window)


(defun >>=term/get-buffer-name (base-name &optional prefix)
  "Return a new `buffer-name' based on BASE-NAME and PREFIX arguments."
  (unless base-name (setq base-name >>=|term/default-buffer-name))
  (cond
    ((or (null prefix) (eq prefix 0) (eq prefix '-))
      base-name)
    ((integerp prefix)
      (format "%s<%s>" base-name prefix))
    ((stringp prefix)
      prefix)
    (t
      (generate-new-buffer-name base-name))))



;;; Packages

;; TODO: https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org
(use-package eshell
  :defines eshell-visual-commands eshell-prompt-regexp
  :functions eshell-cmpl-initialize
  :commands eshell eshell-command
  :init
  (defun >>-eshell/delete-char-or-quit (arg)
    (interactive "p")
    ;; TODO: check using not nil value for `limit' in `looking-back'
    (if (and (eolp) (looking-back eshell-prompt-regexp nil))
      (progn
        (eshell-life-is-too-much)
        (ignore-errors    ; TODO: use `kill-buffer-and-window'
          (delete-window)))
      ;; else
      (delete-char arg)))

  (defun >>-eshell/first-time ()
    "Run the first time `eshell-mode' is entered.a"
    (add-to-list 'eshell-visual-commands "htop"))

  (defun >>-eshell/init ()
    "Initialize eshell."
    (eshell-cmpl-initialize)
    (keymap-set eshell-mode-map "C-d" '>>-eshell/delete-char-or-quit)
    (when (bound-and-true-p helm-mode)
      (keymap-set eshell-mode-map
        "<remap> <eshell-pcomplete>" 'helm-esh-pcomplete)
      (keymap-set eshell-mode-map
        "<remap> <eshell-list-history>" 'helm-eshell-history)))
  :custom
  (eshell-history-size 1024)
  (eshell-hist-ignoredups t)
  :hook
  (eshell-first-time-mode . >>-eshell/first-time)
  (eshell-mode . >>-eshell/init)
  :config
  (eshell/alias "l" "ls -lh $1")
  (eshell/alias "ll" "ls -alhF $1")
  (eshell/alias "la" "ls -A $1"))


(use-package eshell-syntax-highlighting
  ;; TODO: Check `eshell-syntax-highlighting-highlight-in-remote-dirs'
  :after eshell
  :ensure t
  :hook
  (eshell-mode . eshell-syntax-highlighting-mode))


(use-package comint
  :preface
  (defun >>-comint/init ()
    "Initialize comint."
    (when (bound-and-true-p helm-mode)
      (require 'helm-eshell)
      (keymap-set comint-mode-map "C-c C-l" 'helm-comint-input-ring)
      (keymap-set comint-mode-map "M-s f" 'helm-comint-prompts-all)))
  :hook
  (comint-mode . >>-comint/init))


(use-package term
  :preface
  (defun >>-term/raw-kill-line ()
    "Kill the rest of the current line in `term-char-mode'."
    (interactive)
    (term-send-raw-string "\C-k")
    (kill-line))

  (defun >>=ansi-term (&optional program name)
    "Create an interactive terminal buffer running PROGRAM and a buffer NAME."
    (interactive)
    (unless program (setq program (>>=term/shell-file-name)))
    (unless name (setq name >>=|term/default-buffer-name))
    (let ((prog (split-string-shell-command program)))
      (apply 'term-ansi-make-term name (car prog) nil (cdr prog)))
    (set-buffer name)
    (term-mode)
    (term-char-mode)
    (let (term-escape-char)
      (term-set-escape-char ?\C-x))
    (switch-to-buffer name))
  :bind
  (:map term-raw-map
    ("C-y" . term-paste)
    ("C-k" . >>-term/raw-kill-line))
  :custom
  (term-input-autoexpand t)
  :config
  (let ((key (concat (key-description term-escape-char) " C-t")))
    (keymap-set term-mode-map key 'term-char-mode)
    (keymap-set term-raw-map key 'term-line-mode))
    (advice-add 'term-handle-exit :after '>>-term/handle-exit))


(use-package eterm-256color
  :when >>=|term/install-customizable-colors
  :after term
  :ensure t
  :hook
  (term-mode . eterm-256color-mode))



;;; Managed Terminals

(define-error 'missing-instance
  "Existent buffer with no associated instance")


(defvar >>-term/linked nil
  "Local variable to link common buffers with terminals.")


(defclass >>=term/settings ()
  (;; instance slots
    (emulator-class
      :initform (>>-term/emulator-class)
      :initarg :emulator-class
      :type symbol    ;; class
      :documentation
      "Terminal emulator class, a sub-class of `>>=term/emulator'.")
    (buffer-name
      :initform (identity >>=|term/default-buffer-name)
      :initarg :buffer-name
      :type string
      :documentation
      "Base buffer name.")
    (program
      :initform (>>=term/shell-file-name)
      :initarg :program
      :type string
      :documentation
      "Executable file-name to load inferior shells from.")
    (modes
      :initform nil
      :initarg :modes
      :type (or null symbol list)         ;; list of major modes
      :documentation
      "A `major-mode' list to associate with terminal meta instances.")
    (paster
      :initform nil
      :initarg :paster
      :type (or null string function)
      :documentation
      "How to paste text into a terminal buffer.")
    ;; class slots
    (global-modes
      :initform nil
      :type list
      :allocation :class
      :documentation
      "A global mapping of (MAJOR-MODE . TERMINAL-SETTINGS).")
    (main-instance
      ;; :type `>>=term/emulator'
      :initform nil
      :allocation :class
      :documentation
      "First terminal emulator instance."))
  :documentation
  "Terminal emulator settings (logical meta-class of `>>=term/emulator').")


(defclass >>=term/emulator ()
  (;; instance slots
    (settings
      ;; type `>>=term/settings'
      :initform nil
      :initarg :settings
      :documentation
      "Terminal configuration settings that created this instance.")
    (buffer
      :initform nil
      :initarg :buffer
      :type (or null buffer)
      :documentation
      "Created buffer."))
  :abstract t
  :documentation
  "Base class for `xorns' terminal emulators.")


(defgeneric >>-term/create-buffer (emulator &optional name)
  "Create a new terminal EMULATOR buffer.")


(defgeneric >>-term/send-string (emulator string)
  "Send STRING to a terminal EMULATOR shell session.")


(defsubst >>-term/check-prefix (prefix base-name)
  "Check PREFIX for BASE-NAME."
  (when (string= (>>=prefix prefix (length base-name)) base-name)
    prefix))


(defun >>-term/get-defaul-prefix (base-name)
  "Get default prefix for BASE-NAME."
  (or
    (>>-term/check-prefix >>-term/linked base-name)
    (>>-term/check-prefix (buffer-name) base-name)))


(defun >>=term/buffer-p (&optional buffer-or-name terminal)
  "Return non-nil when BUFFER-OR-NAME is a TERMINAL buffer."
  (when-let ((obj (>>=toolbox/property :term-instance buffer-or-name)))
    (when (or (null terminal) (eq (oref obj settings) terminal))
      obj)))


(defun >>-term/get-instance (buffer)
  "Get the terminal emulator instance from an existing BUFFER."
  (if-let ((obj (>>=term/buffer-p buffer)))
    obj
    ;; else
    (signal 'missing-instance (list :buffer-name (buffer-name buffer)))))


(defmethod make-instance
  ((_ (subclass >>=term/settings)) &rest slots)
  "Create a terminal settings instance using SLOTS arguments.
Do not use this method directly, use `>>=term/define' macro instead."
  (let ((res (super)))
    ;; check main instance
    (unless (oref-default '>>=term/settings main-instance)
      (oset-default '>>=term/settings main-instance res))
    ;; set global modes
    (when-let ((modes (plist-get slots :modes)))
      (let ((gmodes (oref-default '>>=term/settings global-modes)))
        (dolist (mode (>>=cast-list modes))
          (if-let ((cell (assq mode gmodes)))
            (setcdr cell res)
            ;; else
            (setq gmodes (cons (cons mode res) gmodes))))
        (oset-default '>>=term/settings global-modes gmodes)))
    res))


(defmethod make-instance
  ((class (subclass >>=term/emulator)) settings &optional prefix)
  "Create a terminal emulator instance.
Instance is built based on the emulator CLASS, a SETTINGS definition, and an
optional interactive command PREFIX argument."
  (let ((base-name (oref settings buffer-name)))
    (unless prefix
      (when-let ((aux (>>-term/get-defaul-prefix base-name)))
        (when (get-buffer aux)
          (setq prefix aux))))
    (let ((name (>>=term/get-buffer-name base-name prefix)))
      (if-let ((buffer (get-buffer name)))    ;; existing buffer
        (>>-term/get-instance buffer)
        ;; else
        (let* ((program (oref settings program))
               (buf (>>-term/create-buffer class program name))
               (obj (super class :settings settings :buffer buf)))
          (>>=toolbox/set-properties buf :term-instance obj)
          obj)))))


(defun >>-term/get-default-terminal ()
  "Get the default terminal settings for the `current-buffer'."
  (or
    (when-let ((obj (>>=term/buffer-p)))     ; >>=term/emulator
      (oref obj settings))
    (cdr (assq major-mode (oref-default '>>=term/settings global-modes)))
    (oref-default '>>=term/settings main-instance)
    (signal 'missing-instance `(:term-instance ,(current-buffer)))))


(defun >>-term/linked-p (item name)
  "Check if ITEM (a buffer or a window) is linked with NAME."
  (let ((buf (if (bufferp item) item (window-buffer item))))
    (string-equal (buffer-local-value '>>-term/linked buf) name)))


(defun >>-term/link-buffers (source target)
  "Link SOURCE and TARGET buffers."
  (unless (>>=term/buffer-p source)
    (set (make-local-variable '>>-term/linked) (buffer-name target))
    (>>=toolbox/set-properties target :linked-source source)))


(defun >>-term/visible-linked-buffer (name)
  "Find a visible BUFFER linked to terminal NAME."
  (let ((res (>>-window/find-first (lambda (w) (>>-term/linked-p w name)))))
    (when res
      (window-buffer res))))


(defun >>-term/get-linked-buffer ()
  "Get buffer linked to a terminal buffer."
  (or
    ;; source buffer
    (when-let ((res (>>=toolbox/property :linked-source)))
      (when (buffer-live-p res)
        res))
    ;; visible window or buffer
    (let ((name (buffer-name)))
      (or
        (>>-term/visible-linked-buffer name)
        (car (match-buffers '>>-term/linked-p nil name))))))


(defun >>-term/get-paste-text ()
  "Return a list of TERMINAL buffers."
  (unless (>>=term/buffer-p)
    (>>=str-non-empty (>>=str-trim (>>=buffer-focused-text)))))


(defsubst >>-term/check-eol (text)
  "Check if TEXT ends with an EOL."
  (let ((EOL "\n"))
    (if (string= (>>=suffix text 1) EOL)
      text
      ;; else
      (concat text EOL))))


(defun >>-term/get-paster (&optional buffer)
  "Get the paste function for BUFFER."
  (unless buffer
    (setq buffer (current-buffer)))
  (when-let ((obj (>>=toolbox/property :term-instance buffer)))
    (let* ((settings (oref obj settings))
           (paster (oref settings paster)))
      (cond
        ((null paster)
          (lambda (text)
            (when text
              (>>-term/send-string obj (>>-term/check-eol text)))))
        ((functionp paster)
          (lambda (text)
            (when text
              (>>-term/send-string obj
                (>>-term/check-eol (funcall paster text))))))
        ((stringp paster)
          (lambda (text)
            (when text
              (if (string-match-p "\n" text)
                (progn
                  (kill-new text)
                  (>>-term/send-string obj (>>-term/check-eol paster))
                  (current-kill 1))
                ;; else
                (>>-term/send-string obj (>>-term/check-eol text))))))
        (t
          (user-error ">>= invalid terminal paster '%s'" paster))))))


(defun >>=term/buffer-list (&optional terminal)
  "Return a list of TERMINAL buffers."
  (match-buffers '>>=term/buffer-p nil terminal))


(define-obsolete-function-alias '>>=xterminal '>>=term/launch "0.11.0")
(defun >>=term/launch (terminal &optional prefix)
  "Launch a TERMINAL using PREFIX to complement the buffer name.
TERMINAL must be a `>>=term/settings' instance.  If nil, its actual value is
determined based on default values for the `current-buffer' (see function
`>>-term/get-default-terminal').  Argument PREFIX represents the terminal tab
to launch.  An integer PREFIX value sets the tab-index to its value.  A nil
PREFIX means the linked terminal if any or the implicit tab-index."
  (interactive "i\nP")
  (unless terminal
    (setq terminal (>>-term/get-default-terminal)))
  (let* ((obj (make-instance (oref terminal emulator-class) terminal prefix))
         (current (current-buffer))
         (target (oref obj buffer)))
    (if (eq target current)
      (when-let ((source (>>-term/get-linked-buffer)))
        (>>=toolbox/switch-to-buffer source)
        (>>-term/link-buffers source target)
        source)
      ;; else
      (>>-term/link-buffers current target)
      (>>=toolbox/switch-to-buffer target)
      target)))


(defun >>=term/add (&optional terminal)
  "Add a new tab for a TERMINAL."
  (interactive)
  (>>=term/launch terminal t))


(defun >>=term/select ()
  "Select a terminal buffer."
  (interactive)
  (let* ((items
           (cons
             (list "[add new tab]")
             (mapcar
               (lambda (buffer) (cons (buffer-name buffer) buffer))
               (>>=term/buffer-list))))
         (name (completing-read ">>= terminal: " items nil t)))
    (if-let ((buffer (cdr (assoc-string name items))))
      (progn
        (>>=toolbox/switch-to-buffer buffer)
        buffer)
      ;; else
      (>>=term/add))))


(defun >>=term/paste (&optional prefix)
  "Paste current buffer selected text to the linked terminal.
When the optional argument PREFIX is not nil, an existing terminal is used or
selected."
  (interactive "P")
  (when-let ((text (>>-term/get-paste-text)))
    (>>=toolbox/switch-to-buffer
      (save-window-excursion
        (cond
          ((consp prefix)
            (>>=term/select))
          ((bufferp prefix)
            prefix)
          ((instance-of prefix '>>=term/emulator)
            (oref prefix buffer))
          ((instance-of prefix '>>=term/settings)
            (>>=term/launch prefix))
          (t
            (>>=term/launch nil prefix)))))
    (funcall (>>-term/get-paster) text)))


(defclass >>=term/ansi (>>=term/emulator) ()
  "Specialization class for `xorns' terminals using `ansi-term'.")


(defmethod >>-term/create-buffer
  ((_ (subclass >>=term/ansi)) program name)
  "Create an `ansi-term' running PROGRAM and using NAME for the new buffer."
  (save-window-excursion
    (>>=ansi-term program name)))


(defmethod >>-term/send-string ((term >>=term/ansi) string)
  "Send STRING to an ANSI TERM shell session."
  (with-current-buffer (oref term buffer)
    (term-send-raw-string string)))

(defun >>-term/vt-setup ()
  "Setup `vterm'."
  (use-package vterm
    :ensure t
    :defines vterm-mode-map vterm-exit-functions vterm-always-compile-module
    :commands vterm vterm-send-string vterm-send-key vterm-end-of-line
    vterm--set-directory
    :init
    (defun >>-vterm/kill-line ()
      "Kill the rest of the current line."
      (interactive)
      (kill-ring-save (point) (vterm-end-of-line))
      (vterm-send-key "k" nil nil t))

    (defun >>=vterm/directory-sync ()
      "Synchronize current working directory in `vterm' BUFFER."
      (interactive)
      (when (bound-and-true-p vterm--process)
        (vterm--set-directory
          (file-truename
            (format "/proc/%d/cwd/" (process-id vterm--process))))))
    :custom
    (vterm-max-scrollback 10000)
    (vterm-always-compile-module t)
    (vterm-shell (>>=term/shell-file-name))
    :bind
    (:map vterm-mode-map
      ("C-y" . vterm-yank)
      ("M-y" . vterm-yank-pop)
      ("C-k" . >>-vterm/kill-line))
    :config
    (advice-add 'vterm-send-return :after '>>=vterm/directory-sync)
    (add-hook 'vterm-exit-functions '>>-term/handle-exit))

  (defclass >>=term/vt (>>=term/emulator) ()
    "Specialization class for `xorns' terminals using `vterm'.")

  ;; avoid warning the function is not known to be defined
  (declare-function >>=term/vt--eieio-childp nil)

  (defmethod >>-term/create-buffer
    ((_ (subclass >>=term/vt)) program name)
    "Create an `vterm' running PROGRAM and using NAME for the new buffer."
    (require 'vterm nil 'noerror)
    (unless (boundp 'vterm-shell)
      (defvar vterm-shell))
    (let ((vterm-shell program))
      (save-window-excursion
        (funcall 'vterm name))))

  (defmethod >>-term/send-string ((term >>=term/vt) string)
    "Send STRING to a `vterm' TERM shell session."
    (with-current-buffer (oref term buffer)
      (vterm-send-string string))))


(when (>>-term/vt-p)
  (add-hook 'emacs-startup-hook '>>-term/vt-setup))


(eval-and-compile
  (defconst >>-!term/command-doc
    (concat
      "Start a new %s terminal session, or switch to an already active "
      "session. Return the buffer selected (or created). PREFIX argument "
      "represents the terminal tab to launch. An integer PREFIX value sets "
      "the tab-index to its value. A nil PREFIX means the linked terminal if "
      "any or the implicit tab-index."))

  (defun >>-term/clean-name (name)
    "Clean a terminal NAME (used in `>>=term/define')."
    (let ((res (symbol-name name)))
      (setq
        res (>>=safe-replace ">>=" "" res)
        res (>>=safe-replace "-?term\\(inal\\)?-?" "" res)
        res (>>=safe-replace "-?shell-?" "" res))
      (upcase res))))


(defmacro >>=term/define (name &rest keywords)
  "Define a new terminal manager.
Define NAME as a variable and a command to manage terminals.  KEYWORDS are
slots defined in `>>=term/settins'.  The defined command is a wrapper around
`>>=term/launch'."
  (declare (indent 1) (debug t))
  (let ((id (>>-term/clean-name name)))
    `(progn
       (defconst ,name (make-instance '>>=term/settings ,@keywords)
         ,(format "%s terminal configuration." id))
       (defun ,name (&optional prefix)
         ,(concat
            (format "Launch an interactive %s terminal buffer.\n" id)
            (internal--format-docstring-line >>-!term/command-doc id))
         (interactive "P")
         (>>=term/launch ,name prefix)))))



;;; Define terminal settings

(>>=term/define >>=main-term)


(defun >>=term/paste-to-main (&optional prefix)
  "Paste current buffer selected text to the main terminal.
Optional PREFIX argument has the same meaning as in `>>=term/launch'"
  (interactive "P")
  (when-let ((text (>>-term/get-paste-text)))
    (>>=term/launch >>=main-term prefix)
    (funcall (>>-term/get-paster) text)))


(>>=bind-global-keys
  "C-c t" >>=main-term
  "C-c C-t" >>=term/paste-to-main
  "s-M-t" >>=main-term
  "C-`" >>=term/launch
  "C-~" >>=term/add
  "s-/" >>=term/launch
  "s-?" >>=term/add
  "C-M-`" >>=term/select
  "M-s-/" >>=term/paste)


(when (bound-and-true-p >>=!emacs-as-wm)
  ;; Like on my i3 window manager
  (>>=bind-global-keys
    "<s-return>" >>=main-term
    "<M-s-return>" >>=term/paste-to-main))


(provide 'xorns-term)
;;; xorns-term.el ends here
