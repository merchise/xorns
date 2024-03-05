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

  (defvar >>=|term/emulator-class '>>=term/vt
    "Default terminal emulator class, a sub-class of `>>=term/emulator'.")

  (defvar >>=|term/install-customizable-colors t
    "Add customizable 256 color support to `term' and `ansi-term' .")

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


(defmethod >>=term/get-buffer-name (base-name &optional prefix)
  "Return a new `buffer-name' based on BASE-NAME and PREFIX arguments."
  (unless base-name (setq base-name >>=|term/default-buffer-name))
  (cond
    ((null prefix)
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
  :ensure helm
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


(use-package vterm
  :ensure t
  :defines vterm-exit-functions
  :commands vterm vterm-send-string
  :custom
  (vterm-max-scrollback 10000)
  (vterm-always-compile-module t)
  (vterm-shell (>>=term/shell-file-name))
  :config
  (add-hook 'vterm-exit-functions '>>-term/handle-exit))



;;; Managed Terminals

(define-error 'missing-instance
  "Existent buffer with no associated instance")


(defclass >>=term/settings ()
  (;; instance slots
    (emulator-class
      :initform (>>=value-of >>=|term/emulator-class)
      :initarg :emulator-class
      :type class
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
      :initform nil
      :type (or null object)
      :allocation :class
      :documentation
      "First terminal emulator instance."))
  :documentation
  "Terminal emulator settings (logical meta-class of `>>=term/emulator').")


(defclass >>=term/emulator ()
  (;; instance slots
    (settings
      :initform nil
      :initarg :settings
      :type (or null object)    ;; >>=term/settings
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


(defun >>-term/get-instance (buffer)
  "Get the terminal emulator instance from an existing BUFFER."
  (if-let ((obj (>>=toolbox/property :term-instance buffer)))
    obj
    ;; else
    (signal 'missing-instance `(:buffer-name ,(buffer-name buffer)))))


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
  (let ((name (>>=term/get-buffer-name (oref settings buffer-name) prefix)))
    (if-let ((buffer (get-buffer name)))    ;; existing buffer
      (>>-term/get-instance buffer)
      ;; else
      (setq buffer (>>-term/create-buffer class (oref settings program) name))
      (let ((obj (super class :settings settings :buffer buffer)))
        (>>=toolbox/set-properties buffer :term-instance obj)
        obj))))


(defun >>-term/get-default-settings ()
  "Get the default terminal configuration settings for the `current-buffer'."
  (or
    (when-let ((obj (>>=toolbox/property :term-instance))) ; >>=term/emulator
      (oref obj settings))
    (cdr (assq major-mode (oref-default '>>=term/settings global-modes)))
    (oref-default '>>=term/settings main-instance)
    (signal 'missing-instance `(:term-instance ,(current-buffer)))))


(defun >>=term/launch (terminal &optional prefix)
  "Launch a TERMINAL using PREFIX to complement the buffer name.
TERMINAL must be a `>>=term/settings' instance.  If nil, its actual value is
determined based on default values for the `current-buffer' (see function
`>>-term/get-default-settings').  Argument PREFIX represents the terminal tab
to launch.  An integer PREFIX value sets the tab-index to its value.  A nil
PREFIX means the linked terminal if any or the implicit tab-index."
  (interactive "i\nP")
  (unless terminal
    (setq terminal (>>-term/get-default-settings)))
  (let* ((res (make-instance (oref terminal emulator-class) terminal prefix))
         (buffer (oref res buffer)))
    (>>=toolbox/switch-to-buffer buffer)
    res))


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


(defclass >>=term/vt (>>=term/emulator) ()
  "Specialization class for `xorns' terminals using `vterm'.")


(defmethod >>-term/create-buffer
  ((_ (subclass >>=term/vt)) program name)
  "Create an `vterm' running PROGRAM and using NAME for the new buffer."
  (let ((vterm-shell program))
    (save-window-excursion
      (vterm name))))


(defmethod >>-term/send-string ((term >>=term/vt) string)
  "Send STRING to a `vterm' TERM shell session."
  (with-current-buffer (oref term buffer)
    (vterm-send-string string)))


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


(provide 'xorns-term)
;;; xorns-term.el ends here
