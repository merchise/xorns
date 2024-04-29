;;; xorns-prog.el --- Programming language source code editing  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;;; Commentary:

;; Generic definitions for editing programming language source code.

;; Enjoy!


;;; Code:

(eval-and-compile
  (require 'compile)
  (require 'xorns-tools)
  (require 'xorns-buffers)
  (require 'xorns-text)
  (require 'hideshow)
  (require 'transient)
  (require 'use-package))

(require 'xorns-term)



;;; Tools

(defun >>=compiler/add-match-errors (key regexp &rest args)
  "Add an element to the compilation error match association list.
KEY must be a symbol, REGEXP could be a string or a sexp syntax to be
converted by `rx-to-string', ARGS are valid definitions for (FILE [LINE COLUMN
TYPE HYPERLINK HIGHLIGHT...]) as defined in `compilation-error-regexp-alist'
variable documentation."
  (unless (stringp regexp)
    (setq regexp (rx-to-string regexp)))
  (add-to-list 'compilation-error-regexp-alist key)
  (add-to-list 'compilation-error-regexp-alist-alist `(,key ,regexp ,@args)))



;;; Common Systems

(defvar >>=|programming/features '(auto-fill flycheck yasnippet)
  "Features to turn on in prog mode.")


;; TODO: move this to `xorns-text.el', there are snippets not only for
;; programming modes.
(use-package yasnippet
  :when (memq 'yasnippet >>=|programming/features)
  :ensure t
  :demand t
  :commands yas-global-mode yas-load-directory
  :preface
  (defun >>=snippets/initialize ()
    "Initialize `xorns' snippets."
    (let* ((lib-dir (bound-and-true-p >>=!xorns/lib-dir))
           (snip-dir (expand-file-name "snippets" lib-dir)))
      (if (file-exists-p snip-dir)
        (progn
          (add-to-list 'yas-snippet-dirs snip-dir t)
          (yas-load-directory snip-dir))
        ;; else
        (message ">>= snippets directory '%s' does not exist." snip-dir))))
  :config
  (yas-global-mode +1)
  (>>=snippets/initialize))


(use-package flycheck
  :when (memq 'flycheck >>=|programming/features)
  :ensure t
  :commands global-flycheck-mode
  :custom
  (flycheck-idle-change-delay 10)
  :config
  (global-flycheck-mode +1))


(use-package prog-mode
  :init
  (defun >>=init-prog-mode ()
    "Init `prog-mode' based modes."
    (when (>>=local-buffer)
      (flyspell-prog-mode))
    (>>=init-text-mode)
    (when (memq 'flycheck >>=|programming/features)
      (turn-on-auto-fill))
    (subword-mode))
  :hook
  (prog-mode . >>=init-prog-mode))


(use-package conf-mode
  :after prog-mode
  :hook
  (conf-mode . >>=init-prog-mode))


;; `company-mode' can be managed internally by `lsp-mode'.  Other possible
;; choices are to activate it globally, or by using `prog-mode-hook'.
;; TODO: check if this should be included in `>>=|programming/features'
(use-package company
  :ensure t)


;; TODO (use-package eldoc



;;; Lisp

(use-package lisp-mode    ;; Common lisp
  :custom
  (emacs-lisp-docstring-fill-column 78)
  (lisp-indent-offset 2)
  (create-lockfiles nil))    ;; TODO: Conflict with 'pyls'?


(use-package elisp-mode
  ;; TODO: see https://codeberg.org/mychris/dotemacs
  :hook
  (emacs-lisp-mode . company-mode))



;;; Python

(defvar >>=|blacken/enable t
  ;; TODO: check if this should be included in `>>=|programming/features'
  "Whether `blacken' is enabled when entering `python-mode'.")


(defconst >>-!python/env-locators
  '(
     "venv"
     ".venv"
     ("poetry.lock" "poetry" "env" "info" "-p")
     ("Pipfile.lock" "pipenv" "--venv")
     (".python-version" "pyenv" "prefix")   ;; TODO [manu]: Clashes with rye
     )
  "Default python (virtual) environment locators.
See `>>=|python/env-locators' for more information.")


(defvar >>=|python/env-locators nil
  "Python (virtual) environment locators.
Extra definitions to be appended to `>>-!python/env-locators' default
definitions.  Each item must be either a string representing the MARK-FILE or
a list with the form (MARK-FILE [COMMAND]).  The result list will be used by
the function `>>=python/locate-env'.")


(autoload 'po-mode "po-mode"    ; TODO: Check this
  "Major mode for translators when they edit PO files." t)


(defun >>-python/check-env (path)
  "Check if PATH is a correct Python (virtual) environment."
  (when (stringp path)
    (setq path (expand-file-name path))
    (and
      (or
        (string-prefix-p >>=!home-dir path)
        (string-prefix-p temporary-file-directory path))
      (file-executable-p (>>=path/join path "bin" "python"))
      path)))


(defun >>-python/get-env (base item)
  "Get a Python environment from a BASE directory given an ITEM definition."
  (when (stringp item)
    (setq item (list item)))
  (let ((name (car item))
        (command (cadr item))
        (args (cddr item)))
    (when-let ((root (locate-dominating-file base name)))
      (let ((default-directory root))
        (>>-python/check-env
          (if command
            (if (stringp command)
              (car-safe (>>=process/safe-lines command args))
              ;; else
              (apply command name args))
            ;; else
            name))))))


(defun >>=python/locate-env (&optional root)
  "Look a Python (virtual) environment in the context of a ROOT workspace."
  (unless root
    (setq root default-directory))
  (seq-some
    (lambda (item) (>>-python/get-env root item))
    (append >>-!python/env-locators >>=|python/env-locators)))


(defconst >>=|pyright/extensions '(".py" ".pyi")
  "Extensions used by Pyright, a static type checker for Python.")


(defconst >>=|pyright-error-rx
  `(seq
     (* (| " " space))
     (group (seq (+ any) (| ,@>>=|pyright/extensions)))
     any
     (group (+ digit))
     any
     (group (+ digit))
     (* (| " " space))
     any
     (* (| " " space))
     "error:"))


(defconst >>=|pyright-warning-rx
  `(seq
     (* (| " " space))
     (group (seq (+ any) (| ,@>>=|pyright/extensions)))
     any
     (group (+ digit))
     any
     (group (+ digit))
     (* (| " " space))
     any
     (* (| " " space))
     "warning:"))


(>>=compiler/add-match-errors
  '>>=|pyright-error >>=|pyright-error-rx 1 2 3 2 1)
(>>=compiler/add-match-errors
  '>>=|pyright-warning >>=|pyright-warning-rx 1 2 3 1 1)


(use-package python
  :defer t
  :preface
  (defun -python-mode-setup()
    (outline-minor-mode)

    (when-let ((venv-path (>>=python/locate-env (>>=project-root))))
      ;; TODO: Check `lsp-pylsp-get-pyenv-environment' function, and
      ;; `lsp-after-initialize-hook' in Python: `lsp-pylsp-after-open-hook'.
      ;; (rename 'pylsp' -> 'pyls' for all cases)
      (let (modules)
        (when (boundp 'lsp-pylsp-plugins-jedi-environment)
          (>>=set 'lsp-pylsp-plugins-jedi-environment venv-path)
          (setq modules (cons "pylsp" modules)))
        (when (boundp 'lsp-pyls-plugins-jedi-environment)
          (>>=set 'lsp-pyls-plugins-jedi-environment venv-path)
          (setq modules (cons "pyls" modules)))
        (when (and modules init-file-debug)
          (message
            "Setting Python (virtual) environment '%s' in (%s) modules"
            venv-path (string-join modules ", "))))

      ;; lsp-pyright does its own lookup with the function
      ;; lsp-pyright-locate-venv; so we don't need to do anything here for it.
      ;; TODO: See other options:
      ;; https://github.com/emacs-lsp/lsp-python-ms/blob/master/lsp-python-ms.el
      ;; - lsp-python-ms--dominating-{pyenv|asdf|venv|conda}-python,
      ;; - lsp-python-ms-python-executable-cmd
      ))

  (defun -inferior-python-setup()
    ;; (setq-default indent-tabs-mode nil)
    (display-line-numbers-mode 0))
  :bind
  (:map python-mode-map
    ("C-m" . newline-and-indent))
  :hook
  (python-mode . -python-mode-setup)
  (inferior-python-mode . -inferior-python-setup))


(use-package with-venv
  :ensure t)


(use-package blacken
  :ensure t
  :commands blacken-mode blacken-buffer
  :preface
  (defun >>=blacken/turn-on ()
    "Setup `blacken' inner a file in `python-mode'."
    (interactive)
    (turn-off-auto-fill)
    (blacken-mode +1))

  (defun >>=blacken/try-reformat-buffer ()
    "Reformat current buffer if `blacken-mode' is active."
    (when (bound-and-true-p blacken-mode)
      (blacken-buffer init-file-debug)))

  (defun >>-blacken/may-enable-mode ()
    "Determine whether `blacken' may be enabled (see `>>=|blacken/enable')."
    (when >>=|blacken/enable
      (>>=blacken/turn-on)))
  :hook
  (python-mode . >>-blacken/may-enable-mode)
  (before-save . >>=blacken/try-reformat-buffer)
  :custom
  (blacken-line-length 'fill)
  (blacken-only-if-project-is-blackened t))


(use-package pipenv
  :ensure t
  :preface
  (declare-function pipenv-projectile-after-switch-extended 'pipenv)
  :hook
  (python-mode . pipenv-mode)
  :custom
  (pipenv-projectile-after-switch-function
    #'pipenv-projectile-after-switch-extended))



;;; Python terminal

(defvar >>=|python/use-ipython-as-terminal t
  "Try using IPython as a terminal if it is installed.")


(if (and >>=|python/use-ipython-as-terminal (executable-find "ipython"))
  (>>=term/define >>=python-term
    :program "ipython"
    :buffer-name "*PYTHON*"
    :modes 'python-mode
    :paster "%paste"
    )
  ;; else
  (>>=term/define >>=python-term
    :program "python"
    :buffer-name "*PYTHON*"
    :modes 'python-mode))



;;; Language Server Protocol

(require 'dash)
(require 's)
(require 'cl-lib)


(defvar >>=|lsp/enable-mode t
  "Determine when to enable `lsp' when entering a programming mode.
Language server activation is based on the logic of the `>>=check-major-mode'
function.  Value t is translated to use `>>-lsp-buffer?' function.")


(defvar >>=|lsp/startup-deferred nil
  "Use the entry point that defers server startup until buffer is visible.")


(defvar >>=|lsp/use-pyright nil
  ;; TODO: check if this should be included in `>>=|programming/features'
  "Use lsp-pyright.")


(use-package lsp-mode
  :ensure t
  :demand t
  :commands lsp lsp-deferred
  :autoload lsp-buffer-language--configured-id lsp-format-buffer
  :preface
  (defsubst >>=lsp/startup-function ()
    "Return LSP entry point function based on `>>=|lsp/startup-deferred'."
    (if >>=|lsp/startup-deferred 'lsp-deferred 'lsp))

  (defsubst >>-lsp/entry-point ()
    "Entry point that determines whether to defer the server startup or not."
    (funcall (>>=lsp/startup-function)))

  (defun >>-lsp-buffer? ()
    "Validate current buffer language for `lsp-language-id-configuration'."
    ;; based on `lsp-buffer-language'
    (and
      (buffer-file-name)
      (lsp-buffer-language--configured-id)))

  (defun >>-lsp/enable-mode-function ()
    (cond
      ((null >>=|lsp/enable-mode)
        nil)
      ((eq >>=|lsp/enable-mode 'ask)
        'lsp)
      ((functionp >>=|lsp/enable-mode)
        >>=|lsp/enable-mode)
      (t
        '>>-lsp-buffer?)))

  (defun >>-lsp/may-enable-server ()
    "Determine whether `lsp' may be enabled (see `>>=|lsp/enable-mode')."
    (let ((fn (>>-lsp/enable-mode-function)))
      (when (and fn (funcall fn))
        (>>-lsp/entry-point))))

  (defun >>=lsp/safe-format-buffer ()
    "Ask the server to format this document, checking if possible."
    (interactive)
    (when lsp-mode
      (lsp-format-buffer)))
  :custom
  (lsp-auto-guess-root t)
  (lsp-keymap-prefix "C-s-l")    ; "s-l" is the lock key in several laptops
  :hook
  (prog-mode . >>-lsp/may-enable-server))


(use-package lsp-ui
  :ensure t
  :hook
  (lsp-mode . lsp-ui-mode)
  ;; :custom (lsp-ui-doc-delay 1.0)
  :bind
  (:map lsp-ui-mode-map
    ("C-S-c C-S-j" . lsp-ui-imenu)
    ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
    ([remap xref-find-references] . lsp-ui-peek-find-references))
  :config
  (advice-add 'lsp-ui-imenu :after (lambda () (setq mode-line-format nil))))


(use-package lsp-pyright
  :when >>=|lsp/use-pyright
  :ensure t)



;;; Debug Adapter Protocol

(defvar >>=|dap/enable t
  ;; TODO: check if this should be included in `>>=|programming/features'
  "Determines if `dap-mode' (Debug Adapter Protocol) is configured.")


(use-package dap-mode
  :when >>=|dap/enable
  :ensure t
  :after lsp-mode
  :commands dap-debug
  :preface
  (defvar >>=|dap/python-debugger nil
    "Which Python debugger to use (calculated if not given).")

  (defun >>-dap/python-debugger ()
    "Calculate which Python debugger is active ('debugpy' or `ptvsd')."
    (or
      >>=|dap/python-debugger
      (seq-find
        (lambda (pkg)
          (>>=process/safe-lines
            "python" "-m" (symbol-name pkg) "--version"))
        '(debugpy ptvsd))))
  :init
  (transient-define-prefix >>=dap/menu ()
    "DAP local menu."
    [["Stepping"
       ("n" "Next" dap-next)
       ("i" "Step in" dap-step-in)
       ("o" "Step out" dap-step-out)
       ("c" "Continue" dap-continue)
       ("r" "Restart frame" dap-restart-frame)
       ("q" "Disconnect" dap-disconnect)]
     ["Switch"
       ("ss" "Session" dap-switch-session)
       ("st" "Thread" dap-switch-thread)
       ("sf" "Stack frame" dap-switch-stack-frame)
       ("su" "Up stack frame" dap-up-stack-frame)
       ("sd" "Down stack frame" dap-down-stack-frame)
       ("sl" "List locals" dap-ui-locals)
       ("sb" "List breakpoints" dap-ui-breakpoints)
       ("sS" "List sessions" dap-ui-sessions)]
     ["Breakpoints"
       ("bb" "Toggle" dap-breakpoint-toggle)
       ("ba" "Add" dap-breakpoint-add)
       ("bd" "Delete" dap-breakpoint-delete)
       ("bk" "Delete All" dap-breakpoint-delete-all)
       ("bc" "Set condition" dap-breakpoint-condition)
       ("bh" "Set hit condition" dap-breakpoint-hit-condition)
       ("bl" "Set log message" dap-breakpoint-log-message)]
     ["Eval"
       ("ee" "Eval" dap-eval)
       ("ea" "Add expression" dap-ui-expressions-add)
       ("er" "Eval region" dap-eval-region)
       ("es" "Eval thing at point" dap-eval-thing-at-point)]
     ["Debug"
       ("dd" "Debug" dap-debug)
       ("ds" "Debug restart" dap-debug-restart)
       ("dr" "Debug recent" dap-debug-recent)
       ("dl" "Debug last" dap-debug-last)
       ("de" "Edit debug template" dap-debug-edit-template)]]
    (interactive)
    (transient-setup '>>=dap/menu))
  :bind
  (:map dap-mode-map
    ("C-s-d" . >>=dap/menu))
  :config
  (use-package dap-lldb)
  (use-package dap-python
    :config
    (when-let ((debugger (>>-dap/python-debugger)))
      (setq dap-python-debugger debugger))))


;;; C/C++ Mode -- Linux kernel programming

(use-package cc-mode
  :bind
  (:map c-mode-base-map
    ("C-c c" . compile))
  :hook
  (c-mode-common .
    (lambda ()
      (c-set-style "linux")
      (setq tab-width 4)
      (setq c-basic-offset 4))))



;;; Javascript, CoffeeScript and LiveScript

(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" "\\.pac\\'" "node")
  :custom
  (js2-strict-inconsistent-return-warning nil)
  (js2-strict-missing-semi-warning nil))



;;; XML, SGML, and Web editing modes

(use-package sgml-mode
  :hook
  (sgml-mode . sgml-electric-tag-pair-mode))


(use-package nxml-mode
  :custom
  (nxml-auto-insert-xml-declaration-flag t)
  (nxml-slash-auto-complete-flag t))


;;; Selectively display code/comment blocks

(use-package hideshow
  :preface
  (defun >>=toggle-hide-show (&optional arg)
    "Toggle block hiding/showing if ARG is nil, >=0 show-all, <0 hide-all."
    (interactive "P")
    (if (null arg)
      (hs-toggle-hiding)
      ;; else
      (if (>= (prefix-numeric-value arg) 0)
        (hs-show-all)
        ;; else
        (hs-hide-all))))
  :bind
  (:map hs-minor-mode-map
    ("C-=" . >>=toggle-hide-show)
    ("C-+" . hs-show-all))
  :hook
  ((prog-mode . hs-minor-mode)
   (nxml-mode . hs-minor-mode))
  :config
  (add-to-list 'hs-special-modes-alist
    '(nxml-mode
       "<!--\\|<[^/>]*[^/]>"
       "-->\\|</[^/>]*[^/]>"
       "<!--"
       sgml-skip-tag-forward
       nil)))


(provide 'xorns-prog)
;;; xorns-prog.el ends here
