;;; xorns-prog.el --- Programming language source code editing  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;;; Commentary:

;; Generic definitions for editing programming language source code.

;; Enjoy!


;;; Code:

(eval-and-compile
  (require 'xorns-text)
  (require 'xorns-tools)
  (require 'xorns-buffers)
  (require 'xorns-term)
  (require 'hideshow)
  (require 'transient)
  (require 'use-package nil 'noerror))



;;; Common Systems

(use-package auto-complete
  :ensure t
  :preface
  (declare-function ac-flyspell-workaround 'auto-complete)
  :custom
  (ac-trigger-key "TAB")
  :config
  ;; TODO: check `global-auto-complete-mode'
  (ac-flyspell-workaround))


(use-package yasnippet
  :ensure t
  :preface
  (declare-function yas-global-mode 'yasnippet)
  (declare-function yas-load-directory 'yasnippet)

  (defun >>=snippets/initialize ()
    "Initialize `xorns' snippets."
    (let* ((lib-dir (>>=value-of >>=!xorns/lib-dir))
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
  :ensure t
  :preface
  (declare-function global-flycheck-mode 'flycheck)
  :custom
  (flycheck-idle-change-delay 10)
  :config
  (global-flycheck-mode +1))


(use-package prog-mode
  :preface
  (declare-function auto-complete-mode 'auto-complete)
  :init
  (defun >>=init-prog-mode ()
    "Init `prog-mode' based modes."
    (when (>>=local-buffer)
      (auto-complete-mode +1)
      (flyspell-prog-mode))
    (>>=init-text-mode)
    (turn-on-auto-fill)
    (subword-mode))
  :hook
  (prog-mode . >>=init-prog-mode))


(use-package conf-mode
  :after prog-mode
  :hook
  (conf-mode . >>=init-prog-mode))



;;; Emacs Lisp

(use-package lisp-mode
  :custom
  (emacs-lisp-docstring-fill-column 78)
  (lisp-indent-offset 2)
  ;; TODO: Conflict with 'pyls'
  (create-lockfiles nil))



;;; Python

(defvar >>=|blacken/enable t
  "Determines if `blacken' is enabled when entering `python-mode'.
Possible values are any of the two canonical boolean values, or the symbol
'ask', in which case the first time entering the mode the user will be asked.
You always can manually enable this mode using `>>=blacken/turn-on' or
`blacken-mode'.")


(defconst >>-!python/env-locators
  '(
     "venv"
     ".venv"
     ("poetry.lock" "poetry" "env" "info" "-p")
     ("Pipfile.lock" "pipenv" "--venv")
     (".python-version" "pyenv" "prefix")
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
      (file-executable-p (>>=dir-join path "bin" "python"))
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
      ;; lsp-pyrigth does its own lookup with the function
      ;; lsp-pyright-locate-venv; so we don't need to do anything here for it.
      ;; TODO: See other options:
      ;; https://github.com/emacs-lsp/lsp-python-ms/blob/master/lsp-python-ms.el
      ;; - lsp-python-ms--dominating-{pyenv|asdf|venv|conda}-python,
      ;; - lsp-python-ms-python-executable-cmd
      ))

  (defun -inferior-python-setup()
    ;; (setq-default indent-tabs-mode nil)
    (linum-mode 0))
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
  :preface
  (declare-function blacken-mode 'blacken)
  (declare-function blacken-buffer 'blacken)

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
    (>>=major-mode-trigger blacken >>=|blacken/enable >>=blacken/turn-on))

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



;;; Language Server Protocol

(require 'dash)
(require 's)
(require 'cl-lib)


(defvar >>=|lsp/enable-mode t
  "Determine when to enable `lsp' when entering a programming mode.
Language server activation is based on the logic of the `>>=check-major-mode'
function.  Value t is translated to use `>>-lsp-buffer?' function.")


;; Next configuration allows `company-mode' to be managed internally by
;; `lsp-mode'.  Other possible choices are to activate it globally, or by
;; using `prog-mode-hook'.
(use-package company
  :ensure t)


(use-package lsp-mode
  :ensure t
  :demand t
  :preface
  (declare-function lsp 'lsp-mode)

  (defun >>-lsp-buffer? ()
    "Validate current buffer language for `lsp-language-id-configuration'."
    ;; TODO: refactor this, this code was copied from `lsp-buffer-language'
    ;; but skipping the warning.
    (when-let ((fn (buffer-file-name)))
      (->> lsp-language-id-configuration
        (-first
          (-lambda ((mode-or-pattern . language))
            (cond
              ((and (stringp mode-or-pattern)
                 (s-matches? mode-or-pattern fn))
                language)
              ((eq mode-or-pattern major-mode)
                language))))
        cl-rest)))

  (defun >>-lsp/may-enable-server ()
    "Determine whether `lsp' may be enabled (see `>>=|lsp/enable-mode')."
    (>>=major-mode-trigger
      lsp
      (if (eq >>=|lsp/enable-mode t)
        '>>-lsp-buffer?
        >>=|lsp/enable-mode)
      lsp +1))
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
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil))
  )



;;; Debug Adapter Protocol

(defvar >>=|dap/enable t
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



;;; Javascript, CoffeeScript and LiveScript

(use-package tern
  ;; `tern' program must be installed in your system
  :ensure t
  :config
  (add-hook 'js2-mode-hook #'tern-mode))


(use-package tern-auto-complete
  :ensure t
  :preface
  (declare-function tern-ac-setup 'tern-auto-complete)
  :after tern
  :config
  (tern-ac-setup))


(use-package prettier
  ;; `prettier' program must be installed in your system
  :ensure t
  :init
  (defvar >>=|prettier/enable-mode t
    "Configure when to enable `prettier-mode'.")

  (defun >>-prettier-mode? ()
    "Enable `prettier-mode' depending on `>>=|prettier/enable-mode' variable."
    (when (fboundp 'prettier-mode)
      (funcall 'prettier-mode (if >>=|prettier/enable-mode +1 -1)))))


(use-package js2-mode
  :ensure t
  :after (tern-auto-complete prettier)
  :mode ("\\.js\\'" "\\.pac\\'" "node")
  :hook
  (js-mode . tern-mode)
  (js-mode . >>-prettier-mode?)
  :custom
  (js-indent-level 2)
  :config
  (progn
    ;; TODO: What about to add also all interpreters currently using `js-mode'
    ;;       ("rhino", "gjs", and "nodejs")
    (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
    (tern-ac-setup)))


(use-package json-mode
  ;; TODO: Requires npm package `json-ls' (JSON Language Server)
  :mode "\\.json\\'"
  :ensure t
  :after prettier
  :requires (flycheck)
  :hook
  (json-mode .
    (lambda ()
      (setq flycheck-checker 'json-jsonlint)))
  (json-mode . >>-prettier-mode?))



;;; C/C++ Mode -- Linux kernel programming

(use-package cc-mode
  :ensure nil
  :bind
  (:map c-mode-base-map
    ("C-c c" . compile))
  :hook
  (c-mode-common .
    (lambda ()
      (c-set-style "linux")
      (setq tab-width 4)
      (setq c-basic-offset 4))))



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
