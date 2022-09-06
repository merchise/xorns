;;; xorns-prog.el --- Programming language source code editing  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;;; Commentary:

;; Generic definitions for editing programming language source code.

;; Enjoy!


;;; Code:

(require 'xorns-text)
(require 'xorns-tools)
(require 'xorns-buffers)
(require 'xorns-simple)

(eval-and-compile
  (require 'hideshow)
  (require 'xorns-term)
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
    (let* ((lib-dir (bound-and-true-p >>=!library-directory))
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


(autoload 'po-mode "po-mode"    ; TODO: Check this
  "Major mode for translators to edit PO files" t)


(use-package python
  :defer t
  :preface

  (defun >>-compute-local-venv (root)
    (let ((local-venv (>>=dir-join root ".venv")))
      (when (file-exists-p local-venv) local-venv)))

  (defun >>-compute-pipfile-env (root)
    (let ((pipfile-lock-fname (expand-file-name "Pipfile.lock" root)))
      (when (file-exists-p pipfile-lock-fname)
        (condition-case nil
          (car-safe (process-lines (>>=executable-find "pipenv") "--venv"))))))

  (defun >>-compute-poetry-env (root)
    (let ((poetry-lock-fname (expand-file-name "poetry.lock" root)))
      (when (file-exists-p poetry-lock-fname)
        (condition-case nil
          (car-safe (process-lines (>>=executable-find "poetry") "env" "info" "-p"))))))

  (defun >>-get-venv-path()
    "Get the virtual the environment's path, nil if none."
    (when-let ((root (>>=project-root)))
      (or
        (>>-compute-local-venv root)
        (>>-compute-pipfile-env root)
        (>>-compute-poetry-env root))))

  (defun -python-mode-setup()
    (outline-minor-mode)
    (when-let ((venv-path (>>-get-venv-path)))
      (message "Found venv path '%s'" venv-path)
      (when (boundp 'lsp-pylsp-plugins-jedi-environment)
        (progn
          (message "Setting '%s' in pylsp" venv-path)
          (>>=set 'lsp-pylsp-plugins-jedi-environment venv-path)))
      (when (boundp 'lsp-pyls-plugins-jedi-environment)
        (progn
          (message "Setting '%s' in pyls" venv-path)
          (>>=set 'lsp-pyls-plugins-jedi-environment venv-path)))
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
  ((python-mode . -python-mode-setup)
    (inferior-python-mode . -inferior-python-setup)))


(use-package blacken
  :ensure t
  :preface
  (declare-function blacken-mode 'blacken)

  (defun >>=blacken/turn-on ()
    "Setup `blacken' inner a file in `python-mode'."
    (interactive)
    (turn-off-auto-fill)
    (blacken-mode))

  (defun >>-blacken/may-enable-mode ()
    "Determine whether `blacken' may be enabled (see `>>=|blacken/enable')."
    (>>=major-mode-trigger blacken >>=|blacken/enable >>=blacken/turn-on))
  :hook
  (python-mode . >>-blacken/may-enable-mode)
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
  :ensure t)


(use-package js2-mode
  :ensure t
  :after (tern-auto-complete prettier)
  :mode ("\\.js\\'" "\\.pac\\'" "node")
  :hook
  (js-mode . tern-mode)
  (js-mode . prettier-mode)
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
  :requires (flycheck)
  :hook
  (json-mode .
    (lambda ()
      (setq flycheck-checker 'json-jsonlint)))
  (json-mode . prettier-mode))



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
