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

(eval-when-compile
  (require 'xorns-term)
  (require 'use-package))



;;; Common Systems

(use-package auto-complete
  :ensure t
  :custom
  (ac-trigger-key "TAB")
  :config
  (progn
    (ac-flyspell-workaround)))


(use-package yasnippet
  :ensure t
  :preface
  (progn
    (defun >>=snippets/initialize ()
      "Initialize `xorns' snippets."
      (let* ((lib-dir (bound-and-true-p >>=!library-directory))
	     (snip-dir (expand-file-name "snippets" lib-dir)))
	(if (file-exists-p snip-dir)
	  (progn
	    (add-to-list 'yas-snippet-dirs snip-dir t)
	    (yas-load-directory snip-dir))
	  ;; else
	  (message ">>= snippets directory '%s' does not exist." snip-dir)))))
  :config
  (progn
    (yas-global-mode +1)
    (>>=snippets/initialize)))


(use-package flycheck
  :ensure t
  :functions global-flycheck-mode
  :custom
  (flycheck-idle-change-delay 10)
  :config
  (global-flycheck-mode +1))


(use-package prog-mode
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
  :init
  (defun -inferior-python-setup()
    (setq-default indent-tabs-mode nil)
    (linum-mode 0))
  :bind
  (:map python-mode-map
    ("C-m" . newline-and-indent))
  :hook
  ((python-mode . outline-minor-mode)
   (inferior-python-mode . -inferior-python-setup)))


(>>=define-terminal python
  :program "ipython" "python"
  :paster ("ipython" . "%paste")
  :mode python)


(use-package blacken
  :ensure t
  :preface
  (progn
    (defun >>=blacken/turn-on ()
      "Setup `blacken' inner a file in `python-mode'."
      (interactive)
      (turn-off-auto-fill)
      (blacken-mode))

    (defun >>-blacken/may-enable-mode ()
      "Determine whether `blacken' may be enabled (see `>>=|blacken/enable')."
      (>>=major-mode-trigger blacken >>=|blacken/enable >>=blacken/turn-on))
    )
  :hook
  (python-mode . >>-blacken/may-enable-mode))


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

(defvar >>=|lsp/enable-mode t
  "Determine when to enable `lsp' when entering a programming mode.
Language server activation is based on the logic of the `>>=check-major-mode'
function.  The possible values of this variable are the same as the CRITERIA
argument, with the exception of the symbol 'ask' which is replaced by `lsp' to
be used as a semantic identity in this case.")


(use-package lsp-mode
  :ensure t
  :commands lsp
  :preface
  (defun >>-lsp/may-enable-server ()
    "Determine whether `lsp' may be enabled (see `>>=|lsp/enable-mode')."
     (>>=major-mode-trigger lsp >>=|lsp/enable-mode lsp +1))
  :custom
  (lsp-auto-guess-root t)
  (lsp-keymap-prefix "C-s-l")
  :hook
  (prog-mode . >>-lsp/may-enable-server))


(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :hook
  (lsp-mode . lsp-ui-mode)
  :config
  (progn
    (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
      (setq mode-line-format nil))))


(use-package company-lsp
  :ensure t
  :after lsp-mode
  :commands company-lsp)



;;; Javascript, CoffeeScript and LiveScript

(use-package tern
  ;; This requires you have the `tern' program installed in your system and in
  ;; the exec-path.
  :ensure t
  :config
  (add-hook 'js2-mode-hook #'tern-mode))


(use-package tern-auto-complete
  :ensure t
  :after tern
  :config
  (tern-ac-setup))


(use-package js2-mode
  :ensure t
  :after tern-auto-complete
  :mode ("\\.js\\'" "\\.pac\\'" "node")
  :hook
  (js-mode . tern-mode)
  :custom
  (js-indent-level 2)
  :config
  (progn
    ;; TODO: What about to add also all interpreters currently using `js-mode'
    ;;       ("rhino", "gjs", and "nodejs")
    (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
    (require 'tern-auto-complete)
    (tern-ac-setup)))


(use-package json-mode
  ;; TODO: Requires npm package `json-ls' (JSON Language Server)
  :mode "\\.json\\'"
  :ensure t
  :requires (flycheck)
  :hook
  (json-mode .
    (lambda ()
      (setq flycheck-checker 'json-jsonlint))))



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
