;;; xorns-prog.el --- Programming language source code editing

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;;; Commentary:

;; Generic definitions for editing programming language source code.
;;
;; todo: http://company-mode.github.io/
;;       company-tern

;; Enjoy!


;;; Code:

(require 'xorns-text)
(require 'xorns-utils)
(require 'xorns-buffers)
(require 'xorns-simple)

(require 'use-package)
(require 'xorns-packages)


(defgroup xorns-prog nil
  "Programming configurations for `xorns'."
  :prefix "xorns-prog-"
  :group 'xorns
  :group 'programming)



;;; Common Systems

(>>=ensure-packages auto-complete yasnippet flycheck)

(use-package auto-complete
  :custom
  (ac-trigger-key "TAB")
  :config
  (progn
    (ac-flyspell-workaround)))


(use-package yasnippet
  :config
  (yas-global-mode 1))


(use-package flycheck
  :functions global-flycheck-mode
  :custom
  (flycheck-idle-change-delay 10)
  :config
  (global-flycheck-mode t))


(use-package prog-mode
  :init
  (defun >>=init-prog-mode ()
    "Init `prog-mode' based modes."
    (when (>>=local-buffer)
      (auto-complete-mode t)
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

(>>=ensure-packages
  lsp-mode lsp-ui company-lsp pipenv blacken)


(defgroup xorns-python nil
  "Programming configurations for `xorns'."
  :prefix "xorns-python-"
  :group 'xorns-prog
  :group 'python)


(defcustom xorns-python-pyls-arguments nil
  "Extra arguments for the python language server."
  :group 'xorns-python
  :type '(repeat string))


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


(use-package lsp-mode
  :commands lsp
  :custom
  (lsp-auto-guess-root t)
  :hook
  (prog-mode . lsp))


(use-package lsp-ui
  :commands lsp-ui-mode
  :init
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  :custom
  (lsp-prefer-flymake nil))


(use-package company-lsp
  :after lsp-mode
  :commands company-lsp)


(use-package pipenv
  :preface
  (declare-function pipenv-projectile-after-switch-extended 'pipenv)
  :hook (python-mode . pipenv-mode)
  :custom
  (pipenv-projectile-after-switch-function
    #'pipenv-projectile-after-switch-extended))


(use-package blacken
  :init
  (defun >>-blacken-setup ()
    (turn-off-auto-fill)
    (blacken-mode))
  :hook
  (python-mode . >>-blacken-setup)
  ;; :custom
  ;; (blacken-line-length 'fill)
  )



;;; Javascript, CoffeeScript and LiveScript

(>>=ensure-packages tern tern-auto-complete js2-mode)

(use-package tern
  :config
  (add-hook 'js2-mode-hook #'tern-mode))


(use-package tern-auto-complete
  :after tern
  :config
  (tern-ac-setup))


;; This requires you have the tern program installed in your system and in the
;; exec-path.

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))



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



;;; Java, Scala, ...

(defvar >>=|programming/extra-languages nil
  ;; todo: pending task for future release
  "Extra Programming Languages to configure; for example (java scala R).")


(when (memq 'java >>=|programming/extra-languages)
  (>>=ensure-packages javadoc-lookup)
  (use-package javadoc-lookup
    :bind
    ("C-h j" . javadoc-lookup)))


(provide 'xorns-prog)
;;; xorns-prog.el ends here
