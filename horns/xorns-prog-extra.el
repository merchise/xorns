;;; xorns-prog-extra.el --- Programming source code editing (extra packages)  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;;; Commentary:

;; Extra definitions for editing programming language source code.

;; Enjoy!


;;; Code:

(require 'use-package)
(require 'xorns-tools)
(require 'xorns-project)


(defvar >>=|programming/extra-languages nil
  "Extra Programming Languages to configure.
For example (rust haskell java scala R).")


(defvar >>=|programming/extra-features nil
  "Extra Programming Features to configure.
For example (toml classic-snippets).")



;;; Java, Scala, ...

(use-package javadoc-lookup
  :when (memq 'java >>=|programming/extra-languages)
  :ensure t
  :bind
  ("C-h j" . javadoc-lookup))



;;; Rust

(when (memq 'rust >>=|programming/extra-languages)
  (add-to-list '>>=|programming/extra-features 'toml)
  (use-package rust-mode
    :ensure t
    :commands rust-mode)
  (use-package flycheck-rust
    :ensure t
    :defer t
    :after rust-mode
    :hook
    (flycheck-mode . flycheck-rust-setup))
  (use-package cargo
    :ensure t
    :bind
    (:map rust-mode-map
      ("<f5>" . cargo-process-build))
    :hook
    (rust-mode . cargo-minor-mode)))



;;; Haskell

(when (memq 'haskell >>=|programming/extra-languages)
  (use-package lsp-haskell
    :ensure t
    :defer t)
  (use-package flycheck-haskell
    :ensure t
    :commands flycheck-haskell-configure
    :hook
    (flycheck-mode . flycheck-haskell-configure))
  (use-package haskell-snippets
    :ensure t
    :defer t
    :after yasnippet))



;;; ReScript

(use-package rescript-mode
  :when (memq 'rescript >>=|programming/extra-languages)
  :ensure t
  :mode ("\\.res\\'")
  :config
  (>>=projectile/add-root-files
    "bsconfig.json"    ; TODO: obsolete after ReScript version 11
    "rescript.json"))


(use-package lsp-rescript
  :when (memq 'rescript >>=|programming/extra-languages)
  :ensure t
  :after lsp-mode rescript-mode
  :demand t    ;; TODO: WTF, without this the package is not loaded, weird.
  :hook
  (before-save . >>=lsp/safe-format-buffer)
  :config
  ;; TODO: check the nvm package below to see if can guess the right node
  ;; version and find the 'rescript-language-server' there.
  (>>=set-custom-value?
    lsp-rescript-server-command
    '("rescript-language-server" "--stdio")))



;;; Extra dependencies

(use-package toml-mode
  :when (memq 'toml >>=|programming/extra-features)
  :ensure t
  :defer t)


(use-package yasnippet-classic-snippets
  :when (memq 'classic-snippets >>=|programming/extra-features)
  :ensure t)


(use-package envrc
  :when (memq 'envrc >>=|programming/extra-features)
  :commands envrc-global-mode
  :ensure t
  :config
  (envrc-global-mode t))



;;; Gitlab

(when (memq 'gitlab >>=|programming/extra-features)
  (use-package gitlab-ci-mode
    :ensure t
    :defer t)


  (use-package gitlab-ci-mode-flycheck
    :ensure t
    :defer t))



;;; Jinja2

(use-package jinja2-mode
  :when (memq 'jinja2 >>=|programming/extra-languages)
  :ensure t)


;;; nvm

(use-package nvm
  :when (memq 'nvm >>=|programming/extra-features)
  :ensure t)


;;; Odoo tools

(eval-and-compile
  (defconst odoo--compilation-error-rx
    (rx-to-string
      '(seq
         "ERROR"
         (* any)
         "`"
         (* (or space control))
         "File \""
         (group (group (* (not "\"")))
           "\","
           (* space)
           "line"
           (* space)
           (group (+ digit)))))))

(when (memq 'odoo >>=|programming/extra-features)
  (add-to-list 'compilation-error-regexp-alist 'odoo-traceback-error)
  (add-to-list
    'compilation-error-regexp-alist-alist
    (cons 'odoo-traceback-error (cons odoo--compilation-error-rx '(2 3 4 2 1)))))


(provide 'xorns-prog-extra)
;;; xorns-prog-extra.el ends here
