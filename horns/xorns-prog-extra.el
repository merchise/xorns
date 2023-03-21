;;; xorns-prog-extra.el --- Programming source code editing (extra packages)  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;;; Commentary:

;; Extra definitions for editing programming language source code.

;; Enjoy!


;;; Code:

(require 'use-package)


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
    :defer t)
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

(defvar >>=|rescript-compilation-error-rx
  (rx-to-string
    '(seq
       (or
         "We've found a bug for you!"
         "Syntax error!"
         (seq
           "Warning number"
           (* space)
           (+ digit)
           (* space)
           "(configured as error)"))
       (* (or space control))
       line-start
       (* space)
       (group (group (seq (* any) ".res"))
         ":"
         (group (+ digit))
         ":"
         (group (+ digit))
         (? "-" (+ digit) (? ":" (+ digit))))
       line-end))
  "The regular expression for detecting compilation errors in rescript.")


(defvar >>=|rescript-compilation-warning-rx
  (rx-to-string
    '(seq
       "Warning number"
       (+ space)
       (+ digit)
       (* (or space control))
       line-start
       (* space)
       (group (group (seq (* any) ".res"))
         ":"
         (group (+ digit))
         ":"
         (group (+ digit))
         (? "-" (+ digit)))
       line-end))
  "The regular expression for detecting compilation warnings in rescript.")


(when (memq 'rescript >>=|programming/extra-languages)
  (use-package rescript-mode
    :mode ("\\.res\\'")
    :config
    (add-to-list 'compilation-error-regexp-alist '>>-rescript-error)
    (add-to-list 'compilation-error-regexp-alist '>>-rescript-warning)
    (add-to-list
      'compilation-error-regexp-alist-alist
      (cons '>>-rescript-error (cons >>=|rescript-compilation-error-rx '(1 2 3 2 1))))
    (add-to-list
      'compilation-error-regexp-alist-alist
      (cons '>>-rescript-warning (cons >>=|rescript-compilation-warning-rx '(1 2 3 1 1)))))

  (use-package lsp-rescript
    :config
    (add-hook 'rescript-mode-hook 'lsp-deferred)
    (add-hook 'before-save-hook 'lsp-format-buffer)))



;;; Extra dependencies

(use-package toml-mode
  :when (memq 'toml >>=|programming/extra-features)
  :ensure t
  :defer t)


(use-package yasnippet-classic-snippets
  :when (memq 'classic-snippets >>=|programming/extra-features)
  :ensure t)



;;; Gitlab

(when (memq 'gitlab >>=|programming/extra-features)
  (use-package gitlab-ci-mode
    :ensure t
    :defer t)

  (use-package gitlab-ci-mode-flycheck
    :ensure t
    :defer t))




(provide 'xorns-prog-extra)
;;; xorns-prog-extra.el ends here
