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
