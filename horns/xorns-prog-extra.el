;;; xorns-prog-extra.el --- Programming source code editing (extra packages)

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


(defvar >>-|programming/config-toml nil
  "Configure TOML dependency.")



;;; Java, Scala, ...

(use-package javadoc-lookup
  :when (memq 'java >>=|programming/extra-languages)
  :ensure t
  :bind
  ("C-h j" . javadoc-lookup))



;;; Rust

(when (memq 'rust >>=|programming/extra-languages)
  (setq >>-|programming/config-toml t)
  (use-package rust-mode
    :ensure t
    :defer t)
  (use-package flycheck-rust
    :ensure t
    :defer t
    :after rust-mode
    :hook
    (flycheck-mode . flycheck-rust-setup))
  (use-package racer
    :ensure t
    :defer t
    :hook
    (rust-mode . racer-mode))
  (use-package cargo
    :ensure t
    :bind
    (:map rust-mode-map ("<f5>" . cargo-process-build))
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
  :when >>-|programming/config-toml
  :ensure t
  :defer t)


(provide 'xorns-prog-extra)
;;; xorns-prog-extra.el ends here
