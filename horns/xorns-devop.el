;;; xorns-devop.el --- DevOps  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;;; Commentary:

;; DevOps definitions

;; Enjoy!


;;; Code:

(eval-and-compile
  (require 'use-package))


;;; Docker

(defvar >>=|devops/enable t
  "Enable devops-related features: docker, compose, etc.")

(use-package docker
  :when >>=|devops/enable
  :bind ("C-c d" . docker)) ;; TODO: customisable map.


(use-package dockerfile-mode
  :when >>=|devops/enable
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.docker\\'" . dockerfile-mode)))


(use-package docker-compose-mode
  :when >>=|devops/enable
  :ensure t)

(use-package lsp-docker
  :ensure t
  :when >>=|devops/enable)


(provide 'xorns-devop)
;;; xorns-devop.el ends here
