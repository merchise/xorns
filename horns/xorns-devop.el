;;; xorns-devop.el --- DevOps  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;;; Commentary:

;; DevOps definitions

;; Enjoy!


;;; Code:

(eval-and-compile
  (require 'use-package))


;;; Configuration

(defvar >>=|devops/enable t
  "Enable devops-related features: docker, compose, etc.")

(defvar >>=|devops/features '(docker)
  "Enable specific features of DevOps.")



;;; Docker
(use-package docker
  :when (and >>=|devops/enable (memq 'docker >>=|devops/features)
  :ensure t
  :bind ("C-c d" . docker)) ;; TODO: customisable map.


(use-package dockerfile-mode
  :when (and >>=|devops/enable (memq 'docker >>=|devops/features)
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.docker\\'" . dockerfile-mode)))


(use-package docker-compose-mode
  :when (and >>=|devops/enable (memq 'docker >>=|devops/features)
  :ensure t)

(use-package lsp-docker
  :ensure t
  :when (and >>=|devops/enable (memq 'docker >>=|devops/features))


;;; Caddy

(use-package caddyfile-mode
  :when (and >>=|devops/enable (memq 'caddy >>=|devops/features)
  :ensure t)



;;; Nginx

(use-package nginx-mode
  :when (and >>=|devops/enable (memq 'nginx >>=|devops/features)
  :ensure t)


(provide 'xorns-devop)
;;; xorns-devop.el ends here
