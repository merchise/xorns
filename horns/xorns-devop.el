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

(define-obsolete-variable-alias '>>=|devops/enable
  '>>=|devops/features "0.11.1"
  "These two variables were redundant definitions.")
(defvar >>=|devops/features '(docker)
  "Enable specific features of DevOps.")



;;; Docker
(use-package docker
  :when (memq 'docker >>=|devops/features)
  :ensure t
  :bind ("C-c d" . docker)) ;; TODO: customisable map.


(use-package dockerfile-mode
  :when (memq 'docker >>=|devops/features)
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.docker\\'" . dockerfile-mode)))


(use-package docker-compose-mode
  :when (memq 'docker >>=|devops/features)
  :ensure t)


(use-package lsp-docker
  :when (memq 'docker >>=|devops/features)
  :ensure t)



;;; Caddy

(use-package caddyfile-mode
  :when (memq 'caddy >>=|devops/features)
  :ensure t)



;;; Nginx

(use-package nginx-mode
  :when (memq 'nginx >>=|devops/features)
  :ensure t)


(provide 'xorns-devop)
;;; xorns-devop.el ends here
