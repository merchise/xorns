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

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))


(use-package dockerfile-mode
  :ensure t)


(use-package docker-compose-mode
  :ensure t)


(provide 'xorns-devop)
;;; xorns-devop.el ends here
