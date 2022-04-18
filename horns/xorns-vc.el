;;; xorns-vc.el --- Version Control Integration  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;;; Commentary:

;; Configure Version Control Systems, very focused in `magit' for GIT.

;; Enjoy!


;;; Code:

(require 'xorns-text)
(require 'use-package)


(use-package git-modes
  :ensure t
  :defer t)


(use-package magit
  :ensure t
  :bind
  (("C-x g" . magit-status)
   ("C-c g c" . magit-clone)
   ("C-c g s" . magit-status)
   ("C-c g b" . magit-blame)
   ("C-c g l" . magit-log-buffer-file)
   ("C-c g p" . magit-pull))
  :hook
  (git-commit-mode . >>=tex-mode-setup)
  :config
  (put 'magit-clean 'disabled nil))


(provide 'xorns-vc)
;;; xorns-vc.el ends here
