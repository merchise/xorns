;;; xorns-minibuffer.el --- Mini-buffer completion

;; Copyright (c) Merchise Autrement [~º/~]

;;; Commentary:

;; You can configure `>>=|minibuffer/completing-framework' variable to decide
;; which input completing framework is configured; when nil, just builtin
;; packages (like `ido') are configured; other possible options are `ido+',
;; `ivy', or `helm'.
;;
;; The package `ido' is part of Emacs, to configure extra related packages
;; (like `ido-completing-read+', will be configured), `ido+' must be used.

;; Enjoy!


;;; Code:

(require 'use-package)
(require 'xorns-tools)


(defvar >>=|minibuffer/completing-framework nil
  "Kind of mini-buffer input completing framework.")


(use-package minibuffer
  :bind
  ("C-M-," . completion-at-point))




(use-package ido
  :when
  (let ((cf >>=|minibuffer/completing-framework))
    (or (null cf) (eq cf 'ido+)))
  :custom
  (ido-auto-merge-work-directories-length -1)
  (ido-auto-merge-delay-time 1.5)
  (ido-use-filename-at-point t)
  :config
  (progn
    (ido-everywhere +1)
    (ido-mode +1)))


(use-package ido-completing-read+
  :when (eq >>=|minibuffer/completing-framework 'ido+)
  :ensure t
  :config
  (ido-ubiquitous-mode +1))




(use-package ivy
  :when (eq >>=|minibuffer/completing-framework 'ivy)
  :ensure t
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-fixed-height-minibuffer t)
  :config
  (progn
    (ivy-mode +1)))


(use-package swiper
  :when (eq >>=|minibuffer/completing-framework 'ivy)
  :ensure t
  :bind
  ([remap isearch-forward] . swiper))


(use-package counsel
  :when (eq >>=|minibuffer/completing-framework 'ivy)
  :ensure t
  :config
  (progn
    (counsel-mode +1)))


(provide 'xorns-minibuffer)
;;; xorns-minibuffer.el ends here
