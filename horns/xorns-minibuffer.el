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
(require 'xorns-bindings)


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




(use-package helm
  :when (eq >>=|minibuffer/completing-framework 'helm)
  :ensure t
  :bind
  (:map helm-map
    ("<C-M-left>" . helm-previous-source)
    ("<C-M-right>" . helm-next-source))
  :config
  (progn
    (>>=remap "M-x" helm-M-x "M-X")
    (helm-mode +1)))


(use-package helm-config
  :when (eq >>=|minibuffer/completing-framework 'helm)
  :ensure helm
  :demand t
  :preface
  (progn
    (defun >>=helm/multi (&optional arg)
      "Use `helm-mini' if nil, otherwise call `helm-multi-files'."
      (interactive "P")
      (if (null arg)
	(helm-mini)
	;; else
	(helm-multi-files)))
    )
  :bind
  (("M-Y" . helm-show-kill-ring)
   ("C-h SPC" . helm-all-mark-rings)
   ("C-x C-f" . helm-find-files)
   ("C-x b" . >>=helm/multi)
   :map minibuffer-local-map
   ("C-c C-l" . helm-minibuffer-history)
   :map helm-command-map
   ("o" . helm-occur))
  :config
  (progn
    ;; Note: Do not remove `:demand' option.  "C-x c" is very similar to
    ;; `kill-emacs' ("C-x C-c"), so we use "C-c h".  Global method is used
    ;; because `helm-command-prefix-key' do not work if changed after
    ;; `helm-config' is loaded
    (global-set-key (kbd "C-c h") 'helm-command-prefix)
    (global-unset-key (kbd "C-x c"))))


(use-package swiper-helm
  :when (eq >>=|minibuffer/completing-framework 'helm)
  :ensure t
  :config
  (progn
    (>>=remap "C-s" swiper-helm "C-S-s")))


(provide 'xorns-minibuffer)
;;; xorns-minibuffer.el ends here
