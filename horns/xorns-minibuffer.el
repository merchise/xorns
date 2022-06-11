;;; xorns-minibuffer.el --- Mini-buffer completion  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;;; Commentary:

;; The input completing framework to use can be configured with the variable
;; `>>=|minibuffer/completing-framework'; when nil, builtin package `ido' in
;; its standard mode is configured; `ido+' will configure standard `ido' plus
;; some extra related packages (`ido-everywhere' and `ido-completing-read+');
;; `ivy' and `helm' are the other two possibilities.

;; Enjoy!


;;; Code:

(eval-and-compile
  (require 'xorns-setup)
  (require 'helm-mode nil 'noerror)
  (require 'helm-for-files nil 'noerror)
  (require 'helm-buffers nil 'noerror)
  (require 'vertico nil 'noerror)
  (require 'counsel nil 'noerror)
  (require 'consult-xref nil 'noerror)
  (require 'consult-register nil 'noerror)
  (require 'ivy nil 'noerror)
  (require 'ido nil 'noerror)
  (require 'ido-completing-read+ nil 'noerror))

(require 'xorns-bindings)



(defvar >>=|minibuffer/completing-framework nil
  "Kind of mini-buffer input completing framework.
Possible values are `ido+', `ivy', `vertico', and `helm'.")


(defvar >>=|minibuffer/configure-savehist t
  "If the mini-buffer history should be saved between Emacs sessions.
Always considered true when `>>=|minibuffer/completing-framework' is
`vertico'.")


(use-package minibuffer
  :bind
  ("C-M-," . completion-at-point))


(use-package savehist
  :preface
  (defsubst >>-configure-savehist? ()
    (or
      >>=|minibuffer/configure-savehist
      (eq >>=|minibuffer/completing-framework 'vertico)))
  :when (>>-configure-savehist?)
  :init
  (savehist-mode +1))




(use-package ido
  :when
  (let ((cf >>=|minibuffer/completing-framework))
    (or (null cf) (eq cf 'ido+)))
  :custom
  (ido-auto-merge-work-directories-length -1)
  (ido-auto-merge-delay-time 1.5)
  :config
  (when (eq >>=|minibuffer/completing-framework 'ido+)
    (ido-everywhere +1))
  (ido-mode +1))


(use-package ido-completing-read+
  :when (eq >>=|minibuffer/completing-framework 'ido+)
  :ensure t
  :config
  (ido-ubiquitous-mode +1))



(use-package swiper
  :when (eq >>=|minibuffer/completing-framework 'ivy)
  :ensure t
  :bind
  ([remap isearch-forward] . swiper))


(use-package counsel
  :when (eq >>=|minibuffer/completing-framework 'ivy)
  :ensure t
  :demand t
  :preface
  (defun >>-counsel-yank-pop (&optional arg)
    "xorns replacement for `counsel-yank-pop' (ARG is used as in original)."
    (interactive "*p")
    (if (active-minibuffer-window)
      (yank-pop arg)
      ;; else
      (counsel-yank-pop arg)))
  :custom
  (counsel-find-file-at-point t)
  :bind
  ;; ("C-x d" . counsel-dired)
  ([remap recentf-open-files] . counsel-recentf)
  ("M-y" . >>-counsel-yank-pop)
  :config
  (counsel-mode +1))




(use-package ivy
  :when (eq >>=|minibuffer/completing-framework 'ivy)
  :ensure t
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-fixed-height-minibuffer t)
  (ivy-use-virtual-buffers t)
  (ivy-virtual-abbreviate nil)    ; 'abbreviate is also nice
  :config
  (ivy-mode +1))




(use-package consult
  :ensure t
  :when (eq >>=|minibuffer/completing-framework 'vertico)
  :bind
  ;; C-c bindings (mode-specific-map)
  ("C-c h" . consult-history)
  ("C-c m" . consult-mode-command)
  ("C-c b" . consult-bookmark)
  ;; C-x bindings (ctl-x-map)
  ([remap repeat-complex-command] . consult-complex-command)
  ("C-x b" . consult-buffer)
  ("C-x 4 b" . consult-buffer-other-window)
  ("C-x 5 b" . consult-buffer-other-frame)
  ;; Other custom bindings
  ([remap yank-pop] . consult-yank-pop)
  ([remap apropos-command] . consult-apropos)
  ;; M-g bindings (goto-map)
  ([remap goto-line] . consult-goto-line)
  ("M-g i" . consult-imenu)
  ("M-g I" . consult-imenu-multi)
  :hook
  (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq
    register-preview-delay 0
    register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq
    xref-show-xrefs-function #'consult-xref
    xref-show-definitions-function #'consult-xref)
  :config
  (with-no-warnings
    (consult-customize
      consult-theme
      :preview-key '(:debounce 0.2 any)
      consult-ripgrep    ;;
      consult-git-grep
      consult-grep
      consult-bookmark
      consult-recent-file
      consult-xref
      consult--source-bookmark
      consult--source-recent-file
      consult--source-project-recent-file
      :preview-key (kbd "M-.")))
  (setq
    consult-narrow-key "<"
    consult-project-function #'>>=project-root)
  )


(use-package vertico
  :when (eq >>=|minibuffer/completing-framework 'vertico)
  :ensure t
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode +1)
  )




(use-package helm
  :when (eq >>=|minibuffer/completing-framework 'helm)
  :ensure t
  :bind
  (:map helm-map
    ("<C-M-left>" . helm-previous-source)
    ("<C-M-right>" . helm-next-source))
  :config
  (>>=remap "M-x" helm-M-x "M-X")
  (helm-mode +1))


(use-package helm-config
  :when (eq >>=|minibuffer/completing-framework 'helm)
  :ensure helm
  :demand t
  :preface
  (defun >>=helm/multi (&optional arg)
    "Use `helm-mini' if nil, otherwise call `helm-multi-files'."
    (interactive "P")
    (if (null arg)
      (helm-mini)
      ;; else
      (helm-multi-files)))
  :bind    ; TODO: Check this configuration
  (("M-Y" . helm-show-kill-ring)
   ("C-h SPC" . helm-all-mark-rings)
   ("C-x C-f" . helm-find-files)
   ("C-x b" . >>=helm/multi)
   :map minibuffer-local-map
   ("C-c C-l" . helm-minibuffer-history))
  :config
  ;; Note: Do not remove `:demand' option.  "C-x c" is very similar to
  ;; `kill-emacs' ("C-x C-c"), so we use "C-c h".  Global method is used
  ;; because `helm-command-prefix-key' do not work if changed after
  ;; `helm-config' is loaded
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c")))


(use-package swiper-helm
  :when (eq >>=|minibuffer/completing-framework 'helm)
  :ensure t
  :config
  (>>=remap "C-s" swiper-helm "C-S-s"))


(provide 'xorns-minibuffer)
;;; xorns-minibuffer.el ends here
