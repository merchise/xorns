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

(require 'xorns-tools)


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
  :commands ido-everywhere
  :custom
  (ido-auto-merge-work-directories-length -1)
  (ido-auto-merge-delay-time 1.5)
  :config
  (when (eq >>=|minibuffer/completing-framework 'ido+)
    (ido-everywhere +1))
  (ido-mode +1))


(use-package ido-completing-read+
  :when (eq >>=|minibuffer/completing-framework 'ido+)
  :commands ido-ubiquitous-mode
  :ensure t
  :config
  (ido-ubiquitous-mode +1))




(use-package marginalia
  :when >>=|minibuffer/completing-framework
  :ensure t
  :preface
  (eval-when-compile
    (declare-function marginalia-mode 'marginalia))
  :config
  (marginalia-mode))


(use-package embark
  :when >>=|minibuffer/completing-framework
  :ensure t
  :preface
  (eval-when-compile
    (declare-function embark-prefix-help-command 'embark))
  :bind
  ("C-'" . embark-act)
  ("C-." . embark-dwim)
  ("C-h B" . embark-bindings)
  :config
  (setq prefix-help-command #'embark-prefix-help-command)
  (add-to-list    ;; TODO: check this
    'display-buffer-alist
    '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
       nil
       (window-parameters (mode-line-format . none)))))



(use-package swiper
  :when (eq >>=|minibuffer/completing-framework 'ivy)
  :ensure t
  :bind
  ([remap isearch-forward] . swiper))


(use-package counsel
  :when (eq >>=|minibuffer/completing-framework 'ivy)
  :ensure t
  :demand t
  :commands counsel-mode counsel-yank-pop
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
  :functions ivy-mode
  :ensure t
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-fixed-height-minibuffer t)
  (ivy-use-virtual-buffers t)
  (ivy-virtual-abbreviate nil)    ; 'abbreviate is also nice
  :config
  (ivy-mode +1))




(use-package consult
  ;; based on example configuration - https://github.com/minad/consult
  :when (eq >>=|minibuffer/completing-framework 'vertico)
  :functions consult-xref consult-register-window consult-register-format
  :ensure t
  :bind
  (
    ("C-S-c M-x" . consult-mode-command)
    ("C-S-c h" . consult-history)
    ("C-S-c k" . consult-kmacro)
    ("C-S-c m" . consult-man)
    ("C-S-c i" . consult-info)
    ([remap Info-search] . consult-info)
    ("C-x M-:" . consult-complex-command)     ; repeat-complex-command
    ("C-x b" . consult-buffer)                ; switch-to-buffer
    ("C-x 4 b" . consult-buffer-other-window) ; switch-to-buffer-other-window
    ("C-x 5 b" . consult-buffer-other-frame)  ; switch-to-buffer-other-frame
    ("C-x t b" . consult-buffer-other-tab)    ; switch-to-buffer-other-tab
    ("C-x r b" . consult-bookmark)            ; bookmark-jump
    ("C-x p b" . consult-project-buffer)      ; project-switch-to-buffer
    ("M-#" . consult-register-load)
    ("M-'" . consult-register-store)          ; abbrev-prefix-mark (unrelated)
    ("C-M-#" . consult-register)
    ("M-y" . consult-yank-pop)                ; yank-pop
    ("M-g e" . consult-compile-error)
    ("M-g f" . consult-flymake)               ; Alternative: consult-flycheck
    ("M-g g" . consult-goto-line)             ; goto-line
    ("M-g M-g" . consult-goto-line)           ; goto-line
    ("M-g o" . consult-outline)
    ("M-g m" . consult-mark)
    ("M-g k" . consult-global-mark)
    ("M-g i" . consult-imenu)
    ("M-g I" . consult-imenu-multi)
    ("M-s d" . consult-find)                  ; alt: consult-fd
    ("M-s c" . consult-locate)
    ("M-s g" . consult-grep)
    ("M-s G" . consult-git-grep)
    ("M-s r" . consult-ripgrep)
    ("M-s l" . consult-line)
    ("M-s L" . consult-line-multi)
    ("M-s k" . consult-keep-lines)
    ("M-s u" . consult-focus-lines)
    ("M-s e" . consult-isearch-history)
    :map isearch-mode-map
    ("M-e" . consult-isearch-history)     ; isearch-edit-string
    ("M-s e" . consult-isearch-history)   ; isearch-edit-string
    ("M-s l" . consult-line)              ; needed to detect isearch
    ("M-s L" . consult-line-multi)        ; needed to detect isearch
    :map minibuffer-local-map
    ("M-s" . consult-history)             ; next-matching-history-element
    ("M-r" . consult-history)             ; previous-matching-history-element
  )
  :hook
  (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq
    register-preview-delay 0.5
    register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq
    ;; use consult to select xref locations with preview
    xref-show-xrefs-function #'consult-xref
    xref-show-definitions-function #'consult-xref)
  :config
  (declare-function consult--customize-put 'consult)    ; avoid warning
  (consult-customize
    consult-theme
    :preview-key '(:debounce 0.2 any)
    consult-ripgrep
    consult-git-grep
    consult-grep
    consult-bookmark
    consult-recent-file
    consult-xref
    consult--source-bookmark
    consult--source-file-register
    consult--source-recent-file
    consult--source-project-recent-file
    ;; :preview-key "M-."
    :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<")    ; "C-+"
  (setq consult-project-function #'>>=project-root)
 )


(use-package vertico
  :when (eq >>=|minibuffer/completing-framework 'vertico)
  :ensure t
  :commands vertico-mode
  :custom
  (vertico-cycle t)
  :config
  (vertico-mode +1))


(use-package embark-consult
  :when (eq >>=|minibuffer/completing-framework 'vertico)
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))




(use-package helm
  :when (eq >>=|minibuffer/completing-framework 'helm)
  :ensure t
  :functions helm-mini helm-multi-files helm-mode
  :preface
  (defun >>=helm/multi (&optional arg)
    "Use `helm-mini' if nil, otherwise call `helm-multi-files'."
    (interactive "P")
    (if (null arg)
      (helm-mini)
      ;; else
      (helm-multi-files)))
  :bind
  ;; we use 'C-c h' because 'C-x c' is similar to `kill-emacs' ('C-x C-c')
  ;; maybe use -> (global-unset-key (kbd "C-x c")
  ("C-c h" . helm-command-prefix)
  ("M-Y" . helm-show-kill-ring)
  ("C-h SPC" . helm-all-mark-rings)
  ("C-x C-f" . helm-find-files)
  ("C-x b" . >>=helm/multi)
  (:map minibuffer-local-map
    ("C-c C-l" . helm-minibuffer-history))
  (:map helm-map
    ("<C-M-left>" . helm-previous-source)
    ("<C-M-right>" . helm-next-source))
  :config
  (>>=remap "M-x" helm-M-x "M-X")
  (helm-mode +1))


(use-package swiper-helm
  :when (eq >>=|minibuffer/completing-framework 'helm)
  :ensure t
  :config
  (>>=remap "C-s" swiper-helm "C-S-s"))


(provide 'xorns-minibuffer)
;;; xorns-minibuffer.el ends here
