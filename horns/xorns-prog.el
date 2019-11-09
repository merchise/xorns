;;; xorns-prog.el --- Programming language source code editing

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;;; Commentary:

;; Generic definitions for editing programming language source code.
;;
;; todo: http://company-mode.github.io/
;;       company-tern

;; Enjoy!


;;; Code:

(require 'javadoc-lookup nil 'noerror)

(require 'xorns-text)
(require 'xorns-utils)
(require 'xorns-buffers)
(require 'xorns-simple)

(require 'use-package)
(require 'xorns-packages)


(defgroup xorns-prog nil
  "Programming configurations for `xorns'."
  :prefix "xorns-prog-"
  :group 'xorns
  :group 'programming)



;;; Common Systems

(>>=ensure-packages auto-complete)

(use-package auto-complete
  :custom
  (ac-trigger-key "TAB")
  :config
  (progn
    (ac-flyspell-workaround)))


;;; Transparent Remote Access

(add-hook 'prog-mode-hook          ; run for all programming modes
  (lambda ()
    (if (>>=local-buffer)
      (auto-complete-mode t)
      (flyspell-prog-mode))
    (turn-on-auto-fill)
    (subword-mode nil)))


(add-hook 'conf-mode-hook          ; For configuration files
  (lambda ()
    (auto-complete-mode t)
    (turn-on-auto-fill)
    (subword-mode nil)))



;; Emacs Lisp

(use-package lisp-mode
  :custom
  (emacs-lisp-docstring-fill-column 78)
  (lisp-indent-offset 2)
  ;; TODO: Conflict with 'pyls'
  (create-lockfiles nil)
  )



;;; Python

(>>=ensure-packages
  flycheck lsp-mode lsp-ui company-lsp pipenv blacken)

;; Python

(defgroup xorns-python nil
  "Programming configurations for `xorns'."
  :prefix "xorns-python-"
  :group 'xorns-prog
  :group 'python)


(defcustom xorns-python-pyls-arguments nil
  "Extra arguments for the python language server."
  :group 'xorns-python
  :type '(repeat string))


(autoload 'po-mode "po-mode"    ; TODO: Check this
  "Major mode for translators to edit PO files" t)


(use-package python
  :defer t
  :init
  (defun -inferior-python-setup()
    (setq-default indent-tabs-mode nil)
    (linum-mode 0))

  :bind (:map python-mode-map ("C-m" . newline-and-indent))
  :hook ((python-mode . outline-minor-mode)
          (inferior-python-mode . -inferior-python-setup))
  )

(use-package flycheck
  :functions global-flycheck-mode
  :custom (flycheck-idle-change-delay 10)
  :config
  (global-flycheck-mode t))


(use-package lsp-mode
  :commands lsp
  :init
  (add-hook 'prog-mode-hook #'lsp))


(use-package lsp-ui
  :commands lsp-ui-mode
  :init
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  :custom (lsp-prefer-flymake nil)
  )

(use-package company-lsp
  :commands company-lsp
  )


(use-package yasnippet
  :commands yas-global-mode
  :init
  (yas-global-mode 1))


(use-package pipenv
  :hook (python-mode . pipenv-mode)
  :commands pipenv-projectile-after-switch-extended
  :custom
  (pipenv-projectile-after-switch-function
    #'pipenv-projectile-after-switch-extended))


;;; Hooks
(use-package blacken
  :hook
  ((python-mode .
     (lambda ()
       (turn-off-auto-fill)
       (blacken-mode)))))



;;; Javascript, CoffeeScript and LiveScript

(>>=ensure-packages tern tern-auto-complete js2-mode)

(use-package tern
  :config
  (add-hook 'js2-mode-hook #'tern-mode))


(use-package tern-auto-complete
  :after tern
  :config
  (tern-ac-setup))


;; This requires you have the tern program installed in your system and in the
;; exec-path.

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))



;;; Linux kernel programming

(defcustom xorns-linux-kernel-trees-path
  "~/src/linux-trees"
  "Where do you put the linux kernel source trees."
  :group 'xorns
  :type 'string)

;; TODO: 'c-syntactic-element' is void
(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces.  IGNORED is ignored."
  (defvar c-syntactic-element)    ;; Avoiding warning: ref to free variable
  (let* ((anchor (c-langelem-pos c-syntactic-element))
          (column (c-langelem-2nd-pos c-syntactic-element))
          (offset (- (1+ column) anchor))
          (steps (floor offset c-basic-offset)))
    (* (max steps 1)
      c-basic-offset)))

(add-hook 'c-mode-common-hook
  (lambda ()
    ;; Add kernel style
    (c-add-style
      "linux-tabs-only"
      '("linux" (c-offsets-alist
		  (arglist-cont-nonempty
		    c-lineup-gcc-asm-reg
		    c-lineup-arglist-tabs-only))))))

(add-hook 'c-mode-hook
  (lambda ()
    (let ((filename (buffer-file-name)))
      ;; Enable kernel mode for the appropriate files
      (when (and filename
	      (string-match (expand-file-name xorns-linux-kernel-trees-path)
		filename))
	(setq-default
	  indent-tabs-mode t
	  show-trailing-whitespace t)
	(c-set-style "linux-tabs-only")))))


(global-set-key (kbd "C-M-,") 'completion-at-point)

;;; C/C++ Mode -- Linux kernel programming

(use-package cc-mode
  :ensure nil
  :bind
  (:map c-mode-base-map
    ("C-c c" . compile))
  :hook
  (c-mode-common .
    (lambda ()
      (c-set-style "linux")
      (setq tab-width 4)
      (setq c-basic-offset 4))))




(provide 'xorns-prog)
;;; xorns-prog.el ends here
