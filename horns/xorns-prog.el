;;; xorns-prog.el --- Programming language source code editing

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>
;; or type `C-h C-c' in Emacs.

;;; Commentary:

;; Generic definitions for editing programming language source code.

;; This module is automatically used when::
;;
;;     (require 'xorns)

;; Enjoy!


;;; Code:

(require 'tramp)
(require 'auto-complete)
(require 'tern)
(require 'tern-auto-complete)
(require 'cc-mode nil 'noerror)
(require 'javadoc-lookup nil 'noerror)
(require 'blacken nil 'noerror)

(require 'xorns-text nil 'noerror)
(require 'xorns-utils)

(require 'use-package)


(defgroup xorns-prog nil
  "Programming configurations for `xorns'."
  :prefix "xorns-prog-"
  :group 'xorns
  :group 'programming)




;; Emacs Lisp

(use-package lisp-mode
  :custom
  (emacs-lisp-docstring-fill-column 78)
  (lisp-indent-offset 2)
  ;; TODO: Conflict with 'pyls'
  (create-lockfiles nil)
  )




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
    (xorns-set-value 'indent-tabs-mode nil)
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

(add-hook 'prog-mode-hook          ; run for all programming modes
  (lambda ()
    (condition-case err
      (progn
        (unless (tramp-connectable-p (buffer-file-name))
          (xorns-auto-complete-mode)
          (flyspell-prog-mode))
        (turn-on-auto-fill)
        (ispell-change-dictionary "english")
        (subword-mode nil)
        (xorns-try-linum-mode))
      (error (message "error@prog-mode-hook: %s" err)))))


(add-hook 'conf-unix-mode-hook          ; For configuration files
  (lambda ()
    (condition-case err
      (progn
        (xorns-auto-complete-mode)
        (turn-on-auto-fill)
        (ispell-change-dictionary "english")
        (subword-mode nil)
        (xorns-try-linum-mode))
      (error (message "error@conf-unix-mode-hook: %s" err)))))




;;; Javascript, CoffeeScript and LiveScript

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
	(xorns-set-values
	  '(indent-tabs-mode t)
	  '(show-trailing-whitespace t))
	(c-set-style "linux-tabs-only")))))


(global-set-key (kbd "C-M-,") 'completion-at-point)

(when (featurep 'blacken)
  (add-hook 'python-mode-hook 'turn-off-auto-fill)
  (add-hook 'python-mode-hook 'blacken-mode))


(provide 'xorns-prog)
;;; xorns-prog.el ends here
