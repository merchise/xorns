;;; xorns-buffers.el --- Advanced tools for manipulating buffers  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;;; Commentary:

;; This module has advanced features to manipulate buffers: view, edit,
;; delete, or change attributes of buffers.
;;
;; Main features are:
;;
;; - Configure `C-x C-b' to list buffers using `ibuffer' instead
;;   standard `list-buffers', or `xorns-select-buffer' if
;;   `xorns-use-select-buffer' is non nil and `xorns-select-buffer-enabled'
;;   evaluates to t.
;;
;; - Set `ibuffer' groups.
;;
;; - Functionality to force `*scratch*' buffer.
;;
;; Some common-systems level buffer manipulation utilities could be found on
;; `xorns-core' module.

;; Enjoy!


;;; Code:

(eval-and-compile
  (require 'xorns-tools))

(require 'ibuf-ext)
(require 'xorns-core)


(use-package ibuffer-vc
  :ensure t)


(use-package ibuf-ext
  :defer t
  :custom
  (ibuffer-show-empty-filter-groups nil))


(use-package ibuffer
  :init
  (defun >>=ibuffer-visit-buffer (&optional single)
    "Visit the buffer on this line (use SINGLE window)."
    (interactive "P")
    (ibuffer-visit-buffer single)
    (kill-buffer "*Ibuffer*"))
  :bind
  (("C-x C-b" . ibuffer)
   :map ibuffer-mode-map
   ("M-RET" . >>=ibuffer-visit-buffer))
  :custom
  (ibuffer-saved-filter-groups    ; check `ibuffer-saved-filters'
    '(("Normal"
        ("System"
          (or
            (name . "[*]scratch[*]")
            (name . "[*]Messages[*]")
            (name . "[*]Backtrace[*]")))
        ("Terms"
          (or
            (mode . term-mode)
            (mode . vterm-mode)
            (mode . eshell-mode)))
        ("Dired"
          (or
            (mode . dired-omit-mode)
            (mode . dired-mode)))
        ("GNUs/Org"    ; TODO: Check this
          (or
            (name . "*Deft*")
            (name . "bbdb")
            (name . "[.]newsrc-dribble")
            (mode . org-mode)
            (mode . org-agenda-mode)
            (mode . diary-mode)
            (mode . calendar-mode)
            (mode . bbdb-mode)
            (mode . message-mode)
            (mode . mail-mode)
            (mode . gnus-group-mode)
            (mode . gnus-summary-mode)
            (mode . gnus-article-mode)))
        ("Configuration" (mode . conf-unix-mode))
        ("Programming"
          (or
            (name . "^[.]env")
            (derived-mode . prog-mode)
            (derived-mode . conf-mode)
            (derived-mode . yaml-mode)
            (mode . ess-mode)
            (mode . compilation-mode)))
        ("Web"
          (or
            (derived-mode . html-mode)
            (derived-mode . sgml-mode)
            (derived-mode . css-mode)
            (mode . javascript-mode)
            (mode . w3m-mode)
            (mode . js2-mode)
            (mode . coffee-mode)
            (mode . scss-mode)
            (derived-mode . haml-mode)
            (mode . sass-mode)))
        ("Text/TeX"
          (or
            (and
              (derived-mode . text-mode)
              (not (starred-name)))
            (derived-mode . tex-mode)
            (mode . latex-mode)
            (mode . context-mode)
            (mode . ams-tex-mode)
            (mode . bibtex-mode)))
        ("Version Control"
          (or
            (name . "^[.]git")
            (name . "^magit")
            (name . "[*]vc[*]")
            (mode . git-commit-mode)
            (mode . git-commit-major-mode)
            (mode . git-rebase-mode)
            (mode . magit-mode)
            (mode . magit-cherry-mode)
            (mode . magit-diff-mode)
            (mode . magit-log-mode)
            (mode . magit-log-select-mode)
            (mode . magit-merge-preview-mode)
            (mode . magit-popup-mode)
            (mode . magit-process-mode)
            (mode . magit-refs-mode)
            (mode . magit-reflog-mode)
            (mode . magit-revision-mode)
            (mode . magit-stash-mode)
            (mode . magit-stashes-mode)
            (mode . magit-status-mode)
            (mode . diff-mode)))
        ("LSP/Linters/Logs"
          (or
            (name . "lsp")
            (name . "blacken")
            (name . "prettier")
            (name . "eslint")
            (name . "bash-ls")
            (name . "json-ls")
            (name . "html-ls")
            (name . "css-ls")
            (name . "dockerfile-ls")
            (name . "^[*]rust")
            (name . "^[*]Flycheck")
            (name . "^[*]Dired log[*]$")))
        ("Backup files"
          (or
            (name . "^[#].*[#]$")))
        ("Help/Info/Completions/Customize"
          (or
            (name . "^[*]Help[*]$")
            (name . "^[*]info[*]$")
            (name . "^[*]helpful")
            (name . "^[*]Customize")
            (derived-mode . special-mode)
            (mode . Info-mode)
            (mode . rfcview-mode)
            (mode . Custom-mode)
            (mode . completion-list-mode)
            (mode . rg-mode)))
        ("X Window Manager" (mode . exwm-mode))
        )))
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-default-sorting-mode 'major-mode)
  (ibuffer-formats
    '((mark modified read-only vc-status-mini " "
        (name 24 24 :left :elide)
        " "
        (size-h 9 -1 :right)
        " "
        (mode 16 16 :left :elide)
        " "
        filename-and-process)))
  :hook
  ((ibuffer-mode .
     (lambda () (ibuffer-switch-to-saved-filter-groups "Normal"))))
  :config
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (file-size-human-readable (buffer-size))))



;;; Misc

(global-set-key (kbd "C-x <f2>") 'rename-buffer)
(global-set-key (kbd "C-c M-s") '>>=scratch/force)
;; (global-set-key (kbd "C-c h") '>>=toggle-header-mode-line)

(add-hook 'after-init-hook '>>=set-default-directory)


(provide 'xorns-buffers)
;;; xorns-buffers.el ends here
