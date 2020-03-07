;;; xorns-buffers.el --- Buffers management

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;; Commentary:

;; This module main features are:
;;
;; - Configure `C-x C-b' to list buffers using `ibuffer' instead
;;   standard `list-buffers', or `xorns-select-buffer' if
;;   `xorns-use-select-buffer' is non nil and `xorns-select-buffer-enabled'
;;   evaluates to t.
;;
;; - Set `ibuffer' groups.
;;
;; - Functionality to force `*scratch*' buffer.

;; Enjoy!


;;; Code:

(require 'use-package)
(require 'use-package-chords)
(require 'xorns-utils)
(require 'ibuf-ext)


;; TODO: To use grizzl instead of ido for completion
;;       (setq flycheck-completion-system 'grizzl)
(use-package grizzl
  :ensure t
  ;; see `akheron/emacs.org'
  :defer t)


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
  (ibuffer-saved-filter-groups
    '(("Normal"
	("System"
	  (or
	    (name . "[*]scratch[*]")
	    (name . "[*]Messages[*]")
	    (name . "[*]Backtrace[*]" )
	    (mode . Custom-mode)))
	("Terms"
	  (or
	    (mode . term-mode)
	    (mode . vterm-mode)))
	("Dired"
	  (or
	    (mode . dired-omit-mode)
	    (mode . dired-mode)))
	("GNUs/Org"
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
	("Python" (mode . python-mode))
	("Haskell/Agda/Coq"
	  (or
	    (mode . haskell-mode)
	    (mode . agda2-mode)
	    (mode . coq-mode)))
	("Lisp"
	  (or
	    (mode . emacs-lisp-mode)
	    (mode . lisp-interaction-mode)
	    (mode . lisp-mode)))
	("Programming"
	  (or
	    (mode . c-mode)
	    (mode . cc-mode)
	    (mode . ruby-mode)
	    (mode . rust-mode)
	    (mode . scala-mode)
	    (mode . java-mode)
	    (mode . scala-mode-inf)
	    (mode . prog-mode)))
	("Text/Markdown/RST/TeX"
	  (or
	    (mode . text-mode)
	    (mode . rst-mode)
	    (mode . markdown)
	    (mode . markdown-mode)
	    (mode . tex-mode)))
	("Web/XML/HTML/CSS"
	  (or
	    (mode . w3m-mode)
	    (mode . javascript-mode)
	    (mode . nxml-mode)
	    (mode . html-mode)
	    (mode . css-mode)
	    (mode . less-mode)
	    (mode . sass-mode)))
	("Version Control"
	  (or
	    (name . "^magit")
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
	("Help/Info/Completions/Customize"
	  (or
	    (name . "^[*]Help[*]$")
	    (name . "^[*]Apropos[*]$")
	    (name . "^[*]info[*]$")
	    (name . "^[*]helpful")
	    (name . "^[*]Customize")
	    (mode . help-mode)
	    (mode . Info-mode)
	    (mode . Man-mode)
	    (mode . woman-mode)
	    (mode . rfcview-mode)
	    (mode . completion-list-mode)))
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
    (file-size-human-readable (buffer-size)))
  )



;;; Custom key-bindings

(global-set-key (kbd "C-x <f2>") 'rename-buffer)

(defun xorns-toggle-header-mode-line ()
  "Toggle visibility of header mode-line."
  (interactive)
  (if (not header-line-format)
      (setq header-line-format
	'(multiple-frames "%b"
	   (" " (:eval (abbreviate-file-name default-directory)))))
    ; else
    (setq header-line-format nil))
  (force-mode-line-update 'all))



;;; Buffers

(defun xorns-force-scratch (&optional arg)
  "Switch to `*scratch*` buffer, creating a new one if needed.

An optional argument ARG could be given to delete other windows; if
`0' also reset `default-directory' to `xorns' default."
  (interactive "P")
  (let ((buf (get-buffer-create "*scratch*")))
    (set-buffer-major-mode buf)
    (switch-to-buffer-other-window buf)
    (if (= (prefix-numeric-value arg) 0)
      (>>=set-default-directory))
    (if arg (delete-other-windows))))


(global-set-key (kbd "C-c s") 'xorns-force-scratch)
;; (global-set-key (kbd "C-c h") 'xorns-toggle-header-mode-line)



;;; Hooks

(add-hook 'after-init-hook '>>=set-default-directory)


(provide 'xorns-buffers)
;;; xorns-buffers.el ends here
