;;; xorns-term.el --- Terminal support

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;;; Commentary:

;; xorns-term is our interface to the `ansi-term' (general command
;; interpreter).

;; Enjoy!


;;; Code:

(require 'use-package)

(defun >>-shell-file-name ()
  "Command file name for the user's shell."
  (or
    explicit-shell-file-name
    (getenv "ESHELL")
    (getenv "SHELL")
    shell-file-name))


(use-package eshell
  :commands (eshell eshell-command)
  :preface
  (progn
    (eval-when-compile
      (require 'em-term)
      (declare-function eshell-cmpl-initialize 'em-cmpl))

    (defun >>-eshell/first-time ()
      "Run the first time `eshell-mode' is entered.a"
      (add-to-list 'eshell-visual-commands "htop"))

    (defun >>-eshell/init ()
      "Initialize eshell."
      (eshell-cmpl-initialize)
      (define-key eshell-mode-map
	(kbd "C-c C-d") 'quit-window)
      (when (bound-and-true-p helm-mode)
	(require 'helm-eshell)
	(define-key eshell-mode-map
	  [remap eshell-pcomplete] 'helm-esh-pcomplete)
	(define-key eshell-mode-map
	  [remap eshell-list-history] 'helm-eshell-history)))
    )
  :custom
  (eshell-history-size 1024)
  (eshell-hist-ignoredups t)
  :hook
  (eshell-first-time-mode . >>-eshell/first-time)
  (eshell-mode . >>-eshell/init)
  :config
  (progn
    (require 'esh-io)
    (require 'em-alias)
    (eshell/alias "l" "ls -lh $1")
    (eshell/alias "ll" "ls -alhF $1")
    (eshell/alias "la" "ls -A $1")))


(use-package comint
  :ensure helm
  :preface
  (defun >>-comint/init ()
    "Initialize comint."
    (when (bound-and-true-p helm-mode)
      (require 'helm-eshell)
      (define-key comint-mode-map
	(kbd "C-c C-l") 'helm-comint-input-ring)
      (define-key comint-mode-map
	(kbd "M-s f") 'helm-comint-prompts-all)))
  :hook
  (comint-mode . >>-comint/init))


(use-package term
  :preface
  (progn
    (declare-function term-send-raw-string 'term)

    (defun >>-term/raw-kill-line ()
      "Kill the rest of the current line in `term-char-mode'."
      (interactive)
      (term-send-raw-string "\C-k")
      (kill-line))

    (defun >>=term-main-shell ()
      "Command to execute ANSI terminal."
      (interactive)
      (let* ((command (>>-shell-file-name))
	      (buf-name "Terminal")
	      (starred (format "*%s*" buf-name))
	      (buffer (get-buffer starred))
	      (process (get-buffer-process buffer)))
	(if buffer
	  (if process
	    (progn
	      (setq command nil)
	      (switch-to-buffer buffer))
	    ;; else
	    (message ">>= killing '%s' because process was finished." starred)
	    (kill-buffer buffer)))
	(if command
	  (ansi-term command buf-name)
	  ;; else
	  buffer)))
    )
  :bind
  (("C-c t" . ansi-term)
   ("s-M-t" . >>=term-main-shell)
   (:map term-mode-map
     ("C-c C-t" . term-char-mode))
   (:map term-raw-map
     ("C-c C-t" . term-line-mode)
     ("C-y" . term-paste)
     ("C-k" . >>-term/raw-kill-line)))
  :custom
  (term-input-autoexpand t))


(provide 'xorns-term)
;;; xorns-term.el ends here
