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


(use-package term
  :preface
  (progn
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
