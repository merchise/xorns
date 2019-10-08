;;; xorns-sketch.el --- Structures that are pending of review

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;; Enjoy!


;;; Code:

(require 'rst)
(require 'python)
(require 'xorns-utils)
(require 'xorns-term)



(defun xorns-python-shell-send-paste (start end)
  "Send the region delimited by START and END wrapped with a %paste magic."
  (interactive "r")
  (unless (region-active-p)
    ;; If the region is not active, use the current line
    (save-excursion
      (end-of-line)
      (setq end (point))
      (beginning-of-line)
      (search-forward-regexp "[^\\s \t\n]" end 'noerror)
      (backward-char)  ;; search-forward ends after that
      (setq start (point))))
  ;; `python-shell-send-string' does too much magic trying to detect the
  ;; beginning of the output; using `comint-send-string' seems to be more
  ;; reliable.
  (save-excursion
    (kill-new (buffer-substring start end))
    (let*
      ((buffer (xorns-ansi-term))
        (process (get-buffer-process buffer)))
      ;; (with-current-buffer "*Python Shell*" (term-send-raw-string "x = 1\n")?
      (comint-send-string process "%paste\n"))))


(add-hook 'python-mode-hook
  (lambda ()
    (condition-case err
      (progn
        (define-key python-mode-map (kbd "C-c C-r")
          'xorns-python-shell-send-paste))
      (error (message "error@python-mode-hook: %s" err)))))


(add-hook 'text-mode-hook
  (lambda ()
    (define-key rst-mode-map (kbd "C-c C-r !")
      'xorns-python-shell-send-paste)
    (define-key rst-mode-map (kbd "C-c C-r C-r")
      'xorns-python-shell-send-cpaste)))


(defun xorns-python-indent-rigidly (start end arg)
  "Indent rigidly the region.

START and END mark the region.  ARG will be used to tell whether to indent or
outdent.

This simply calls `indent-rigidly' using ±4 spaces."
  (interactive "r\np")

  ;; TODO: Take ±4 from a configuration/environmental feature
  (if (= arg 1)  ;; the non-arg
    (indent-rigidly start end 4)
    (indent-rigidly start end -4)))


(define-key python-mode-map (kbd "C-x <C-tab>") 'xorns-python-indent-rigidly)
(define-key python-mode-map (kbd "C-x C-x <tab>") 'xorns-python-indent-rigidly)


(provide 'xorns-sketch)
;;; xorns-sketch.el ends here
