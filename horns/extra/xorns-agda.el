;;; xorns-agda.el ---  configuration for midnight-mode

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Experimental module to document a feature to develop in the future.
;;
;; More documentation in:
;; - https://www.emacswiki.org/emacs/MidnightMode
;; - https://fortune-teller-amy-88756.netlify.com/knusper

;;; Code:

(when (null (functionp 'agda-mode))
  (-when-let* ((agda-mode (xorns-executable-find "agda-mode"))
               (agda-locate (concat agda-mode " locate"))
               (coding-system-for-read 'utf-8))
    (load-file (shell-command-to-string agda-locate))))

(provide 'xorns-agda)
;;; xorns-agda.el ends here
