;;; xorns-term.el --- Terminal support

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;;; Commentary:

;; xorns-term is our interface to the `ansi-term' (general command
;; interpreter).

;; Enjoy!


;;; Code:

(require 'use-package)


(use-package term
  :bind
  (("C-c t" . ansi-term)
   (:map term-mode-map
     ("C-c C-t" . term-char-mode))
   (:map term-raw-map
     ("C-c C-t" . term-line-mode)))
  :custom
  (term-input-autoexpand t))


(provide 'xorns-term)
;;; xorns-term.el ends here
