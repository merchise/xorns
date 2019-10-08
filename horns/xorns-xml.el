;;; xorns-xml.el --- Execute all Merchise preferred initialization

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;;; Commentary:

;; Basically requires hideshow and integrates with nxml


;;; Code:
(require 'hideshow)
(require 'sgml-mode)
(require 'nxml-mode)

(add-to-list 'hs-special-modes-alist
  '(nxml-mode
     "<!--\\|<[^/>]*[^/]>"
     "-->\\|</[^/>]*[^/]>"

     "<!--"
     sgml-skip-tag-forward
     nil))

(add-hook 'nxml-mode-hook 'hs-minor-mode)

(define-key nxml-mode-map (kbd "C-c +") 'hs-toggle-hiding)

(provide 'xorns-xml)
;;; xorns-xml.el ends here
