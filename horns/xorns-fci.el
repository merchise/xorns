;;; xorns-fci.el --- Fill Column Indicator

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;;; Commentary:

;; Generic definitions for editing programming language source code.

;; This module is automatically used when::
;;
;;     (require 'xorns)

;; Enjoy!


;;; Code:


(require 'use-package)


(use-package fill-column-indicator
  :defer t
  :commands fci-mode
  :preface

  (defun xorns-fci-mode-on ()
    "Set `fci-mode' on."
    (fci-mode 1))

  (defun xorns-fci-mode-off ()
    "Set `fci-mode' off."
    (fci-mode 0))

  :custom
  (fill-column 78)
  (fci-rule-width 1)
  (fci-rule-color "#CCCCCC")    ;; or "Legislatively"?

  :hook
  ((text-mode . xorns-fci-mode-on)
   (prog-mode . xorns-fci-mode-on)
   (conf-mode . xorns-fci-mode-on)))



(provide 'xorns-fci)
;;; xorns-fci.el ends here
