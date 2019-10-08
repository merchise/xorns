;;; xorns-ido.el ---  Configuration for ido

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;;; Commentary:

;; Definitions that depends of all `xorns' sub-modules.

;; Enjoy!


;;; Code:

(require 'ido)
(require 'xorns-utils)


(defun -customize-vertical-ido ()
  "Customize `ido' for vertical appearance."
  (setq-default ido-decorations
    '("\n-> " "" "\n " "\n ..." "[" "]"
       " [No match]" " [Matched]" " [Not readable]"
       " [Too big]" " [Confirm]"))

  (defun ido-disable-line-truncation ()
    (set (make-local-variable 'truncate-lines) nil))

  (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)

  (defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
    (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
    (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))

  (add-hook 'ido-setup-hook 'ido-define-keys))


;; TODO: experimental
;;   (-customize-vertical-ido)


(provide 'xorns-ido)
;;; xorns-ido.el ends here
