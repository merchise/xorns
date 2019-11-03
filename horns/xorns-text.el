;;; xorns-text.el --- Merchise text modes, and its idiosyncratic commands

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;;; Commentary:

;; Provides the fundamental text modes configuration.  Will include
;; all basic text modes, like `rst-mode'.

;; Because it is required in `xorns-prog', this module is automatically
;; used when::
;;
;;     (require 'xorns)

;; Enjoy!


;;; Code:

(require 'outline)

(require 'google-translate nil 'noerror)
(require 'google-translate-smooth-ui nil 'noerror)
(require 'xorns-utils nil 'noerror)
(require 'xorns-buffers)
(require 'xorns-packages)

(>>=ensure-packages markdown-mode)



;;; Sub-tree utility
(defun xorns-toggle-subtree ()
  "Show or hide the current subtree depending on its current state."
  (interactive)
  (save-excursion
    (outline-back-to-heading)
    (if (not (outline-invisible-p (line-end-position)))
      (outline-hide-subtree)
      (outline-show-subtree)
      (outline-show-entry))))



;;; Deal with copyright notices

(use-package copyright
  :bind
  ("C-c x c" . copyright-update)
  :custom
  (copyright-year-ranges t))



;;; text-modes

(use-package text-mode
  :mode
  ("\\.log\\'" "/LICENSE\\'")
  :custom
  (truncate-lines t))


(defun >>=tex-mode-setup ()
  "Common settings for tex-modes."
  (setq-default ispell-parser 'tex)
  (turn-on-auto-fill)
  (flyspell-mode nil))


(use-package tex-mode
  :defer t
  :hook
  ((tex-mode . >>=tex-mode-setup)
   (latex-mode . >>=tex-mode-setup)))


(use-package rst
  :defer t
  :hook
  (rst-mode . >>=tex-mode-setup)
  :custom
  (rst-new-adornment-down t))


(use-package markdown-mode
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode
  (("README\\.md\\'" . gfm-mode) ;; github-flavored-markdown
   ("\\.md\\'" . markdown-mode)
   ("\\.mkd" . markdown-mode)
   ("\\.mkdn" . markdown-mode)
   ("\\.mdown" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :init
  (defun >>=markdown-mode-setup ()
    (flyspell-mode)
    (subword-mode))
  :hook
  (markdown-mode . >>=markdown-mode-setup)
  (gfm-mode . >>=markdown-mode-setup)
  :custom-face
  (markdown-code-face ((t nil)))
  :custom
  (markdown-asymmetric-header t))




;;; Key-bindings
(global-set-key (kbd "C-|") 'google-translate-smooth-translate)


;; For outline minor modes
;; TODO: This is defined by standard mode inner "C-c@'
(define-key outline-minor-mode-map (kbd "C-=") 'outline-show-subtree)
(define-key outline-minor-mode-map (kbd "M-=") 'outline-hide-subtree)
(define-key outline-minor-mode-map (kbd "C-+") 'xorns-toggle-subtree)


(provide 'xorns-text)
;;; xorns-text.el ends here
