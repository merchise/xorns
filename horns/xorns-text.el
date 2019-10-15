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

(require 'auto-complete nil 'noerror)
(require 'google-translate nil 'noerror)
(require 'google-translate-smooth-ui nil 'noerror)
(require 'xorns-utils nil 'noerror)
(require 'xorns-buffers)
(require 'xorns-packages)

(>>=ensure-packages markdown-mode)



;;; Custom Variables and Settings

(setq-default
  ;; Consecutive years replaced with range
  copyright-year-ranges t
  ;; Do not display continuation lines
  truncate-lines t
  ;; Key to start auto-complete
  ac-trigger-key "TAB"
  )


;; Enable some disabled commands

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)


;; Typed text replaces the selection
(delete-selection-mode 1)


;; Turn ON parenthesis matching
(use-package paren
  :custom
  (show-paren-style 'mixed)
  :config
  (show-paren-mode))


;; dictionaries

(use-package ispell
  :bind
  (("C-c i d" . ispell-change-dictionary)
   ("C-c i l" . ispell-change-dictionary)
   ("C-c i r" . ispell-region)
   ("C-c i b" . ispell-buffer)
   ("C-c i c" . ispell-comments-and-strings)
   ("C-c i k" . ispell-kill-ispell)
   ("C-c i m" . ispell-message))
  :custom
  (ispell-highlight-p t)
  (ispell-silently-savep t)
  (ispell-dictionary "english"))



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



;;; text-modes

(use-package text-mode
  :mode ("\\.log\\'" "/LICENSE\\'"))


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


(defun xorns-auto-complete-mode ()
  "Turn ON `auto-complete' mode in a safe way.

If this feature is not installed don't fail and just report a message."
  (if (featurep 'auto-complete)
    (auto-complete-mode t)
    ;; else
    (xorns-missing-feature 'auto-complete)))



;;; Key-bindings
(global-set-key (kbd "C-|") 'google-translate-smooth-translate)


;; For outline minor modes
;; TODO: This is defined by standard mode inner "C-c@'
(define-key outline-minor-mode-map (kbd "C-=") 'outline-show-subtree)
(define-key outline-minor-mode-map (kbd "M-=") 'outline-hide-subtree)
(define-key outline-minor-mode-map (kbd "C-+") 'xorns-toggle-subtree)


(provide 'xorns-text)
;;; xorns-text.el ends here
