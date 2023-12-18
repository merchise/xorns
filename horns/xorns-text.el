;;; xorns-text.el --- Merchise text modes, and its idiosyncratic commands  -*- lexical-binding: t -*-

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

(eval-and-compile
  (require 'use-package)
  (require 'xorns-tools))

(require 'xorns-buffers)

;; TODO: Migrate this
(require 'google-translate nil 'noerror)
(require 'google-translate-smooth-ui nil 'noerror)



;;; Sub-tree utility

(defun >>=toggle-subtree ()
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



;;; Graphically indicate the fill column

(use-package display-fill-column-indicator
  :custom
  (fill-column 78))



;;; display line numbers in the left margin

(use-package display-line-numbers
  :config
  (global-display-line-numbers-mode -1))



;;; multiple-cursors

(defvar >>=|ext/multiple-cursors t
  "Define whether to configure the `multiple-cursors' extension.")


(use-package multiple-cursors
  :when >>=|ext/multiple-cursors
  :ensure t
  :demand t
  :init
  (use-package phi-search :ensure t)
  :bind
  ("C-S-c C-S-c" . mc/mark-all-dwim)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this)
  ("C-c m t" . mc/mark-all-like-this)
  ("C-c m m" . mc/mark-all-like-this-dwim)
  ("C-c m l" . mc/edit-lines)
  ("C-c m e" . mc/edit-ends-of-lines)
  ("C-c m a" . mc/edit-beginnings-of-lines)
  ("C-c m n" . mc/mark-next-like-this)
  ("C-c m p" . mc/mark-previous-like-this)
  ("C-c m s" . mc/mark-sgml-tag-pair)
  ("C-c m d" . mc/mark-all-like-this-in-defun))



;;; text-modes

(use-package text-mode
  :after display-line-numbers
  :init
  (defun >>=init-text-mode ()
    "Init `text-mode' based modes."
    ;; TODO: `show-trailing-whitespace'?
    (display-line-numbers-mode +1)
    (display-fill-column-indicator-mode +1))
  :mode
  ("\\.log\\'" "/LICENSE\\'")
  :hook
  (text-mode . >>=init-text-mode)
  :custom
  (truncate-lines t))


(defun >>=tex-mode-setup ()
  "Common settings for tex-modes."
  (setq-default ispell-parser 'tex)
  (turn-on-auto-fill)
  (flyspell-mode))


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
  :ensure t
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
(keymap-set outline-minor-mode-map "C-=" 'outline-show-subtree)
(keymap-set outline-minor-mode-map "M-=" 'outline-hide-subtree)
(keymap-set outline-minor-mode-map "C-+" '>>=toggle-subtree)


(provide 'xorns-text)
;;; xorns-text.el ends here
