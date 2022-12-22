;;; xorns-trees.el --- Tree layout file explorer  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;;; Commentary:

;; Configuration of `treemacs', a file and project explorer similar to NeoTree
;; or vim’s NerdTree, but largely inspired by the Project Explorer in Eclipse.
;; See https://github.com/Alexander-Miller/treemacs

;; Enjoy!


;;; Code:

(eval-and-compile
  (require 'use-package nil 'noerror))


(defvar >>=|treemacs/enable t
  "Determines if `treemacs' is enabled.")


(use-package treemacs
  :when >>=|treemacs/enable
  :ensure t
  :defer t
  ;; :init
  ;; (with-eval-after-load 'winum
  ;;   (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :bind
  ("C-<tab>"   . treemacs-select-window)
  ("C-x t 1"   . treemacs-delete-other-windows)
  ("C-x t t"   . treemacs)
  ("C-x t d"   . treemacs-select-directory)
  ("C-x t B"   . treemacs-bookmark)
  ("C-x t C-t" . treemacs-find-file)
  ("C-x t M-t" . treemacs-find-tag)
  :custom
  (treemacs-follow-after-init t)
  (treemacs-width-is-initially-locked nil)
  :config
  (treemacs-follow-mode t)
  (treemacs-project-follow-mode)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (when treemacs-python-executable
    (treemacs-git-commit-diff-mode t))
  (when (executable-find "git")
    (treemacs-git-mode
      (if treemacs-python-executable 'deferred 'simple)))
  (treemacs-hide-gitignored-files-mode)
  )


(use-package all-the-icons
  :when >>=|treemacs/enable
  :after treemacs
  :ensure t)


(use-package treemacs-projectile
  :when >>=|treemacs/enable
  :after (treemacs projectile)
  :ensure t)


(use-package treemacs-icons-dired
  :when >>=|treemacs/enable
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)


(use-package treemacs-magit
  :when >>=|treemacs/enable
  :after (treemacs magit)
  :ensure t)


(provide 'xorns-trees)
;;; xorns-trees.el ends here
