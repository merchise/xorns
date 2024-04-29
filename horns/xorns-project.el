;;; xorns-project.el --- Manage and navigate projects in Emacs easily  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;;; Commentary:

;; Manage and navigate projects in Emacs easily using Merchise tools.

;; Enjoy!


;;; Code:

(require 'use-package)
(require 'xorns-tools)


(use-package projectile
  :ensure t
  :demand t
  :commands projectile-project-root projectile-mode
  :preface
  (defun >>=projectile/add-ignored-directories (&rest patterns)
    "Add all PATTERNS to `projectile-globally-ignored-directories'."
    (mapc
      (lambda (pattern)
        (add-to-list 'projectile-globally-ignored-directories pattern))
      patterns))

  (defun >>=projectile/add-root-files (&rest files)
    "Add all FILES to `projectile-project-root-files'."
    (mapc
      (lambda (file) (add-to-list 'projectile-project-root-files file))
      files))

  (defun >>=projectile/add-globally-ignored-files (&rest files)
    "Add all FILES to `projectile-globally-ignored-files'."
    (mapc
      (lambda (file) (add-to-list 'projectile-globally-ignored-files file))
      files))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :custom
  (projectile-enable-caching t)
  (projectile-switch-project-action 'projectile-dired)
  (projectile-ignored-project-function 'file-remote-p)
  :config
  (>>=projectile/add-ignored-directories "^elpa$" "^node_modules$")
  (>>=projectile/add-root-files ".travis.yml")
  (>>=projectile/add-globally-ignored-files ".DS_Store") ; MacOS search cache
  (projectile-mode +1)
  (when (bound-and-true-p ivy-mode)
    (setq projectile-completion-system 'ivy))
  (when (bound-and-true-p helm-mode)
    (setq projectile-completion-system 'helm)))


(use-package counsel-projectile
  :when (featurep 'counsel)
  :after projectile
  :ensure t
  :commands counsel-projectile-mode counsel-projectile-grep counsel-git-grep
  :init
  (defun >>=counsel-project-grep ()
    "Grep for a string in the current project."
    (interactive)
    (or
      (condition-case nil
        (or (counsel-git-grep) t)
        (error nil))
      (counsel-projectile-grep)))
  :bind
  ("C-c s" . >>=counsel-project-grep)
  (:map projectile-command-map
    ("." . >>=counsel-project-grep))
  :config
  (counsel-projectile-mode +1))


(provide 'xorns-project)
;;; xorns-project.el ends here
