;;; xorns-project.el --- Manage and navigate projects in Emacs easily  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;;; Commentary:

;; Manage and navigate projects in Emacs easily using Merchise tools.

;; Enjoy!


;;; Code:

(require 'use-package)
(require 'xorns-tools)


(defvar >>=|projectile/extra-ignored-directories nil
  "A list of extra directories ignored by projectile.")


(defvar >>=|projectile/project-root-files nil
  "A list of files considered to mark the root of a project.")


(use-package projectile
  :ensure t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :bind
  ("C-c C-d" . projectile-dired)
  ("C-c C-b" . projectile-ibuffer)
  ("C-c C-k" . projectile-kill-buffers)
  :custom
  (projectile-enable-caching t)
  (projectile-switch-project-action 'projectile-dired)
  (projectile-ignored-project-function 'file-remote-p)
  :config
  (progn
    (>>=append projectile-globally-ignored-directories
      '("elpa" ".vscode" "node_modules")
      >>=|projectile/extra-ignored-directories)
    (>>=append projectile-project-root-files
      '(".travis.yml")
      >>=|projectile/project-root-files)
    (add-to-list
      ;; Ignore Mac Search Index Cache
      'projectile-globally-ignored-files ".DS_Store")
    (projectile-mode +1)
    (if (bound-and-true-p ivy-mode)
      (setq projectile-completion-system 'ivy))
    (if (bound-and-true-p helm-mode)
      (setq projectile-completion-system 'helm))))


(provide 'xorns-project)
;;; xorns-project.el ends here
