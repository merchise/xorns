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
  :demand t
  :commands projectile-project-root projectile-mode
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :custom
  (projectile-enable-caching t)
  (projectile-switch-project-action 'projectile-dired)
  (projectile-ignored-project-function 'file-remote-p)
  :config
  (>>=append projectile-globally-ignored-directories
    '("elpa" ".vscode" "node_modules")
    >>=|projectile/extra-ignored-directories)
  (>>=append projectile-project-root-files
    '(".travis.yml") >>=|projectile/project-root-files)
  (when (memq 'rescript >>=|programming/extra-languages)
    (>>=append projectile-project-root-files
      '("bsconfig.json") >>=|projectile/project-root-files))
  (add-to-list
    ;; Ignore Mac Search Index Cache
    'projectile-globally-ignored-files ".DS_Store")
  (projectile-mode +1)
  (if (bound-and-true-p ivy-mode)
    (setq projectile-completion-system 'ivy))
  (if (bound-and-true-p helm-mode)
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
