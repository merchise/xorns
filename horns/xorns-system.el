;;; xorns-system.el --- Cryptography protocols  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;;; Commentary:

;; System level configuration
;;
;; Include: grep tools, dictionary servers, processes and commands,
;; cryptography or techniques for secure communications, version control
;; system tools (mainly `magit').
;;
;; Also `treemacs', a file and project explorer similar to NeoTree or vim’s
;; NerdTree, but largely inspired by the Project Explorer in Eclipse.  See
;; https://github.com/Alexander-Miller/treemacs


;; Enjoy!


;;; Code:

(eval-and-compile
  (require 'xorns-tools)
  (require 'use-package))



;;; grep

(defvar >>=|ext/ripgrep "rg"
  "Whether `ripgrep' extensions must be configured.
Could be a boolean, or a string specifying the `ripgrep' command name, the
default value is \"rg\".  Usually this variable is used with the function
`>>=command/check'.")


(use-package grep    ;; todo: check `wgrep', `scf-mode', `deadgrep'
  :demand t
  :bind
  (("C-c C-g n" . find-name-dired)
   ("C-c C-g f" . find-grep)
   ("C-c C-g g" . grep)
   ("C-c C-g d" . find-grep-dired)
   ("C-c r" . rgrep)
   ("C-c C-g r" . rgrep))
  :config
  (progn
    (dolist
      (type
        '(jpg jpeg png gif    ; images
          mpg mjpg avi        ; videos
          rar zip 7z))        ; archives
      (add-to-list 'grep-find-ignored-files (format "*.%s" type)))
    (dolist
      (name '(".tox" ".hypothesis" ".idea" ".mypy_cache" ".vscode"))
      (add-to-list 'grep-find-ignored-directories name))))


(use-package deadgrep
  :when (>>=command/check >>=|ext/ripgrep)
  :ensure t
  :after grep
  :init
  (use-package rg
    :ensure t
    :init
    (defvar >>=|rg/max-columns 512
      "Override value for `--max-columns' option.")
    :bind
    ("C-c s" . rg-project)
    :config
    (when >>=|rg/max-columns
      (let ((max (format "--max-columns=%s" >>=|rg/max-columns)))
        (setq rg-command-line-flags (cons max rg-command-line-flags)))))
  :bind
  ([remap rgrep] . deadgrep))



;;; Dictionary servers

(use-package dictionary
  :bind
  ("M-#" . dictionary-lookup-definition)
  ("C-c w" . dictionary-search))



;;; Processes and commands

(use-package proced
  :unless (eq system-type 'darwin)    ;; unavailable on OS-X
  :bind
  ("C-x p" . proced))



;;; which-key

(defvar >>=|which-key/enable t
  "Whether `which-key' package must be configured.
If not a boolean value, a side-window will be configured and should be one of
top, bottom, left or right symbols.")


(use-package which-key
  :when >>=|which-key/enable
  :ensure t
  :hook
  (after-init . which-key-mode)
  :config
  (unless (eq >>=|which-key/enable t)
    (setq
      which-key-popup-type 'side-window
      which-key-side-window-location >>=|which-key/enable)))



;;; Cryptography or techniques for secure communications

;; GnuPG
;; https://stackoverflow.com/questions/60812866/emacs-gpg-pinentry-el-for-authentication

(defvar >>=|crypt/gpg-integration t
  "When to configure GPG integration with Emacs.
Right now, variable `epg-pinentry-mode' is set, but other configurations would
be needed in the future..")


(when >>=|crypt/gpg-integration
  (require 'epg-config)
  (set
    (if (> emacs-major-version 26)
      'epg-pinentry-mode
      ;; else: ; obsolete since 27.1
      'epa-pinentry-mode)
    'loopback))



;;; Version Control Integration

(defvar >>=|ext/git-forges t
  "Whether `forge' extensions must be configured.")


(use-package git-modes
  :ensure t
  :defer t)


(use-package magit
  :ensure t
  :bind
  (("C-x g" . magit-status)
   ("C-c g c" . magit-clone)
   ("C-c g s" . magit-status)
   ("C-c g b" . magit-blame)
   ("C-c g l" . magit-log-buffer-file)
   ("C-c g p" . magit-pull))
  :hook
  (git-commit-mode . >>=tex-mode-setup)
  :config
  (put 'magit-clean 'disabled nil))


(use-package forge
  :when >>=|ext/git-forges
  :ensure t
  :after magit)



;;; Tree layout file explorer

(defvar >>=|treemacs/enable t
  "Determines if `treemacs' is enabled.")


(use-package treemacs
  :when >>=|treemacs/enable
  :ensure t
  :defer t
  :bind
  ("C-<tab>"    . treemacs-select-window)
  ("C-x t 1"    . treemacs-delete-other-windows)
  ("C-x t t"    . treemacs)
  ("C-<escape>" . treemacs)
  ("C-x t d"    . treemacs-select-directory)
  ("C-x t B"    . treemacs-bookmark)
  ("C-x t C-t"  . treemacs-find-file)
  ("C-x t M-t"  . treemacs-find-tag)
  :custom
  (treemacs-follow-after-init t)
  (treemacs-width-is-initially-locked nil)
  :config
  (declare-function treemacs-hide-gitignored-files-mode 'treemacs-async)
  (declare-function treemacs-git-mode 'treemacs-async)
  (declare-function treemacs-fringe-indicator-mode 'treemacs-fringe-indicator)
  (declare-function treemacs-filewatch-mode 'treemacs-filewatch-mode)
  (declare-function treemacs-follow-mode 'treemacs-follow-mode)
  (treemacs-follow-mode t)
  (treemacs-project-follow-mode)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (when treemacs-python-executable
    (treemacs-git-commit-diff-mode t))
  (when (executable-find "git")
    (treemacs-git-mode
      (if treemacs-python-executable 'deferred 'simple)))
  (treemacs-hide-gitignored-files-mode))


(use-package all-the-icons
  :when >>=|treemacs/enable
  :after treemacs
  :ensure t)


(use-package treemacs-icons-dired
  :when >>=|treemacs/enable
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)


(use-package treemacs-projectile
  :when >>=|treemacs/enable
  :after (treemacs projectile)
  :ensure t)


(use-package treemacs-magit
  :when >>=|treemacs/enable
  :after (treemacs magit)
  :ensure t)


(provide 'xorns-system)
;;; xorns-system.el ends here
