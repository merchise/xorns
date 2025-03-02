;;; xorns-system.el --- Cryptography protocols  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;;; Commentary:

;; System packages configuration

;; Include: grep tools, dictionary servers, processes and commands,
;; cryptography or techniques for secure communications, version control
;; system tools (mainly `magit').

;; Also `treemacs', a file and project explorer similar to NeoTree or vim’s
;; NerdTree, but largely inspired by the Project Explorer in Eclipse.  See
;; https://github.com/Alexander-Miller/treemacs

;; To solve issues of some modules using some Unicode characters and icons
;; make sure you have proper resources installed on your system to avoid that.
;; For more information see packages `all-the-icons' and `nerd-icons',
;; specially `all-the-icons-install-fonts' and `nerd-icons-install-fonts'
;; helper functions.

;; TODO: check the order of this module, maybe it should be higher up in
;;       `xorns-common-systems':
;;          (select-window
;;            (display-buffer "*scratch*" nil t))

;; Enjoy!


;;; Code:

(eval-and-compile
  (require 'use-package)
  (require 'xorns-tools)
  (require 'xorns-traits)
  (require 'xorns-term))



;;; grep

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



;;; ripgrep

(>>=trait/check-obsolete >>=|ext/ripgrep ripgrep "0.11.5")


(>>=trait ripgrep ^ deadgrep
  :ensure t
  :after grep
  :init
  (defalias '>>=rg-project 'deadgrep)
  :bind
  ([remap rgrep] . >>=rg-project))


(>>=trait ripgrep | rg
  :ensure t
  :after grep
  :commands transient-get-value rg-run rg-project-root rg-read-pattern
  :init
  (defvar >>=|rg/max-columns 512
    "Override value for `--max-columns' option.")

  (rg-define-search >>=rg-project
    "Similar to `rg-project' searching for REGEXP but for all FILES."
    :dir project
    :files "*")
  :bind
  ([remap rgrep] . >>=rg-project)
  (:map rg-mode-map
    ("o" . compile-goto-error))
  :config
  (when >>=|rg/max-columns
    (let ((max (format "--max-columns=%s" >>=|rg/max-columns)))
      (setq rg-command-line-flags (cons max rg-command-line-flags)))))



;;; FZF

(>>=trait/check-obsolete >>=|ext/fzf fzf "0.11.5")
(>>=trait fzf nil
  :ensure t
  :commands fzf
  :init
  (defvar >>=|fzf/vc-names '("git" "hg")
    "List of version-control names.")

  (defun >>=fzf (&optional initial-directory)
    "Starts a `fzf' session using a smart logic.
First, try to start one of the `fzf-<VC>-files' commands for tracked files in
the current version control repository (see `>>=|fzf/vc-names' variable).  If
not in a repository, the standard `fzf' command is used as a fallback.

With a `\\[universal-argument]' prefix argument, it will prompt for the
INITIAL-DIRECTORY (the root directory for search)."
    (interactive
      (list (when current-prefix-arg (>>=read-initial-directory "FZF"))))
    (let ((default-directory (or initial-directory default-directory)))
      (or
        (catch 'done
          (dolist (vc >>=|fzf/vc-names)
            (let ((fn (>>=function/ensure (format "fzf-%s-files" vc))))
              (condition-case nil
                (throw 'done (or (funcall fn) t))
                ;; handlers
                (error nil)))))
        (fzf))))
  :bind
  ("C-c z" . >>=fzf)
  :config
  (add-to-list '>>=|term/kill-exclude-buffers "^\\*fzf"))



;;; Main search tools keys

(defvar >>=|search/main-map-key
  (if (>>=key-binding "C-z") "C-M-z" "C-z")
  "Prefix key for the main search tools (see `>>=search/main-map'.")


(defvar-keymap >>=search/main-map
  :doc "Keymap for main search tools."
  "g" '>>=counsel-project-grep
  "l" '>>=counsel-locate
  "r" '>>=rg-project
  "z" 'counsel-fzf
  "C-z" 'counsel-buffer-or-recentf
  "Z" '>>=fzf)

(keymap-global-set >>=|search/main-map-key >>=search/main-map)



;;; Dictionary servers

(use-package dictionary
  :custom
  (dictionary-server "dict.org")
  :bind
  ("C-c M-#" . dictionary-lookup-definition)
  ("C-c w" . dictionary-search))



;;; Processes and commands

(use-package proced
  :unless (eq system-type 'darwin)    ; unavailable on OS-X
  :bind
  ("C-c M-p" . proced))



;;; which-key


(>>=trait/check-obsolete >>=|which-key/enable which-key "0.11.5")

(>>=trait which-key    ;; valid-values: `top', `bottom', `left' or `right'
  :ensure t
  :demand t
  :hook
  (after-init . which-key-mode)
  :custom
  (which-key-max-description-length 38)
  :config
  (when (and (featurep 'exwm) (eq which-key-popup-type 'frame))
    (setq which-key-popup-type 'side-window))
  (when (>>=real-symbol this)
    (setq
      which-key-popup-type 'side-window
      which-key-side-window-location this
      which-key-show-prefix 'top)))



;;; Cryptography or techniques for secure communications

;; GnuPG
;; https://stackoverflow.com/questions/60812866/emacs-gpg-pinentry-el-for-authentication

(defvar >>=|crypt/gpg-integration t
  "When to configure GPG integration with Emacs.
Right now, variable `epg-pinentry-mode' is set, but other configurations would
be needed in the future..")


(use-package epg-config
  :when >>=|crypt/gpg-integration
  :demand t
  :init
  (setq epg-pinentry-mode 'loopback))



;;; Version Control Integration

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


(>>=trait/check-obsolete >>=|ext/git-forges forge "0.11.5")
(>>=trait forge
  :ensure t
  :after magit)



;;; Tree layout file explorer

(>>=trait/check-obsolete >>=|treemacs/enable treemacs "0.11.5")


(>>=trait treemacs
  :ensure t
  :defer t
  :commands treemacs-project-follow-mode treemacs-git-commit-diff-mode
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
  :when (>>=trait? treemacs)
  :after treemacs
  :ensure t)


(use-package treemacs-icons-dired
  :when (>>=trait? treemacs)
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)


(use-package treemacs-projectile
  :when (>>=trait? treemacs)
  :after (treemacs projectile)
  :ensure t)


(use-package treemacs-magit
  :when (>>=trait? treemacs)
  :after (treemacs magit)
  :ensure t)


(provide 'xorns-system)
;;; xorns-system.el ends here
