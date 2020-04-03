;;; xorns-mode-line.el --- Control UI appearance

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Utilities used to configure mode-line.  Although the name of this module is
;; 'mode-line', it also will refer to the 'header-line', and the
;; 'frame-title'.  You can change the mode-line kind by using a non-nil value
;; for `>>=|mode-line/kind'.

;;; Code:


(require 'xorns-packages)
(require 'use-package)


(if mode-line-format
  (warn
    (concat
      ">>= Variable `mode-line-format' must be nil at this point; "
      "this code must be called just once and after `xorns-preface'."))
  ;; else
  (setq mode-line-format (default-value 'mode-line-format)))


(defvar >>=|mode-line/kind nil
  "Kind of mode-line.
When nil, the standard Emacs mode-line will be used without change; otherwise
is a symbol indicating kind, 'smart' for `smart-mode-line' removing all
minor-modes indicators, 'mini' for `mini-modeline', 'power' for `powerline',
or 'space' for `spaceline'.")


(use-package minions
  :when (and >>=|mode-line/kind (not (eq >>=|mode-line/kind 'mini)))
  :ensure t
  :bind
  ([S-down-mouse-3] . minions-minor-modes-menu)
  :hook
  (after-init . minions-mode)
  :config
  (if window-system
    (setq minions-mode-line-lighter "◆")))


(use-package smart-mode-line
  :when (or (eq >>=|mode-line/kind 'smart) (eq >>=|mode-line/kind 'mini))
  :ensure t
  :demand t
  :custom
  (sml/no-confirm-load-theme t)
  (sml/theme 'respectful)
  :config
  (progn
    (if (bound-and-true-p >>=emacs-as-wm)
      (display-time-mode +1)
      ;; else
      (setq rm-blacklist ""))
    (sml/setup)))


(use-package mini-modeline
  :when (eq >>=|mode-line/kind 'mini)
  :ensure t
  :demand t
  :after smart-mode-line
  :custom
  (mini-modeline-enhance-visual t)
  :config
  (mini-modeline-mode +1))


(use-package powerline
  :when (eq >>=|mode-line/kind 'power)
  :ensure t
  :custom
  (powerline-default-separator 'utf-8)
  :config
  (powerline-default-theme))



;;; spaceline

(when (eq >>=|mode-line/kind 'space)
  (defvar >>-project-root nil
    "Local variable to store cached `projectile-project-name'.")

  (declare-function projectile-project-name 'projectile)

  (defun >>-project-root ()
    "Local function to calculate and cache `projectile-project-name'."
    (when (and (not >>-project-root) (fboundp 'projectile-project-name))
      (let ((name (projectile-project-name)))
	(set (make-local-variable '>>-project-root)
	  (if (string= name (buffer-name))
	    "-"
	    ;; else
	    name))))
    (unless (string= >>-project-root "-")
      >>-project-root))

  (use-package spaceline-config
    :ensure spaceline
    :init
    (progn
      (require 'spaceline)
      (setq spaceline-highlight-face-func 'spaceline-highlight-face-modified)
      (declare-function spaceline-install 'spaceline)

      (spaceline-define-segment narrow
	"Show when buffer is narrowed."
	(when (buffer-narrowed-p)
	  "Narrowed"))

      (spaceline-define-segment minions
	"A minions menu for minor modes."
	(if (bound-and-true-p minions-mode)
	  (format-mode-line minions-mode-line-modes)
	  ;; else
	  (spaceline-minor-modes-default)))

      (spaceline-define-segment project-root
	"Show the current project root using projectile."
	(>>-project-root))

      (add-hook 'after-init-hook
	(defun spaceline-xorns-theme ()
	  "Install a variation of `spaceline-emacs-theme'."
	  (spaceline-install
	    `((((persp-name :fallback workspace-number)
		 window-number) :separator "|")
	       ((buffer-modified) :face highlight-face)
	       ((buffer-id which-function)
		 :separator " @ " :face highlight-face :tight-left t)
	       remote-host
	       ;; projectile-root
	       project-root
	       ((buffer-size) :separator " | " :when active)
	       (version-control :when active))
	    `(selection-info
	       ((process minions) :when active)
	       ((,(if nil 'buffer-encoding 'buffer-encoding-abbrev)
		  macrodef
		  point-position
		  line-column)
		 :separator " | " :when active)
	       ((narrow buffer-position hud) :face highlight-face)))
	  (setq-default
	    spaceline-buffer-encoding-abbrev-p t
	    mode-line-format '("%e" (:eval (spaceline-ml-main))))))
      )))



;;; header-line

(setq-default frame-title-format
  '(multiple-frames "%b"
     ("" invocation-name " -- "
       (:eval (abbreviate-file-name default-directory)))))


(defvar >>=|show-title-in-header-line nil
  "If non-nil, assign `frame-title-format' to `header-line-format'.")


(defun >>=ui/toggle-header-mode-line ()
  "Toggle if the header line appears or not."
  (interactive)
  (if (not header-line-format)
      (setq header-line-format
	'(multiple-frames "%b"
	   (" " (:eval (abbreviate-file-name default-directory)))))
    ; else
    (setq header-line-format nil))
  (force-mode-line-update 'all))


(defun >>=frame-title-init ()
  "Configure template for displaying the title bar of visible frames.
See `frame-title-format' variable."
  ;; TODO: Spacemacs uses a function to prepare variable value
  (require 'format-spec)
  ;; TODO: Check (display-graphic-p)
  (when (and >>=|show-title-in-header-line frame-title-format)
    (setq header-line-format frame-title-format)
    ))


(provide 'xorns-mode-line)
;;; xorns-mode-line.el ends here
