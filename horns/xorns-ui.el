;;; xorns-ui.el --- Control UI appearance

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; New-age (>>=) module.  This library defines several utilities used to
;; configure UI stuffs, specially mode-lines.
;;
;; A common setup for this module is:
;;
;;   (use-package xorns-ui
;;     :hook
;;     (after-init . spaceline-xorns-theme))
;;
;; Pending tasks
;; - spaceline segments vs packages must be configured:
;;   - window-number: `winum'
;;   - python-env: `pyvenv', `pyenv' or `conda'
;;   - python-pyvenv: `pyvenv'
;;   - python-pyenv: `pyenv'
;;
;; See: seagle0128/lisp/init-ui.el, pierre-lecocq/modules/feat-theme.el,
;; doom-emacs/core/core-ui.el, doom-emacs/early-init.el,
;; purcell/lisp/init-gui-frames.el, raxod502/emacs/radian.el,
;; thierryvolpiatto/init.el

;;; Code:

; (require 'easy-mmode)

(require 'use-package)
(require 'xorns-packages)


(setq-default frame-title-format
  '(multiple-frames "%b"
     ("" invocation-name " -- "
       (:eval (abbreviate-file-name default-directory)))))


(defvar >>=|show-title-in-header-line nil
  "If non-nil, assign `frame-title-format' to `header-line-format'.")


(use-package minions
  :ensure t
  :demand t
  :config
  (unless minions-mode
    (minions-mode)))


(use-package spaceline-config
  :ensure spaceline
  :config
  (require 'spaceline)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-modified)

  (spaceline-define-segment narrow
    "Show when buffer is narrowed."
    (when (buffer-narrowed-p)
      "Narrowed"))

  (spaceline-define-segment minions
    "A minions menu for minor modes."
    (if (bound-and-true-p minions-mode)
      (format-mode-line minions-mode-line-modes)
      ; else
      (spaceline-minor-modes-default)))
  )

(defun spaceline-xorns-theme ()
  "Install a variation of `spaceline-emacs-theme'."
  (spaceline-install
    `((((persp-name :fallback workspace-number)
	 window-number) :separator "|")
      ((buffer-modified) :face highlight-face)
      ((buffer-id which-function)
	:separator " @ " :face highlight-face :tight-left t)
      remote-host
      projectile-root
      ((buffer-size) :separator " | " :when active)
      (version-control :when active))
    `(selection-info
      ((process minions) :when active)
      ((,(if nil 'buffer-encoding 'buffer-encoding-abbrev)
	macrodef
	point-position
	line-column)
       :separator " | " :when active)
       ((narrow buffer-position hud) :face highlight-face)
      )
    )

  (setq-default spaceline-buffer-encoding-abbrev-p t)
  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main))))
  )


(defun >>=ui/toggle-header-mode-line ()
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




(use-package frame
  :config
  (if window-system
    (progn
      ; Start Emacs maximized
      (set-frame-parameter nil 'undecorated t)
      (add-to-list 'default-frame-alist '(undecorated . t))
      (unless (frame-parameter nil 'fullscreen)
	(toggle-frame-maximized))
      ; TODO: Check if both are needed
      (let ((no-border '(internal-border-width . 0))
	    (full-screen '(fullscreen . maximized)))
	(add-to-list 'default-frame-alist no-border)
	(add-to-list 'initial-frame-alist no-border)
	(add-to-list 'default-frame-alist full-screen)
	(add-to-list 'initial-frame-alist full-screen)))
    ; else
    (progn
      ; Enable mouse support when running in a console
      (require 'mouse)
      (xterm-mouse-mode t)
      (defun track-mouse (e))
      (setq mouse-sel-mode t)
      (global-set-key [mouse-4]
	(lambda () (interactive) (scroll-down 1)))
      (global-set-key [mouse-5]
	(lambda () (interactive) (scroll-up 1))))
    ))


(provide 'xorns-ui)
;;; xorns-ui.el ends here
