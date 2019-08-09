;;; xorns-ui.el --- Xorns UI Library

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This is a new-age (>>=) module.  This library defines several utilities
;; used to configure UI stuffs, specially mode-lines.
;;
;; Pending tasks
;; - spaceline segments vs packages must be configured:
;;   - window-number: `winum'
;;   - python-env: `pyvenv', `pyenv' or `conda'
;;   - python-pyvenv: `pyvenv'
;;   - python-pyenv: `pyenv'

;;; Code:

(require 'easy-mmode)


(setq-default frame-title-format
  '(multiple-frames "%b"
     ("" invocation-name " -- "
       (:eval (abbreviate-file-name default-directory)))))


(defvar >>=|show-title-in-header-line nil
  "If non-nil, assign `frame-title-format' to `header-line-format'.")


(defun >>=configure-default-user-interface ()
  )


(defun >>=ui/configure-mode-line ()
  "Configure mode-line using `minions' and `spaceline'."
  (require 'use-package)
  (use-package minions
  :ensure t
  :demand t
  :config
  (unless minions-mode
    (minions-mode)))
  (use-package spaceline-config
    :ensure spaceline
    :init
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
      (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))

    (add-hook 'after-init-hook #'spaceline-xorns-theme))
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


(defun >>=ui/remove-useless-bars ()
  "Remove tool bar, menu bar, and scroll bars."
  (mapcar
    (lambda (name)
      (let ((mode (intern (format "%s-mode" name))))
	(when (and (fboundp mode) (symbol-value mode))
	  (funcall mode -1))))
    '(tool-bar menu-bar scroll-bar tooltip)))


(defun >>=frame-title-init ()
  "Configure template for displaying the title bar of visible frames.
See `frame-title-format' variable."
  ;; TODO: Spacemacs uses a function to prepare variable value
  (require 'format-spec)
  ;; TODO: Check (display-graphic-p)
  (when (and >>=|show-title-in-header-line frame-title-format)
    (setq header-line-format frame-title-format)
    ))



;; http://bzg.fr/emacs-hide-mode-line.html

(defvar-local hidden-mode-line-mode nil)
(defvar-local hide-mode-line nil)


(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global t
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))


;; If you want to hide the mode-line in every buffer by default
;; (add-hook 'after-change-major-mode-hook 'hidden-mode-line-mode)


(provide 'xorns-ui)
;;; xorns-ui.el ends here
