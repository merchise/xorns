;;; xorns-mode-line.el --- Control UI appearance

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This library defines several utilities used to configure UI stuffs,
;; specially mode-lines.
;;
;; This module will be setup using

;;; Code:

(require 'use-package)
(require 'xorns-packages)

(>>=ensure-packages smart-mode-line)

(if mode-line-format
  (warn
    (concat
      ">>= This must be called just once and after `xorns-preface'. "
      "Variable `mode-line-format' must be nil."))
  ;; else
  (setq mode-line-format (default-value 'mode-line-format)))


(defvar >>=|mode-line/kind nil
  "Standard mode-line if nil; otherwise is a symbol indicating kind.
'mini' for `mini-modeline'; 'spaceline' for `spaceline-mode-line'; and 'power'
for the default `powerline'.  In the future new kinds could be add.")


(use-package smart-mode-line
  :custom
  (sml/no-confirm-load-theme t)
  (rm-blacklist "")
  :config
  (progn
    (sml/setup)
    ;; TODO: configure `sml/replacer-regexp-list'
    ))

;; (force-mode-line-update)



;;; spaceline

(require 'spaceline)

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



(defun >>=minor-modes-setup (kind)
  "Configure how to display minor modes in the mode-line for a specific KIND.
There are two alternatives, it is executed a function whose name is
'>>=KIND-minor-modes-setup', or 'KIND-mode' is activated."
  (let ((setup (intern (format ">>=%s-minor-modes-setup" kind)))
	(mode (intern (format "%s-mode" kind))))
    (if (fboundp setup)
      (apply setup)
      ;; else
      (if (fboundp mode)
	(apply setup 1)
	;; else
	(warn ">>= '%s' minor-modes kind can not be setup" kind)))))


;; (setq mode-line-format (default-value 'mode-line-format))

(when (eq >>=|mode-line/kind 'spaceline)
  (use-package minions
    :demand t
    :config
    (unless minions-mode
      (minions-mode)))

  (use-package spaceline-config
    :config
    (progn
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

      (spaceline-define-segment project-root
	"Show the current project root using projectile."
	(>>-project-root))

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
	  mode-line-format '("%e" (:eval (spaceline-ml-main))))))))



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
