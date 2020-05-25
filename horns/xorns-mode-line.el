;;; xorns-mode-line.el --- Control UI appearance

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Utilities used to configure mode-line.  Although the name of this module is
;; 'mode-line', it also will refer to the 'header-line', and the
;; 'frame-title'.  You can change the mode-line kind by using a non-nil value
;; for `>>=|mode-line/kind'.

;; Enjoy!


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
When nil, the standard Emacs mode-line will be used; otherwise, the value must
be a symbol: 'smart' for `smart-mode-line'; 'mini' for `mini-modeline';
'powered-smart' for complementing `smart-mode-line' with `powerline' theme
to ; 'doom' for `doom-modeline'; and 'power' or 'space', for `spaceline'
including `powerline'.")


(defvar >>=|mode-line/show-system-status nil
  "Kind of mode-line.
When non-nil, force mode-line visualization of several system statuses like
battery and time.")


(defvar >>=|mode-line/map-mini-kind-on-wm nil
  "When non-nil remap 'mini' `>>=|mode-line/kind' into value defined here.")


(when (or >>=|mode-line/show-system-status (bound-and-true-p >>=!emacs-as-wm))
  (display-battery-mode +1)
  (display-time-mode +1))


(when (and >>=|mode-line/map-mini-kind-on-wm (eq >>=|mode-line/kind 'mini))
  (setq-default >>=|mode-line/kind >>=|mode-line/map-mini-kind-on-wm))


(use-package minions
  ;; :when (not (eq >>=|mode-line/kind 'mini))
  :ensure t
  :bind
  ([S-down-mouse-3] . minions-minor-modes-menu)
  :hook
  (after-init . minions-mode)
  :config
  (if window-system
    (setq minions-mode-line-lighter "◆")))


(use-package smart-mode-line
  :when (member >>=|mode-line/kind '(smart mini powered-smart power space))
  :ensure t
  :demand t
  :custom
  (sml/no-confirm-load-theme t)
  (sml/theme 'respectful)
  (sml/name-width 30)
  :config
  (progn
    (when (bound-and-true-p battery-mode-line-format)
      (setq sml/battery-format battery-mode-line-format))
    (sml/setup)))


(use-package smart-mode-line-powerline-theme
  :when (eq >>=|mode-line/kind 'powered-smart)
  :ensure t
  :demand t
  :after smart-mode-line)


(use-package mini-modeline
  :when (eq >>=|mode-line/kind 'mini)
  :ensure t
  :demand t
  :after smart-mode-line
  :custom
  ;; Hide all minor modes from the mode-line
  (rm-blacklist "")
  :config
  (progn
    (let ((delta (bound-and-true-p >>=|exwm/systemtray-icons)))
      (if delta
	(setq mini-modeline-right-padding
	  (+ mini-modeline-right-padding
	    (if (eq delta t)
	      (length (bound-and-true-p >>=|exwm/startup-applications))
	      ;; else
	      delta)))))
    (mini-modeline-mode +1)))


(use-package doom-themes
  :when (eq >>=|mode-line/kind 'doom)
  :ensure t
  :custom
  (doom-dracula-brighter-comments t)
  (doom-dracula-colorful-headers t)
  (doom-dracula-comment-bg t)
  :config
  (progn
    (doom-themes-treemacs-config)
    (doom-themes-visual-bell-config)
    (doom-themes-org-config)))


(use-package doom-modeline
  :when (eq >>=|mode-line/kind 'doom)
  :ensure t
  :config
  (progn
    (declare-function doom-modeline-def-modeline 'doom-modeline-core)
    (doom-modeline-def-modeline 'main
      '(buffer-info remote-host buffer-position)
      '(objed-state misc-info vcs checker input-method buffer-encoding lsp
	 major-mode process " "))
    (setq doom-modeline-buffer-file-name-style 'truncate-upto-root)
    (doom-modeline-mode +1)))


(use-package spaceline-config
  :when (member >>=|mode-line/kind '(power space))
  :ensure spaceline
  :custom
  (powerline-default-separator 'utf-8)
  (powerline-height (truncate (* 1.0 (frame-char-height))))
  :config
  (progn
    (declare-function spaceline-compile 'spaceline)

    (spaceline-define-segment xorns-modes
      "A minions menu for minor modes."
      (format-mode-line minions-mode-line-modes))

    (spaceline-compile 'xorns
      ;; left side (important stuff)
      '( (((persp-name :fallback workspace-number) window-number)
	   :separator "|")
	 ((buffer-modified buffer-size input-method)
	   ;; :face highlight-face
	   )
	 (anzu :priority 95)
	 '(buffer-id remote-host buffer-encoding-abbrev)
	 `((selection-info :priority 95)
	   ((xorns-modes) :when active :priority 79)
	   ((macrodef point-position line-column)
	     :separator " | " :when active :priority 96)
	   ((narrow buffer-position hud :priority 99)
	     ;; :face highlight-face
	     )
	    )
	 ((flycheck-error flycheck-warning flycheck-info)
	   :when active :priority 89)
	 (python-pyvenv :fallback python-pyenv)
	 ((which-function projectile-root) :separator " @ ")
	 )
      ;; right side
      '( (version-control :when active :priority 78)
	 ;; (purpose :priority 94)
	 (battery :when active)
	 (global :when active :separator " - " :tight nil)
	 ))

    (if (eq >>=|minibuffer/completing-framework 'helm)
      (spaceline-helm-mode +1))

    (setq-default mode-line-format '("%e" (:eval (spaceline-ml-xorns))))

    (when (bound-and-true-p battery-mode-line-format)
      (setq battery-mode-line-format
	;; Function `powerline-raw' in segment 'global' reformat '%' again
	(replace-regexp-in-string "%%[]]" "%%%%]" battery-mode-line-format)))
    ))



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
