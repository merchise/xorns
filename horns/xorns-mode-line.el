;;; xorns-mode-line.el --- Control UI appearance  -*- lexical-binding: t -*-

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

(eval-and-compile
  (require 'use-package nil 'noerror)
  (require 'spaceline-config nil 'noerror)
  (require 'doom-modeline nil 'noerror)
  (require 'doom-themes-ext-org nil 'noerror)
  (require 'doom-themes-ext-visual-bell nil 'noerror)
  (require 'doom-themes-ext-treemacs nil 'noerror)
  (require 'mini-modeline nil 'noerror)
  (require 'smart-mode-line nil 'noerror))

(require 'xorns-packages)
(require 'xorns-init)



(defvar >>=|mode-line/kind nil
  "Kind of mode-line.
When nil, the standard Emacs mode-line will be used; otherwise, the value must
be a symbol: 'smart' for `smart-mode-line'; 'mini' for `mini-modeline';
'powered-smart' for complementing `smart-mode-line' with `powerline' theme
to ; 'doom' for `doom-modeline'; and 'power' or 'space', for `spaceline'
including `powerline'.")


(defvar >>=|mode-line/map-mini-kind-on-wm nil
  "When non-nil remap 'mini' `>>=|mode-line/kind' into value defined here.")


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
  ;; TODO: Use of `window-system' as a boolean is deprecated, but
  ;; `display-*-p' predicates are specific for each frame or display. I
  ;; discovered this in David Wilson's "Unlock the Power of the Daemon with
  ;; emacsclient", minute 11, https://www.youtube.com/watch?v=ZjCRxAMPdNc
  (if (display-images-p)
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
    (eval-and-compile
      (defun -sml/get-directory (org-func &rest args)
        "Advice `sml/get-directory'."
        (if (eq major-mode 'term-mode)
          ""
          ;; else
          (apply org-func args)))
      (advice-add 'sml/get-directory :around #'-sml/get-directory))
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
  (when-let ((delta (bound-and-true-p >>=|exwm/systemtray-icons)))
    (setq mini-modeline-right-padding
      (+ mini-modeline-right-padding
        (if (eq delta t)
          (max 1 (length (bound-and-true-p >>=|exwm/startup-applications)))
          ;; else
          delta))))
  (mini-modeline-mode +1))


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
  ;; TODO: Check `(all-the-icons-install-fonts)'
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
      '(    ;; left side (important stuff)
         ((buffer-modified buffer-size input-method)
          :face highlight-face
          :priority 90)
         anzu
         ((point-position line-column)
           :separator " | "
           :priority 90)
         ((buffer-id remote-host)
           :priority 90)
         ((buffer-encoding-abbrev buffer-position selection-info)
           :separator " | "
           :priority 75)
         (xorns-modes :when active :priority 15)
         process
         (flycheck-error flycheck-warning flycheck-info)
         (python-pyvenv :fallback python-pyenv)
         ;; TODO: `smart-mode-line' put project together `buffer-id'
         ;;       ((which-function projectile-root) :separator " @ ")
         ;; TODO: `minions' diminished `minor-modes' (`xorns-modes')
         ;;        ((minor-modes :separator spaceline-minor-modes-separator)
         ;;          :when active)
         )
      '(    ;; right side
         (version-control :priority 50)
         (battery :priority 75)
         (global :separator " - " :tight nil :priority 90)
         )
      )

    (if (eq (bound-and-true-p >>=|minibuffer/completing-framework) 'helm)
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



;;; Header

(defun >>=toggle-header-mode-line ()
  "Toggle visibility of header mode-line."
  (interactive)
  (if (not header-line-format)
    (setq header-line-format
      '(multiple-frames "%b"
         (" " (:eval (abbreviate-file-name default-directory)))))
    ;; else
    (setq header-line-format nil))
  (force-mode-line-update 'all))


(provide 'xorns-mode-line)
;;; xorns-mode-line.el ends here
