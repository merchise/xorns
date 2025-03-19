;;; xorns-mode-line.el --- Control UI appearance  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; Authors:

;;   - Medardo Antonio Rodriguez (https://github.com/med-merchise/)

;;   - Manuel Vázquez Acosta (https://github.com/mvaled)

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Utilities used to configure `mode-line'.  Although the name of this module
;; is 'mode-line', it also will refer to the `header-line', and the
;; 'frame-title'.

;; You can extend the mode-line kind by configuring the trait `mode-line'
;; using a non-nil value:
;;
;; - Any non-nil will configure `minions' and `smart-mode-line' packages;
;;
;; - `mini' to use `mini-modeline';
;;
;; - `powered-smart' for complementing `smart-mode-line' with `powerline'
;;   theme (see `smart-mode-line-powerline-theme' package);
;;
;; - `doom' for `doom-modeline'; and
;;
;; - `power' (or `space'), for `spaceline' including `powerline'.


;; Enjoy!


;;; Code:

;; TODO: fix this
(eval-and-compile
  (require 'xorns-tools)
  (require 'xorns-traits)
  (require 'use-package))


(>>=trait/check-obsolete >>=|mode-line/kind mode-line "0.11.6")
(>>=trait mode-line nil)

(>>=trait mode-line
  (use-package minions
    :ensure t
    :demand t
    :commands minions-mode
    :bind
    ([S-down-mouse-3] . minions-minor-modes-menu)
    :config
    (minions-mode +1))

  (use-package smart-mode-line
    :ensure t
    :demand t
    :defines sml/battery-format
    :commands sml/setup
    :custom
    (sml/no-confirm-load-theme t)
    (sml/theme 'dark)
    (sml/name-width 30)
    (sml/read-only-char "%%")
    :config
    (defun >>-sml/get-directory (org-func &rest args)
      "Advice `sml/get-directory'."
      (if (eq major-mode 'term-mode)
        " "
        ;; else
        (apply org-func args)))
    (advice-add 'sml/get-directory :around '>>-sml/get-directory)
    (when (bound-and-true-p battery-mode-line-format)
      (setq sml/battery-format battery-mode-line-format))
    (sml/setup)))


(>>=trait mode-line = powered-smart
  ;; TODO: check for `| power smart'
  (use-package smart-mode-line-powerline-theme
    :ensure t
    :demand t
    :after smart-mode-line
    :custom
    (sml/theme 'powerline)))


(>>=trait mode-line = mini
  (use-package mini-modeline
    :ensure t
    :demand t
    :after smart-mode-line
    :defines mini-modeline-right-padding
    :commands mini-modeline-mode
    :custom
    ;; Hide all minor modes from the mode-line
    (rm-blacklist "")
    :config
    (when (featurep 'exwm)
      (add-hook 'exwm-systemtray-update-hook
        (lambda ()
          (when-let ((pixels (bound-and-true-p >>-exwm/systemtray-width)))
            (setq mini-modeline-right-padding
              (+
                (>>=get-original-value mini-modeline-right-padding)
                (round (/ (float pixels) (frame-char-width)))))))))
    (mini-modeline-mode +1)))


(>>=trait mode-line = doom
  (use-package doom-themes
    :ensure t
    :demand t
    :functions
    doom-themes-org-config
    doom-themes-visual-bell-config
    doom-themes-treemacs-config
    :custom
    (doom-dracula-brighter-comments t)
    (doom-dracula-colorful-headers t)
    (doom-dracula-comment-bg t)
    :config
    (doom-themes-treemacs-config)
    (doom-themes-visual-bell-config)
    (doom-themes-org-config))

  (use-package doom-modeline
    ;; TODO: Check `(all-the-icons-install-fonts)'
    :ensure t
    :demand t
    :defines doom-modeline-buffer-file-name-style
    :commands doom-modeline-mode doom-modeline-def-modeline
    :custom
    (doom-modeline-minor-modes t)
    (doom-modeline-battery t)
    (doom-modeline-buffer-file-name-style 'truncate-upto-root)
    :config
    (doom-modeline-mode +1)))


;; TODO: `smart-mode-line' put project together `buffer-id'
;;       ((which-function projectile-root) :separator " @ ")
;;       (require 'projectile nil 'noerror)
;; TODO: `minions' diminished `minor-modes' (`xorns-modes')
;;       ((minor-modes :separator spaceline-minor-modes-separator) :when
;;       active)
(>>=trait mode-line | '(power space)
  (use-package spaceline-config
    :ensure spaceline
    :demand t
    :defines minions-mode-line-modes
    :commands spaceline-define-segment spaceline-helm-mode spaceline-compile
    :custom
    (powerline-default-separator 'utf-8)
    (powerline-height (truncate (* 1.0 (frame-char-height))))
    :config
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
         (python-pyvenv :fallback python-pyenv))
      '(    ;; right side
         (version-control :priority 50)
         (battery :priority 75)
         (global :separator " - " :tight nil :priority 90)))
    (when (eq (bound-and-true-p >>=|minibuffer/completing-framework) 'helm)
      (spaceline-helm-mode +1))
    (when (bound-and-true-p battery-mode-line-format)
      (setq battery-mode-line-format
        ;; Function `powerline-raw' in segment 'global' reformat '%' again
        (replace-regexp-in-string "%%[]]" "%%%%]" battery-mode-line-format)))
    (setq-default mode-line-format '("%e" (:eval (spaceline-ml-xorns))))))



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
    (setq header-line-format frame-title-format)))



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
