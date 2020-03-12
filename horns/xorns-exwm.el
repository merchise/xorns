;;; xorns-exwm.el --- EXWM is a window manager based on Emacs

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; https://wiki.archlinux.org/index.php/EXWM

;; https://github.com/ch11ng/exwm/wiki

;;; Code:


(require 'use-package)


(use-package exwm
  :ensure t
  :demand t
  :preface
  (defun >>-exwm/init ()
    "For the hook running when EXWM has just finished initialization."
    (display-battery-mode +1)
    (display-time-mode +1))
  :hook
  (exwm-init . >>-exwm/init))


(use-package exwm-config
  :after exwm
  :demand t
  :commands exwm-config-default
  :config
  (progn
    (message ">>= using Emacs as the Desktop Window Manager.")
    (exwm-config-default)
    (->? >>=window-manager/init)))


(use-package exwm-input
  :after exwm
  :demand t
  :commands exwm-reset
  :config
  (progn
    (exwm-input-set-key (kbd "s-r") #'exwm-reset)
    (exwm-input-set-key (kbd "<s-tab>") #'other-frame)
    ;; Bind `C-q', next key is sent literally to the application
    (add-to-list 'exwm-input-prefix-keys ?\C-q)
    (define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)
    (add-to-list 'exwm-input-prefix-keys ?\C-.)
    (add-to-list 'exwm-input-prefix-keys ?\C-,)
    (setq exwm-input-simulation-keys
      `(
	 ;; movement
	 ([?\C-b] . [left])
	 ([?\M-b] . [C-left])
	 ([?\C-f] . [right])
	 ([?\M-f] . [C-right])
	 ([?\C-\S-b] . [S-left])
	 ([?\M-\S-b] . [C-S-left])
	 ([?\C-\S-f] . [S-right])
	 ([?\M-\S-f] . [C-S-right])
	 ([?\C-p] . [up])
	 ([?\C-n] . [down])
	 ([?\C-a] . [home])
	 ([?\C-e] . [end])
	 ([?\M-v] . [prior])
	 ([?\C-v] . [next])
	 ([?\C-d] . [delete])
	 ([?\C-k] . [S-end ?\C-x])
	 ;; cut/paste, selection
	 ([?\C-w] . [?\C-x])
	 ([?\M-w] . [?\C-c])
	 ([?\C-y] . [?\C-v])
	 ([?\M-d] . [C-S-right ?\C-x])
	 ([M-backspace] . [C-S-left ?\C-x])
	 ;; search
	 ([?\C-s] . [?\C-f])
	 ;; escape
	 ([?\C-g] . [escape])
	 ([?\s-q] . [?\C-w])
	 ))))


(use-package desktop-environment
  :ensure t
  :demand t
  :after exwm-input
  :config
  (progn
    (desktop-environment-mode)))


(use-package buffer-move
  :ensure t
  :after exwm-input
  :demand t
  :config
  (progn
    (exwm-input-set-key (kbd "<s-up>") #'buf-move-up)
    (exwm-input-set-key (kbd "<s-down>") #'buf-move-down)
    (exwm-input-set-key (kbd "<s-left>") #'buf-move-left)
    (exwm-input-set-key (kbd "<s-right>") #'buf-move-right)))


(use-package exwm-systemtray
  :after exwm
  :demand t
  :commands exwm-systemtray-enable
  :config
  (progn
    (exwm-systemtray-enable)))


(use-package exwm-workspace
  :after exwm
  :demand t
  :preface
  (progn
    (defun >>-exwm/switch-workspace-0 ()
      "Move to first workspace."
      (interactive)
      (exwm-workspace-switch 0))
    (defun >>-exwm/ws-switch-left ()
      "Move to left workspace. "
      (interactive)
      (let ((current (exwm-workspace--position exwm-workspace--current)))
	(exwm-workspace-switch
	  (1- (if (> current 0) current (exwm-workspace--count))))))
    (defun >>-exwm/ws-switch-right ()
      "Move to left workspace. "
      (interactive)
      (let ((current (exwm-workspace--position exwm-workspace--current))
	    (maxws (1- (exwm-workspace--count))))
	(exwm-workspace-switch
	  (if (< current maxws) (1+ current) 0)))))
  :custom
  (exwm-workspace-show-all-buffers t)
  (exwm-layout-show-all-buffers t)
  :config
  (progn
    (exwm-input-set-key (kbd "s-.") #'>>-exwm/switch-workspace-0)
    (exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)
    (exwm-input-set-key (kbd "<C-s-left>") #'>>-exwm/ws-switch-left)
    (exwm-input-set-key (kbd "<C-s-right>") #'>>-exwm/ws-switch-right)))


(provide 'xorns-exwm)
;;; xorns-exwm.el ends here
