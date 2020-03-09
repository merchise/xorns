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
    (exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)
    (exwm-input-set-key (kbd "<s-tab>") #'other-frame)
    (exwm-input-set-key (kbd "C-;") #'other-window)
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


;; TODO: DamienCassou/init.el: use-package: exwm
;; (use-package desktop-environment


(provide 'xorns-exwm)
;;; xorns-exwm.el ends here
