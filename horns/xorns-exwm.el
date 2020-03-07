;;; xorns-exwm.el --- EXWM is a window manager based on Emacs

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; https://wiki.archlinux.org/index.php/EXWM

;; https://github.com/ch11ng/exwm/wiki

;;; Code:


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


;; TODO: DamienCassou/init.el: use-package: exwm
;; (use-package exwm-input


(provide 'xorns-exwm)
;;; xorns-exwm.el ends here
