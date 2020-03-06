;;; xorns-exwm.el --- EXWM is a window manager based on Emacs

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; https://wiki.archlinux.org/index.php/EXWM

;; https://github.com/ch11ng/exwm/wiki

;;; Code:


(require 'xorns-packages)

;; TODO: DamienCassou/init.el: use-package: exwm
(>>=ensure-packages exwm)

(require 'exwm)
(require 'exwm-config)


(with-eval-after-load 'xorns-exwm
  (message ">>= using Emacs as the Desktop Window Manager.")
  (exwm-config-default)
  (->? >>=window-manager/init)
  )


(defun >>=cmd (command)
  "Execute a shell COMMAND."
  (interactive (list (read-shell-command "$ ")))
  (start-process-shell-command command nil command))


(provide 'xorns-exwm)
;;; xorns-exwm.el ends here
