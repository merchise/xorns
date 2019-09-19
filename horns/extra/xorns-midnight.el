;;; xorns-midnight.el --- Xorns configuration for midnight-mode

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Experimental module to document a feature to develop in the future.
;;
;; More documentation in:
;; - https://www.emacswiki.org/emacs/MidnightMode
;; - https://fortune-teller-amy-88756.netlify.com/knusper

;;; Code:

(require 'use-package)


(use-package midnight
  :bind ("C-c z" . clean-buffer-list)
  ;; :hook (midnight . clean-buffer-list)
  :custom
  (clean-buffer-list-delay-general 1)      ; one day
  (clean-buffer-list-delay-special 900)    ; 15 minutes
  (clean-buffer-list-kill-regexps '("^[*].*"))
  ;; Check: '("^ \\*Minibuf-.*\\*$" "^\\*Summary" "^\\*Article" "^#")
  (clean-buffer-list-kill-never-regexps
   '("^\\([#]\\|[*]\\(scratch\\|Messages\\)\\).*"))
  :config
  (progn
    (defun >>=disable-midnight-mode ()
      "Disable midnight mode."
      (interactive)
      (cancel-timer midnight-timer))
    (midnight-delay-set 'midnight-delay "4:30am")))


(provide 'xorns-midnight)
;;; xorns-midnight.el ends here
