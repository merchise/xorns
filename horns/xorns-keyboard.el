;;; xorns-keyboard.el --- Merchise keyboard extensions for Emacs

;; Copyright (c) Merchise Autrement [~º/~]

;;; Commentary:

;; Configure keyboard extensions.

;; Enjoy!


;;; Code:

(require 'use-package)
(require 'xorns-packages)


(defvar >>=|typing-delay 0.16
  "Max time delay between two press to consider a `key-chord'.")


(>>=ensure-packages use-package-chords)


(use-package use-package-chords
  :config
  (progn
    (setq-default
      key-chord-two-keys-delay (/ >>=|typing-delay 5)
      key-chord-one-key-delay >>=|typing-delay)
    (key-chord-mode 1)))



(provide 'xorns-keyboard)
;;; xorns-keyboard.el ends here
