;;; xorns-building-blocks.el --- Merchise extensions for Emacs  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;;; Commentary:

;; Enjoy!


;;; Code:

(eval-and-compile
  (require 'xorns-tools))


(unless (>>=value-of >>=!xorns/standalone-dir)
  ;; TODO: mail has not yet been migrated to standalone mode
  (require 'xorns-mail))


(provide 'xorns-building-blocks)
;;; xorns-building-blocks.el ends here
