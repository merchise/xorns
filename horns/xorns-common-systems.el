;;; xorns-common-systems.el --- Xorns Configuration for Base System  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; It's installed just by calling `(require 'xorns-packages)' in the
;; initialization process, which is done automatically.

;; Enjoy!


;;; Code:

(require 'xorns-minibuffer)
(require 'xorns-mode-line)
(require 'xorns-buffers)
(require 'xorns-dired)
(require 'xorns-term)
(require 'xorns-system)
(require 'xorns-project)
(require 'xorns-pim)
(require 'xorns-text)
(require 'xorns-prog)
(require 'xorns-prog-extra)
(require 'xorns-devop)

(require 'xorns-bots)


(provide 'xorns-common-systems)
;;; xorns-common-systems.el ends here
