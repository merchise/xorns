;;; xorns-crypt.el --- Cryptography protocols  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;;; Commentary:

;; Configure concepts related to cryptography or techniques for secure
;; communications.

;; GnuPG: See `xorns' documentation and the page
;; https://stackoverflow.com/questions/60812866/emacs-gpg-pinentry-el-for-authentication

;; Enjoy!


;;; Code:

(defvar >>=|crypt/gpg-integration t
  "When to configure GPG integration with Emacs.
Right now, variable `epg-pinentry-mode' is set, but other configurations would
be needed in the future..")


(when >>=|crypt/gpg-integration
  (set
    (if (> emacs-major-version 26)    ; Emacs 27+
      'epg-pinentry-mode
      'epa-pinentry-mode)    ; deprecated
    'loopback))


(provide 'xorns-crypt)
;;; xorns-crypt.el ends here
