;;; xorns-versions.el --- Xorns Versions

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This library defines this package version and the required minimal version
;; of Emacs.

;;; Code:

(defconst xorns-version "0.2.0")
(defconst >>=!emacs-min-version "26.1")

(if (not (version<= >>=!emacs-min-version emacs-version))
  (error "Xorns '%s' requires Emacs version >='%s'"
    xorns-version >>=!emacs-min-version))

(provide 'xorns-versions)
;;; xorns-versions.el ends here
