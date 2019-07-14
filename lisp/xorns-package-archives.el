;;; xorns-package-archives --- ELPA repository for packages supported by Xorns

;; Copyright (c) Merchise Autrement [~º/~]


;;; Code:

(require 'package)

(setq package-archives
  '(("melpa" . "https://melpa.org/packages/")
    ("org" . "https://orgmode.org/elpa/")
    ("gnu" . "https://elpa.gnu.org/packages/")))

(provide 'xorns-package-archives)
;;; xorns-package-archives.el ends here
