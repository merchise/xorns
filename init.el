;;; init.el --- The Emacs Initialization File for Xorns

;;; Commentary:
;;
;; The file that starts all our own way to Emacs

;;; Code:

;; Without this comment emacs25 adds (package-initialize) here
;; (package-initialize)

(defconst emacs-start-time (current-time))

(require 'package)
(package-initialize)

(defconst >>=standalone-startup
  (if (not (package-installed-p 'xorns))    ; TODO: Try (featurep 'xorns)
    (concat
      (if load-file-name
	(file-name-directory load-file-name)
	; else
	default-directory)
      (file-name-as-directory "horns")))
  "If nil, Xorns Started up as an installed package.")


(if >>=standalone-startup
  (add-to-list 'load-path >>=standalone-startup))

(require 'xorns-packages)

(if >>=standalone-startup
  (require 'xorns-init)
  ; else
  (require 'xorns)
  (require 'xorns-extra)
  ; # TODO: Check next
  (autoload 'po-mode "po-mode"
    "Major mode for translators to edit PO files" t)
  )

(provide 'init)
;;; init.el ends here
