;;; init.el --- The Emacs Initialization File for Xorns

;;; Commentary:
;;
;; The file that starts all our own way to Emacs

;;; Code:

;; Without this comment emacs25 adds (package-initialize) here
;; (package-initialize)

(defconst emacs-start-time (current-time))

; Initialize Xorns in standalone mode or as an installed package

(defconst >>=standalone-startup
  (let ((pkg-dir
	  (concat
	    (if load-file-name
	      (file-name-directory load-file-name)
	      ; else
	      default-directory)
	    (file-name-as-directory "horns"))))
    (if (file-directory-p pkg-dir) pkg-dir))
  "If nil, Xorns Started up as an installed package.")


(if >>=standalone-startup
  (add-to-list 'load-path >>=standalone-startup))

(require 'xorns-init)

(provide 'init)
;;; init.el ends here
