;;; init.el --- Xorns version of user’s initialization file

;;; Commentary:
;;
;; The file that starts all our own way to Emacs

;;; Code:

;; Without this comment emacs25 adds (package-initialize) here
;; (package-initialize)

(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)


(defconst >>=base-dir
  (concat
    (if load-file-name
      (file-name-directory load-file-name)
      ;; else
      default-directory))
  "Base 'xorns' directory.")


(defconst >>=standalone-startup
  (if (not (assq 'xorns package-alist))
    ;; xorns is not installed as a package
    (expand-file-name "horns" >>=base-dir))
  "If nil, Emacs started up with `xorns' as an installed package.")


(load (expand-file-name "xorns.lock" >>=base-dir) nil (not init-file-debug))


(if >>=standalone-startup
  (add-to-list 'load-path >>=standalone-startup))


(require 'xorns)


(provide 'init)
;;; init.el ends here
