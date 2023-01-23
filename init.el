;;; init.el --- Xorns version of user’s initialization file

;;; Commentary:
;;
;; The file that starts all our own way to Emacs

;;; Code:

;; This file could be loaded in two different scenarios: running Emacs session
;; using `xorns' as an installed ELPA package, or in STANDALONE mode.

;; Without this comment emacs25 adds (package-initialize) here
;; (package-initialize)


(defconst >>=!xorns/emacs-min-version "27.1"
  "Minimal version of Emacs.")


(unless (version<= >>=!xorns/emacs-min-version emacs-version)
  (error
    "Emacs version '%s' is too old, Xorns requires version %s or above"
    emacs-version
    >>=!xorns/emacs-min-version))


(defconst >>=!xorns/standalone-dir
  (let ((lib-dir (expand-file-name "horns" user-emacs-directory)))
    (if (file-directory-p lib-dir) lib-dir))
  "Non-nil if `xorns' is used in standalone mode.")


(defun >>=xorns/elpa-dir ()
  "Return library directory if `xorns' is used as an ELPA installed package."
  ;; TODO: using `xorns' is used as an ELPA package is deprecated if favor of
  ;; standalone mode.
  (when-let ((pkg-info (assq 'xorns package-alist)))
    (expand-file-name
      (package-desc-full-name (cadr pkg-info))
      package-user-dir)))


(defconst >>=!xorns/lib-dir
  (or >>=!xorns/standalone-dir (>>=xorns/elpa-dir))
  "Directory containing `xorns' library (valid in both modes).")


;; Emacs might fail to start with error "Symbol's value as variable is void"
;; if `file-name-handler-alist` variable is set to nil and option
;; `--without-compress-install` was used to build Emacs.  See:
;; https://github.com/syl20bnr/spacemacs/issues/11585 and
;; https://mail.gnu.org/archive/html/emacs-devel/2022-08/msg00234.html

(defconst >>-startup-file-name-handler-alist
  (let ((needle "--without-compress-install"))
    (when (string-search needle system-configuration-options)
      file-name-handler-alist))
  "Safe value of `file-name-handler-alist' trying to improve startup time.")


(let ((file-name-handler-alist >>-startup-file-name-handler-alist))
  (when >>=!xorns/standalone-dir
    (add-to-list 'load-path >>=!xorns/standalone-dir))
  (require 'xorns))


(provide 'init)
;;; init.el ends here
