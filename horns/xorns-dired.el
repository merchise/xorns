;;; xorns-dired.el --- Merchise extensions for `dired'

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;;; Commentary:

;; Configure and extend all `dired' dependencies in Merchise way.
;;
;; `dired-single' is improved by moving point to last folder when navigating
;; up.
;;
;; `dired-omit-mode' use a more consistent method by hiding only files and
;; folders starting with a dot ("."); also initial state can be set using the
;; variable `>>=|initial-dired-omit-mode'.

;; Enjoy!


;;; Code:

(require 'bind-key)

(require 'use-package)
(require 'xorns-packages)


(defconst >>-listing-switches
  (concat "-alhF"
    (if (or (memq system-type '(gnu gnu/linux))
	  (string= (file-name-nondirectory insert-directory-program) "gls"))
      " --group-directories-first -v"))
  "Calculate default value for switches passed to `ls' for dired.")


(defvar >>=|initial-dired-omit-mode nil
  "Non-nil opens new `dired' buffers with `dired-omit-mode' enabled.")


(use-package dired
  :custom
  (dired-listing-switches >>-listing-switches)
  (dired-ls-F-marks-symlinks t)
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  (dired-guess-shell-alist-user
    '(("\\(\\.ods\\|\\.xlsx?\\|\\.docx?\\|\\.csv\\)\\'" "libreoffice")))
  (dired-auto-revert-buffer t)
  (dired-recursive-copies 'always)
  (dired-dwim-target t)
  (dired-isearch-filenames 'dwim)
  :config
  ;; Enable a disabled command (assign to "M-RET")
  (put 'dired-find-alternate-file 'disabled nil))


(use-package dired-x
  :after dired
  :custom
  (dired-omit-files "^\\.?#\\|^\\.[^.]\\|^\\.\\..+\\|^__pycache__$")
  (dired-omit-verbose nil)
  :hook
  (dired-mode . >>-dired-omit/setup)
  :config
  (progn
    (defun >>-dired-omit/setup ()
      "Setup initial `dired-omit-mode'."
      (interactive)
      (if >>=|initial-dired-omit-mode
	(dired-omit-mode)))

    (bind-keys :map dired-mode-map
      ;; An error occurred with use-package's `:bind'
      (";" . dired-omit-mode))))


(use-package wdired
  :custom
  (wdired-allow-to-change-permissions t))



;;; Reuse the current dired buffer to visit a directory

(>>=require dired-single)


(defun >>=dired-search-forward (target)
  "Search forward from point for directory entry TARGET."
  (when (and target
	  (re-search-forward (format "[[:space:]]%s[/\n]" target) nil t))
    (left-char (1+ (length target)))
    (point)))


(defun >>=dired-single-reload (&optional arg)
  "Reload current-directory in the Dired buffer.
Non-nil ARG will reload the inserted sub-directory where the point is
located."
  (interactive "P")
  (unless (eq major-mode 'dired-mode)
    (error ">>= this function can only be called in `dired-mode'"))
  (let ((pos (point))
	(fname (dired-get-filename 'no-dir 'no-error)))
    (if (null arg)
      (goto-char (point-min)))
    (dired-single-buffer (dired-current-directory))
    (if (and (null (>>=dired-search-forward fname)) (< pos (point-max)))
      (goto-char pos))))


(defun >>=dired-insert-recursive-subdir (dirname)
  "Insert sub-directory DIRNAME into the same buffer using recursive options.
Very similar to `dired-maybe-insert-subdir'."
  (interactive (list (dired-get-filename)))
  (dired-maybe-insert-subdir dirname
    (let ((switches (or dired-subdir-switches dired-actual-switches)))
      (if (dired-switches-recursive-p switches)
	switches
	;; else (add recursive option)
	(concat switches " -R")))))


(defadvice dired-single-buffer (around >>-dired-single-buffer activate)
  "Select source directory item position when navigating up."
  (interactive)
  (let ((org (dired-current-directory)))
    ;; super
    ad-do-it
    ;;
    (let ((dst (dired-current-directory)))
      (if (string-prefix-p dst org)
        (let* ((targets (split-string (substring org (length dst)) "/"))
                (aux (car targets))
                (target (if (string= aux "") (cadr targets) aux)))
	  (goto-char (point-min))
	  (if (null (>>=dired-search-forward target))
	    (dired-next-line 4)))))))


(bind-keys :map dired-mode-map
  ([return] . dired-single-buffer)
  ([M-S-down] . dired-single-buffer)
  ([M-down] . dired-single-buffer)
  ("^" . dired-single-up-directory)
  ("M-P" . dired-single-up-directory)
  ("/" . >>=dired-single-reload)
  ("r" . >>=dired-insert-recursive-subdir)
  ([M-S-up] . dired-single-up-directory)
  ([M-up] . dired-single-up-directory)
  ([mouse-1] . dired-single-buffer-mouse)
  ([mouse-2] . dired-single-buffer-mouse))


(provide 'xorns-dired)
;;; xorns-dired.el ends here
