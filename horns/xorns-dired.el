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
;; folders starting with a dot ("."); a initial state can be configured
;; (~/.config/xorns) using the variable `>>=|dired-omit-mode'.  This variable
;; will be used to toggle this mode globally using the command
;; `>>=dired-omit-mode-toggle'.

;; Enjoy!


;;; Code:

(require 'bind-key)

(require 'use-package)
(require 'xorns-packages)


(defvar >>=|dired-omit-mode nil
  "Non-nil opens new `dired' buffers with `dired-omit-mode' enabled.")


(defvar >>=|dired-omit-extra-files '("__pycache__")
  "A list of extra files (strings) to omit from Dired listings.
This value will complement both `dired-omit-files' main custom variable and
`dired-subdir-switches' when used with `>>=dired-insert-recursive-subdir' new
command.")


(use-package dired
  :custom
  (dired-listing-switches
    (concat "-alhF"
      (let ((program (file-name-nondirectory insert-directory-program)))
	(if (or (memq system-type '(gnu gnu/linux)) (string= program "gls"))
	  " --group-directories-first -v"))))
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



;;; Extra functionality

(require 'dired-x)


(defun >>=dired-omit-mode (&optional buffer)
  "Setup `dired-omit-mode' in BUFFER using `>>=|dired-omit-mode' value."
  (with-current-buffer (or buffer (current-buffer))
    (dired-omit-mode (if >>=|dired-omit-mode +1 -1))))


(defun >>=dired-omit-mode-toggle ()
  "Toggle `>>=|dired-omit-mode' globally."
  (interactive)
  (setq >>=|dired-omit-mode (not >>=|dired-omit-mode))
  (let ((current (current-buffer)))
    (dolist (elt dired-buffers)
      (let ((buf (cdr elt)))
	(cond
	  ((null (buffer-name buf))
	    ;; Buffer is killed - clean up:
	    (setq dired-buffers (delq elt dired-buffers)))
	  (t
	    (>>=dired-omit-mode buf)))))))


(use-package dired-x
  :custom
  (dired-omit-files
    (mapconcat 'identity
      (cons
	"^\\.?#\\|^\\.[^.]\\|^\\.\\..+"
	>>=|dired-omit-extra-files)
      "\\|^"))
  (dired-omit-verbose nil)
  :hook
  (dired-mode . >>=dired-omit-mode)
  :bind
  (:map dired-mode-map
    (";" . >>=dired-omit-mode-toggle)))


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
Very similar to `dired-insert-subdir'."
  (interactive (list (dired-get-filename)))
  (dired-insert-subdir dirname
    (let ((switches (or dired-subdir-switches dired-actual-switches)))
      (concat switches
	(if (not (dired-switches-recursive-p switches))
	  " --recursive")
	(when >>=|dired-omit-mode
    	  (concat " -B "
	    (mapconcat (lambda (arg) (format "--ignore=%s" arg))
	      (append
		'(".*")
		>>=|dired-omit-extra-files
		(mapcar (lambda (arg) (format "*%s" arg))
		  dired-omit-extensions))
	      " "))))))
    (>>=dired-omit-mode))


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



;;; Patch fixing an Emacs bug

(require 'dired-aux)

(defun dired-insert-subdir-validate (dirname &optional switches)
  ;; Fix bug in dired function.  `string-match-p' is used to check the
  ;; switches instead of using `dired-check-switches' causing an error if you
  ;; try to use a value like "-laF -BR --ignore=*.bak" because the 'b' in
  ;; 'bak'.
  ;;
  ;; To report this bug see:
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Bugs.html
  (or (dired-in-this-tree dirname (expand-file-name default-directory))
      (error  "%s: not in this directory tree" dirname))
  (let ((real-switches (or switches dired-subdir-switches)))
    (when real-switches
      (mapcar
	(lambda (x)
	  (or (eq (null (dired-check-switches real-switches x))
		(null (dired-check-switches dired-actual-switches x)))
	    (error
	      "Can't have dirs with and without -%s switches together" x)))
	;; all switches that make a difference to dired-get-filename:
	'("F" "b")))))


(provide 'xorns-dired)
;;; xorns-dired.el ends here
