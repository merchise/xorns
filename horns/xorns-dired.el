;;; xorns-dired.el --- Merchise extensions for `dired'  -*- lexical-binding: t -*-
;; TODO:  -*- checkdoc-verb-check-experimental-flag:nil -*-

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
;; (~/.config/xorns) using the variable `>>=|dired/omit-mode'.  This variable
;; will be used to toggle this mode globally using the command
;; `>>=dired-omit-mode-toggle'.

;; Enjoy!


;;; Code:

(require 'bind-key)
(require 'use-package)
(require 'xorns-tools)
(require 'xorns-packages)


(define-obsolete-variable-alias
  '>>=|dired-omit-mode '>>=|dired/omit-mode "1.0")
(defvar >>=|dired/omit-mode nil
  "Non-nil opens new `dired' buffers with `dired-omit-mode' enabled.")


(define-obsolete-variable-alias
  '>>=|dired-omit-extra-files '>>=|dired/omit-extra-files "1.0")
(defvar >>=|dired/omit-extra-files '("__pycache__")
  "A list of extra files (strings) to omit from Dired listings.
This value will complement both `dired-omit-files' main custom variable and
`dired-subdir-switches' when used with `>>=dired-insert-recursive-subdir' new
command.")



;;; Main modules

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


(defvar >>=|dired-omit-ignores-switches
  (concat "-B "
    (mapconcat (lambda (arg) (format "--ignore='%s'" arg))
      (append
        '(".*")
        >>=|dired/omit-extra-files
        (mapcar
          (lambda (arg)
            (let ((wild (if (string-match-p "/$" arg) "" "*")))
              (concat wild arg)))
          (seq-filter (lambda (arg) (not (string-match-p "^\\." arg)))
            dired-omit-extensions))
        )
      " "))
  "Ignore switches when listing directory if omit-mode and recursive.")


(defun >>=dired-omit-mode (&optional buffer)
  "Setup `dired-omit-mode' in BUFFER using `>>=|dired/omit-mode' value."
  (with-current-buffer (or buffer (current-buffer))
    (dired-omit-mode (if >>=|dired/omit-mode +1 -1))))


(defun >>=dired-omit-mode-toggle ()
  "Toggle `>>=|dired/omit-mode' globally."
  (interactive)
  (setq >>=|dired/omit-mode (not >>=|dired/omit-mode))
  ;; TODO: check why `_current' is unused
  (let ((_current (current-buffer)))
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
        >>=|dired/omit-extra-files)
      "\\|^"))
  (dired-omit-verbose nil)
  :hook
  (dired-mode . >>=dired-omit-mode)
  :bind
  (:map dired-mode-map
    ("." . >>=dired-omit-mode-toggle)))


(use-package wdired
  :custom
  (wdired-allow-to-change-permissions t))



;;; Reuse the current dired buffer to visit a directory

(>>=package/ensure 'dired-single)    ; TODO: check `>>=package/config'
(require 'dired-single)


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
    (concat (or dired-subdir-switches dired-actual-switches) " -R "
      (if >>=|dired/omit-mode >>=|dired-omit-ignores-switches)))
  (>>=dired-omit-mode))


(defun >>=dired-maybe-insert-subdir (dirname &optional switches)
  "Insert sub-directory DIRNAME into the same dired buffer.
If SWITCHES contains recursive flag (see `dired-switches-recursive-p') and
global variable `>>=|dired/omit-mode' is t, `>>=|dired-omit-ignores-switches'
are concatenated.  See `dired-maybe-insert-subdir'."
  (interactive
    (list
      (dired-get-filename)
      (if current-prefix-arg
        (read-string "Switches for listing: "
          (or dired-subdir-switches dired-actual-switches)))))
  (let ((opoint (point))
        (dirname (file-name-as-directory dirname)))
    (when (and >>=|dired/omit-mode (dired-switches-recursive-p switches))
      (setq switches (concat switches " " >>=|dired-omit-ignores-switches)))
    (or (and (not switches)
             (when (dired-goto-subdir dirname)
               (unless (dired-subdir-hidden-p dirname)
                 (dired-initial-position dirname))
               t))
        (dired-insert-subdir dirname switches))
    (push-mark opoint))
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
  ("i" . >>=dired-maybe-insert-subdir)
  ([M-S-up] . dired-single-up-directory)
  ([M-up] . dired-single-up-directory)
  ([mouse-1] . dired-single-buffer-mouse)
  ([mouse-2] . dired-single-buffer-mouse))



;;; Patch to fix `dired-aux' bug

(require 'dired-aux)

;; Next fucntion is a replacement of the original one because `string-match-p'
;; is used to check switches instead of `dired-check-switches' causing an
;; error if you try to use a value like '-laF -BR --ignore=*.bak' because the
;; 'b' in 'bak'.
;;
;; TODO: try to advice original function.

(defun dired-insert-subdir-validate (dirname &optional switches)
  "Check that it is valid to insert DIRNAME with SWITCHES.
Signal an error if invalid (e.g. user typed `i' on `..')."
  (or
    (>>=file-in-dir-tree dirname default-directory)
    (error  "%s: not in this directory tree" dirname))
  (let ((real-switches (or switches dired-subdir-switches)))
    (when real-switches
      (let (case-fold-search)
        (mapcar
          (lambda (x)
            (or
              (eq
                ;; was: (string-match-p x real-switches)
                (null (dired-check-switches real-switches x))
                ;; was: (string-match-p x dired-actual-switches)
                (null (dired-check-switches dired-actual-switches x)))
              (error
                "Can't have dirs with and without -%s switches together" x)))
          ;; all switches that make a difference to dired-get-filename:
          '("F" "b"))))))


(defadvice dired-replace-in-string
  (around >>-dired-replace-in-string (regexp newtext string) activate)
  "Fix one character switch replace."
  (let ((org regexp) (res string))
    (when (and (eq (string-width regexp) 1) (string-equal newtext ""))
      (setq regexp (format " ?\\_<-%s\\_>" regexp))
      (setq res ad-do-it))
    (if (string-equal res string)
      (progn
        (setq regexp org)
        ad-do-it)
      ;; else
      res)))


(provide 'xorns-dired)
;;; xorns-dired.el ends here
