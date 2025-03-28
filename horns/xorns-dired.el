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

(eval-and-compile
  (require 'dired)
  (require 'xorns-tools))


(defvar >>=|dired/omit-mode nil
  "Non-nil opens new `dired' buffers with `dired-omit-mode' enabled.")


(defvar >>=|dired/omit-extra-files '("__pycache__")
  "A list of extra files (strings) to omit from Dired listings.
This value will complement both `dired-omit-files' main custom variable and
`dired-subdir-switches' when used with `>>=dired-insert-recursive-subdir' new
command.")


(defvar >>=|dired/force-group-directories-first t
  "Advice `dired' functions if ls command don't support this option.")


(defvar-local >>-dired/sorted-directories nil
  "A list of already sorted directories.
This variable is only used when `dired' functions are adviced, see variable
`>>=|dired/force-group-directories-first'."  )


(defsubst >>-group-directories-first-p ()
  "Check if there is support for group-directories-first flag."
  (or
    (memq system-type '(gnu gnu/linux))
    (string= (file-name-nondirectory insert-directory-program) "gls")))



;;; Main modules

(use-package dired
  :demand t
  :bind
  (:map ctl-x-map
    ("C-d" . dired))
  (:map dired-mode-map
    ("M-RET" . dired-find-alternate-file))
  :custom
  (dired-listing-switches "-alhF")
  (dired-ls-F-marks-symlinks t)
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  (dired-guess-shell-alist-user
    '(("\\(\\.odt\\|\\.ods\\|\\.xlsx?\\|\\.docx?\\|\\.csv\\)\\'" "libreoffice")))
  (dired-auto-revert-buffer t)
  (dired-recursive-copies 'always)
  (dired-dwim-target t)
  (dired-isearch-filenames 'dwim)
  :config
  (put 'dired-find-alternate-file 'disabled nil)    ; re-enable it for "M-RET"

  (defun >>-dired-readin-sort ()
    "Used when `>>=|dired/force-group-directories-first' is demanded."
    (save-excursion
      (let (buffer-read-only
            start
            (end (point-max))
            (saved (point)))
        (unwind-protect
          (dolist (item dired-subdir-alist)
            (let ((dir-name (car item))
                  (start-marker (cdr item)))
              (goto-char start-marker)
              (setq start (point))
              (when (not (member dir-name >>-dired/sorted-directories))
                (forward-line 2)
                (sort-regexp-fields t "^.*$" "[ ]*." (point) end)
                (setq >>-dired/sorted-directories
                  (cons dir-name >>-dired/sorted-directories))))
            (setq end (1- start)))
          ;; finally
          (goto-char saved)
          (set-buffer-modified-p nil)))))

  (defun >>=dired-search-forward (target)
    "Search forward from point for directory entry TARGET."
    (when (and target
            (re-search-forward (format "[[:space:]]%s[/\n]" target) nil t))
      (left-char (1+ (length target)))
      (point)))

  (if (>>-group-directories-first-p)
    (setq dired-listing-switches
      (concat dired-listing-switches " --group-directories-first -v"))
    ;; else
    (when >>=|dired/force-group-directories-first
      ;; See https://www.emacswiki.org/emacs/DiredSortDirectoriesFirst
      (advice-add 'dired-readin :after '>>-dired-readin-sort)
      (advice-add 'dired-insert-subdir :after '>>-dired-readin-sort)
      (advice-add 'dired-revert :before
        (lambda () (setq >>-dired/sorted-directories nil))))))


(use-package dired-x
  :demand t
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
    ("." . >>=dired-omit-mode-toggle))
  :config
  (defun >>-dired-omit-ignores-switches ()
  "Ignore switches when listing directory if omit-mode and recursive."
  (concat "-B "
    (mapconcat (lambda (arg) (format "--ignore='%s'" arg))
      (append
        '(".*")
        >>=|dired/omit-extra-files
        (mapcar
          (lambda (arg)
            (let ((wild (if (string-match-p "/$" arg) "" "*")))
              (concat wild arg)))
          (seq-filter
            (lambda (arg) (not (string-match-p "^\\." arg)))
            dired-omit-extensions))
        )
      " ")))

  (defun >>=dired-omit-mode (&optional buffer)
    "Setup `dired-omit-mode' in BUFFER using `>>=|dired/omit-mode' value."
    (with-current-buffer (or buffer (current-buffer))
      (setq >>-dired/sorted-directories nil)
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
              (>>=dired-omit-mode buf))))))))


(use-package wdired
  :custom
  (wdired-allow-to-change-permissions t))



;;; Reuse the current dired buffer to visit a directory

(use-package dired-single
  :ensure t
  :demand t
  :functions >>=dired-search-forward >>-dired-omit-ignores-switches
  :bind
  (:map dired-mode-map
    ("<return>" . dired-single-buffer)
    ([M-S-down] . dired-single-buffer)
    ([M-down] . dired-single-buffer)
    ([M-S-up] . dired-single-up-directory)
    ([M-up] . dired-single-up-directory)
    ("^" . dired-single-up-directory)
    ("M-P" . dired-single-up-directory)
    ("/" . >>=dired-single-reload)
    ("r" . >>=dired-insert-recursive-subdir)
    ("i" . >>=dired-maybe-insert-subdir)
    ([mouse-1] . dired-single-buffer-mouse)
    ([mouse-2] . dired-single-buffer-mouse))
  :config
  (defun >>=dired-single-reload (&optional arg)
    "Reload current-directory in the Dired buffer.
If ARG is non-nil, it will reload the inserted sub-directory where the point
is located."
    (interactive "P")
    (unless (eq major-mode 'dired-mode)
      (error ">>= this function can only be called in `dired-mode'"))
    (let ((pos (point))
          (fname (dired-get-filename 'no-dir 'no-error)))
      (when (null arg)
        (goto-char (point-min)))
      (dired-single-buffer (dired-current-directory))
      (when (and (not (>>=dired-search-forward fname)) (< pos (point-max)))
        (goto-char pos))))

  (defun >>=dired-insert-recursive-subdir (dirname)
    "Insert directory DIRNAME into the same buffer using recursive options.
Very similar to `dired-insert-subdir'."
    (interactive (list (dired-get-filename)))
    (dired-insert-subdir dirname
      (concat
        (or dired-subdir-switches dired-actual-switches)
        " -R "
        (when >>=|dired/omit-mode (>>-dired-omit-ignores-switches))))
    (>>=dired-omit-mode))

  (defun >>=dired-maybe-insert-subdir (dirname &optional switches)
    "Insert sub-directory DIRNAME into the same Dired buffer.
If SWITCHES contains recursive flag (see `dired-switches-recursive-p') and
global variable `>>=|dired/omit-mode' is t, then omit extra patterns will be
calculated and concatenated.  See `dired-maybe-insert-subdir'."
    (interactive
      (list
        (dired-get-filename)
        (if current-prefix-arg
          (read-string "Switches for listing: "
            (or dired-subdir-switches dired-actual-switches)))))
    (let ((opoint (point))
          (dirname (file-name-as-directory dirname)))
      (when (and >>=|dired/omit-mode (dired-switches-recursive-p switches))
        (setq switches
          (concat switches " " (>>-dired-omit-ignores-switches))))
      (or
        (and
          (not switches)
          (when (dired-goto-subdir dirname)
            (unless (dired-subdir-hidden-p dirname)
                (dired-initial-position dirname))
              t))
        (dired-insert-subdir dirname switches))
      (push-mark opoint))
    (>>=dired-omit-mode))

  (defun >>-dired-single-buffer (org-dired-single-buffer &rest args)
    "Advice function ORG-DIRED-SINGLE-BUFFER (using same ARGS).
Select source directory item position when navigating up."
    (interactive)
    (let ((org (dired-current-directory)))
      (setq >>-dired/sorted-directories nil)
      ;; super
      (apply org-dired-single-buffer args)
      ;;
      (let ((dst (dired-current-directory)))
        (if (string-prefix-p dst org)
          (let* ((targets (split-string (substring org (length dst)) "/"))
                 (aux (car targets))
                 (target (if (string= aux "") (cadr targets) aux)))
            (goto-char (point-min))
            (if (null (>>=dired-search-forward target))
              (dired-next-line 4)))))))

  (advice-add 'dired-single-buffer :around '>>-dired-single-buffer))



;;; Patch to fix `dired-aux' bug

(require 'dired-aux)

;; Next fucntion is a replacement of the original one because `string-match-p'
;; is used to check switches instead of `dired-check-switches' causing an
;; error if you try to use a value like '-laF -BR --ignore=*.bak' because the
;; 'b' in 'bak'.
;;
;; TODO: try to advice original function.

(defun dired-insert-subdir-validate (dirname &optional switches)
  "Check if DIRNAME is valid to insert.
A SWITCHES option can be specified.  Signal an error if invalid (e.g. user
typed `i' on `..')."
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


;;; TODO: Check this, `dired-replace-in-string' is obsolete
;; (defadvice dired-replace-in-string
;;   (around >>-dired-replace-in-string (regexp newtext string) activate)
;;   "Fix one character switch replace."
;;   (let ((org regexp) (res string))
;;     (when (and (eq (string-width regexp) 1) (string-equal newtext ""))
;;       (setq regexp (format " ?\\_<-%s\\_>" regexp))
;;       (setq res ad-do-it))
;;     (if (string-equal res string)
;;       (progn
;;         (setq regexp org)
;;         ad-do-it)
;;       ;; else
;;       res)))


(provide 'xorns-dired)
;;; xorns-dired.el ends here
