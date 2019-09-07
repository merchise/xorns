;;; xorns-buffers.el --- Buffers management

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>
;; or type `C-h C-c' in Emacs.

;;; Commentary:

;; This module main features are:
;;
;; - Inhibits the start-up screen and initial message for `*scratch*'
;;   buffer.
;;
;; - Configure `C-x C-b' to list buffers using `ibuffer' instead
;;   standard `list-buffers', or `xorns-select-buffer' if
;;   `xorns-use-select-buffer' is non nil and `xorns-select-buffer-enabled'
;;   evaluates to t.
;;
;; - Set `ibuffer' groups.
;;
;; - Functionality to force `*scratch*' buffer.
;;

;; This module is automatically used when::
;;
;;     (require 'xorns)

;; Enjoy!


;;; Code:

(eval-when-compile
  (require 'cl))

(require 'ibuffer nil 'noerror)
(require 'ibuf-ext nil 'noerror)
(require 'grizzl nil 'noerror)
(require 'xorns-utils)

;; Get rid of the startup screen and `*scratch*' buffer message
;; >>=
(xorns-set-values
  '(inhibit-startup-screen t)
  '(initial-scratch-message nil))



;;; IBuffer

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Set `ibuffer' to loads some preferred groups.
(xorns-set-values
  '(ibuffer-saved-filter-groups
     (quote (("xorns-ibuffer-groups"
	       ("System"
		 (or
		   (name . "*scratch*")
		   (name . "*Messages*")
		   (mode . Custom-mode)))
	       ("Shell" (mode . term-mode))
	       ("Dired"
		 (or
		   (mode . dired-omit-mode)
		   (mode . dired-mode)))
	       ("Web"
		 (or
		   (mode . w3m-mode)))
	       ("Org"
		 (or
		   (mode . org-mode)
		   (mode . org-agenda-mode)
		   (mode . diary-mode)
		   (mode . calendar-mode)
		   (mode . bbdb-mode)
		   (name . "*Deft*")
		   (name . "bbdb")))
	       ("Configuration" (mode . conf-unix-mode))
	       ("Python" (mode . python-mode))
	       ("Haskell/Agda/Coq"
		 (or
		   (mode . haskell-mode)
		   (mode . agda2-mode)
		   (mode . coq-mode)))
	       ("Lisp"
		 (or
		   (mode . emacs-lisp-mode)
		   (mode . lisp-interaction-mode)
		   (mode . lisp-mode)))
	       ("C" (or (mode . c-mode) (mode . cc-mode)))
	       ("Scala/Java"
		 (or
		   (mode . scala-mode)
		   (mode . java-mode)
		   (mode . scala-mode-inf)))
	       ("RST/Markdown"
		 (or
		   (mode . rst-mode)
		   (mode . markdown-mode)))
	       ("XML/HTML/CSS"
		 (or
		   (mode . nxml-mode)
		   (mode . html-mode)
		   (mode . css-mode)
		   (mode . less-mode)
		   (mode . sass-mode)))
	       ("Version Control"
		 (or
		   (mode . git-commit-mode)
		   (mode . git-commit-major-mode)
		   (mode . git-rebase-mode)
		   (mode . magit-mode)
		   (mode . magit-cherry-mode)
		   (mode . magit-diff-mode)
		   (mode . magit-log-mode)
		   (mode . magit-log-select-mode)
		   (mode . magit-merge-preview-mode)
		   (mode . magit-popup-mode)
		   (mode . magit-process-mode)
		   (mode . magit-refs-mode)
		   (mode . magit-reflog-mode)
		   (mode . magit-revision-mode)
		   (mode . magit-stash-mode)
		   (mode . magit-stashes-mode)
		   (mode . magit-status-mode)
		   (mode . diff-mode)))
	       ("Help/Info/Completions"
		 (or
		   (mode . help-mode)
		   (mode . Info-mode)
		   (mode . Man-mode)
		   (mode . woman-mode)
		   (mode . rfcview-mode)
		   (mode . completion-list-mode)))
	       )))
     )
  '(ibuffer-formats
     (quote ((mark modified read-only " "
	       (name 22 22 :left :elide)
	       " "
	       (size 9 -1 :right)
	       " "
	       (mode 16 16 :left :elide)
	       " "
	       filename-and-process)
	      (mark " "
		(name 16 -1)
		" "
		filename))))
  )


(add-hook 'ibuffer-mode-hook
  (lambda ()
    (condition-case err
      (ibuffer-switch-to-saved-filter-groups "xorns-ibuffer-groups")
      (error (message "error@ibuffer-mode-hook: %s" err)))))


(defun xorns-toggle-header-mode-line ()
  "Toggle visibility of header mode-line."
  (interactive)
  (if (not header-line-format)
      (setq header-line-format
	'(multiple-frames "%b"
	   (" " (:eval (abbreviate-file-name default-directory)))))
    ; else
    (setq header-line-format nil))
  (force-mode-line-update 'all))


(defun xorns-ibuffer-visit-buffer (&optional single)
  "Visit the buffer on this line.
If optional argument SINGLE is non-nil, then also ensure there is only one
window.  After that standard behaviour, this function kills the `ibuffer'."
  (interactive "P")
  (ibuffer-visit-buffer single)
  (kill-buffer "*Ibuffer*"))


(when (featurep 'ibuffer)
  (define-key ibuffer-mode-map (kbd "M-RET") 'xorns-ibuffer-visit-buffer))



;;; Buffer selection

(defconst xorns-grizzl-select-buffer-enabled
  (and
    (functionp 'grizzl-make-index)
    (functionp 'grizzl-search))
  "This is t if all the requirements for `xorns-select-buffer' are fulfill.")


(defcustom xorns-use-grizzl-select-buffer nil
  "If t then `C-x b` will call `xorns-grizzl-select-buffer'."
  :group 'xorns
  :type 'boolean)


(defun xorns-grizzl-select-buffer ()
  "Select a buffer via `grizzl-search'."
  (interactive)
  (let* (
          (visible-buffer-names
            (loop
              for buffer being the buffers
              for buffer-name = (buffer-name buffer)
              if (not (string-match "^ " buffer-name))
              collect buffer-name
              ))
          (buffers-index (grizzl-make-index visible-buffer-names))
          (buffer (grizzl-completing-read "Buffer: " buffers-index)))
    (if (not (eq buffer (buffer-name)))
      (switch-to-buffer buffer))))


;; TODO: Prepare a function (not a lambda) in order to see the help with
;; `describe-key'.
(when xorns-grizzl-select-buffer-enabled
  (lexical-let ((previous-binding (global-key-binding (kbd "C-x b"))))
    (message "The C-x b previous binding was %s" previous-binding)
    (global-set-key (kbd "C-x b")
      (lambda ()
        (interactive)
        (if xorns-use-grizzl-select-buffer
          (call-interactively #'xorns-grizzl-select-buffer)
          (call-interactively previous-binding)))))
  )


;;; Buffers

(defun xorns-force-scratch (&optional arg)
  "Switch to `*scratch*` buffer, creating a new one if needed.

An optional argument ARG could be given to delete other windows; if
`0' also reset `default-directory' to `xorns' default."
  (interactive "P")
  (let ((buf (get-buffer-create "*scratch*")))
    (set-buffer-major-mode buf)
    (switch-to-buffer-other-window buf)
    (if (= (prefix-numeric-value arg) 0)
      (xorns-set-default-directory))
    (if arg (delete-other-windows))))


(global-set-key (kbd "C-c s") 'xorns-force-scratch)
(global-set-key (kbd "C-c h") 'xorns-toggle-header-mode-line)



;;; Hooks

(add-hook 'after-init-hook
  (lambda ()
    (condition-case err
      ;; Set initial default directory for `*scratch*' buffer
      (xorns-set-default-directory)
      (error (message "error@after-init-hook: %s" err)))))


(provide 'xorns-buffers)
;;; xorns-buffers.el ends here
