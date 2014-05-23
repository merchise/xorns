;;; before-init.el --- Example Merchise Initialization File for local user

;; Copyright (C) 2014 Merchise Autrement

;; This program is free software (GPL v3 or any later).
;; Type `C-h C-c' in Emacs to see full licence.

;;; Commentary:

;; All files in this directory are examples to be sym-linked or copied as base
;; in "~/.emacs.d/".  This one is a initialization file that runs before
;; `xorns'.

;; The name in "~/.emacs.d/" could be created like:
;;   $ ln -sf ~/work/src/xorns/home/before-init.el before-init-med.el

;; Enjoy!

;;; Code:

;; Remove "--group-directories-first" in Mac
;; '(dired-listing-switches "-l --group-directories-first -h")


(setq
  user-mail-address (substitute-env-vars "${USER}@merchise.org")
  split-width-threshold 120)

;; TODO: Check this very well all these
(condition-case err
  (progn
    (require 'electric nil 'noerror)
    (electric-pair-mode t))
  (error (message "error@prog-mode-hook: %s" err)))


;; TODO: Check all these
(set-variable 'clean-buffer-list-delay-general 1)
(set-variable 'clean-buffer-list-delay-special 900)
(set-variable 'dired-isearch-filenames t)
(set-variable 'dired-isearch-filenames-regexp t)
(set-variable 'doc-view-continuous t)
(set-variable 'inferior-lisp-program "clisp")
(set-variable 'list-command-history-max 128)
(set-variable 'term-input-autoexpand t)
(set-variable 'wdired-allow-to-change-permissions t)


; (setq muse-project-alist '(("planner" ("~/.pim/planner/"))))


(defun -set-faces (family height)
  "Initialize faces for theme \"'user\".

The arguments should be a string with the FAMILY name and an integer with the
HEIGHT.

Argument FAMILY could be for example \"Monospace\" for Ubuntu or
\"Lucida Sans Typewriter\". HEIGHT 94 or 115.

Maybe, this function must be migrated to be integral part of `xorns'."
  (condition-case err
    (let* ( (theme 'user)
	    (face 'default)
	    (spec
	      `((t
		  ( :family ,family
		    :foundry "unknown"
		    :slant normal
		    :weight normal
		    :height ,height
		    :width normal))))
	    (comment "Defined internally in `xorns'.")
	    (oldspec nil)
	    (theme-settings
	      (get theme 'theme-settings)))
      (put face 'saved-face spec)
      (put face 'saved-face-comment comment)
      (put face 'theme-face (cons (list theme spec) oldspec))
      (put theme 'theme-settings
	(cons (list 'theme-face face theme spec) theme-settings))
      (put face 'face-comment comment)
      (put face 'face-override-spec nil)
      (face-spec-set face spec t))    ; <-- let*
    (error (message "error@-set-faces: %s" err))))


(require 'ns nil 'noerror)
(if (not (featurep 'ns))
  (-set-faces "Monospace" 94)
  ;; Mac
  (-set-faces "Lucida Sans Typewriter" 115))


;;; before-init.el ends here
