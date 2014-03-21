;;; xorns-buffers.el --- Merchise extensions for Emacs (buffers)

;; Copyright (C) 2014  Merchise Autrement
;; All rights reserved.

;; Author: Medardo Rodriguez <med@merchise.org>
;; Created: 2014-03-10
;; Keywords: merchise, extensions, tools
;; URL: http://dev.merchise.org/xorns/xorns-buffers

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; "xorns-buffers" provides routines and variables to manipulate Emacs
;; buffers.
;;
;; Features:
;;
;; - Resurrects a «*scratch*» buffer is none is present.
;; - Better renaming of buffers; i.e differentiate buffers via project's name
;;   and containing directory hierarchy.
;;
;; Enjoy!


;;; Code:


(require 'dash nil 'noerror)
(require 'projectile nil 'noerror)


(when (featurep 'projectile)
  (projectile-global-mode t)
  (add-to-list 'projectile-project-root-files "setup.py"))


;;;###autoload
(defun xorns-force-switch-to-scratch ()
  "Switch to `*scratch*` buffer, creating a new one if needed."
  (interactive)
  (let ((buf (get-buffer-create "*scratch*")))
    (set-buffer-major-mode buf)
    (switch-to-buffer buf)
    (delete-other-windows)))



;;; Unique nice buffer names

;; TODO: Maybe this must be moved to `xorns.el'
(defmacro --collecting-reduce (form l)
   "Perform a collecting reducing using FORM over the list L.

A collecting reduce is a reduce that does not returns a single value (the
result of the last computation) but that returns a list of all computations
made.

The FORM should use 'FIRST' and 'SECOND' as the current items to be processed.
The first time FORM is executed FIRST takes nil."
   `(-flatten
       (-reduce-from
	  (lambda (previous second)
	     (let ((first (car (last previous))))
		(list previous ,form)))
	  ()
	  ,l)))


(defun xorns-collecting-reduce(func l)
   "Functional form for `--collecting-reduce'.

FUNC must be a function object and L must a sequence."
   (--collecting-reduce (funcall func first second) l))


(defun xorns-find-file-name-components (filename &optional abbreviate)
  "Return a list of FILENAME path components.

If ABBREVIATE is not nil, abbreviates the FILENAME before splitting."
  (when abbreviate
    (setq filename (abbreviate-file-name filename)))
  ;; TODO: Handle "/" in filenames properly
  (let ((result (split-string filename "/")))
    result))


(defsubst -buffer-name-candidates (&optional filename)
  "Local function to get the better buffer names candidates for a FILENAME.

If FILENAME is nil the `buffer-file-name' is used.

The candidates are simply the right-to-left path components concatenated.  For
instance if FILENAME is \"~/.emacs.d/init.el\", then this function returns:

  (\"emacs.d/init.el\", \"~/emacs.d/init.el\")

Notice that the `file-name-nondirectory' candidate \"init.el\" is omitted,
since it's deemed already tried and not unique."
  (cdr
    (--collecting-reduce
      (if first
	(concat second "/" first)
	; else
	second)
      (nreverse
	(xorns-find-file-name-components
	  (or filename buffer-file-name) 'abbrev)))))


;;;###autoload
(defun xorns-find-better-unique-buffer-name ()
  "Hook for `find-file-hook' to find a better buffer name."
  (let ((unique nil)
	(passes 0)
	(max-passes 10)
	(name (buffer-name))
	(project-p (when (functionp 'projectile-project-p)
		     (projectile-project-p)))
	(project-name (when (functionp 'projectile-project-name)
			(projectile-project-name))))
      ;; TODO: This __init__.py hack must be properly checked only for python
      ;; projects, see xorns.el (Which module?).  But since I'm in a hurry,
      ;; and having __init__.py buffer names bugs me, this helps a lot.
    (unless (or project-p (string-match "<[0-9]+>$" name)
	      (eq "__init__.py" name))
      (message "No better name for buffer. It will be called '%s'" name))
    (when (or project-p (string-match "<[0-9]+>$" name)
	    (eq "__init__.py" name))
      (message "Should find a better name for '%s'" name)
      ;;; First just try to add the project-name
      (when project-name
	(setq name (concat project-name ":"
		     (file-name-nondirectory buffer-file-name)))
	;; TODO: See the __init__.py above.  Probably uniqueness is not the
	;; only thing to check, but also goodness.
	(setq unique (and (null (get-buffer name))
		       (not (string-match "__init__.py$" name)))))
      ;;; Then if not unique try prepending path components to buffer name
      (when (not unique)
	(let ((path-components (-buffer-name-candidates)))
	  (--take-while
	    (let ((stop nil)
		   (current-path-component it))
	      (setq name (if project-name
			   (concat project-name
			     ":" current-path-component)
			   current-path-component))
	      (setq unique (null (get-buffer name)))
	      (setq passes (1+ passes))
	      (setq stop (or unique (> passes max-passes)))
	      (not stop))
	    path-components)))
      (when unique
	(message "Found name '%s'" name)
	(rename-buffer name)))))


(provide 'xorns-buffers)
;;; xorns-buffers.el ends here
