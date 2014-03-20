;;; xorns-prog --- Programming language source code editing

;; Copyright (C) 2014 Merchise Autrement

;; Author: Medardo Rodriguez <med@merchise.org>
;; URL: http://dev.merchise.org/emacs/xorns-prog
;; Keywords: initialization, merchise, convenience
;; Version: 20140319.2129

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

;; Generic definitions for editing programming language source code.

;; Enjoy!


;;; Code:


(require 'dash nil 'noerror)   ; facilities like -when-let and -mapcat
(require 'jedi nil 'noerror)
(require 'xorns nil 'noerror)



;;; Project Related functions

(defun -directory-exe-list (dir &optional match)
   "Return a list of names of executable files (not folders) in DIR.

If MATCH is non-nil, mention only file names that match the regexp MATCH.

All file-names are returned absolute."
  (delq nil
    (mapcar
      #'(lambda (item)
	  (if (and (file-executable-p item) (not (file-directory-p item)))
	    item))
      (directory-files dir 'full match 'nosort))))


(defvar xorns-programming-project-alist
  (mapcar 'purecopy
    `(
       (generic-mode
	 :selector
	 (
	   "^\\(.*project\\|makefile\\)\\([.]\\w+\\)?$"
	   #'(lambda (dir)
	       (-directory-exe-list
		 dir
		 "^\\(install\\|setup\\)\\([.]\\w+\\)?$")))
	 :sources "^\\(source\\|src\\|lib\\|plugins\\|addons\\)$")
       (python-mode
	 :init-file "__init__.py"
	 :selector "^setup.py$")))
  "An association list for programming project definitions.

Each definition has a name related with a mode and a set of keywords:

- `:init-file` Packages for the defining mode has an initialization file with
  the name in the value.

- `:selector` Could be a string, a function or a list containing strings or
  functions; strings are regular expressions, a folder with matching files
  represent a project; a function simply return any true value if a project,
  else return nil; a list act as a `or` of all containing values.

- `:inherits` Define a definition to inherits from.

If a mode don't has project definition, `generic mode is used`.

Use `add-to-list` or similar functions to modify this list.

Definitions in each mode of this variable complements the conventions used in
several project management functions.

Currently evaluated folder is considered a package base if the parent folder:

- doesn't contains the file with source code for package initialization that
  certain languages define.  No all programming languages has this kind of
  file, but for example Python define it named '__init__.py'.

- has a special name: 'source', 'src', 'lib', 'plugins' or 'addons'.

- contains files that marks a project: '.*project\([.][a-z]+\)?', executable
  file '\(setup\|install\)\([.][a-z]+\)?', 'makefile\([.][a-z]+\)?'.")


(defun xorns-get-mode-tag (tag &optional mode)
   "Return a named TAG for a given a MODE.

If no mode is given, `generic-mode` is assumed.

Modes are iterated using the inheritance path until the TAG is found."
   (setq mode
      (or
	 ;; manu: If the provided mode is not in the alist use the major-mode
	 ;; or the generic-mode
	 (car (assoc (or mode major-mode) xorns-programming-project-alist))
	 'generic-mode))
   (let (res done)
      (while (not done)
	 (-when-let (mode-tags (assoc mode xorns-programming-project-alist))
	    (-when-let (aux (cadr (memq tag mode-tags)))
	       (setq res aux done t)
	       (progn
		  (setq mode (cadr (memq :inherits mode-tags)))
		  (if (not mode)
		     (setq done t))))
	    (setq done t)))
      res))


(defun xorns-find-project-def-file
  (&optional project-file-name sentinel buffer)
  "Find the PROJECT-FILE-NAME file name.

If SENTINEL is provided, it should be a _directory_, and the search stops when
that directory is reached.

If BUFFER is not provided, the current buffer is used.  the buffer's file name
is used to start the search.

if PROJECT-FILE-NAME is not provided it defaults to \".project.el\"."
  (let* (
	  (project-file-name (or project-file-name ".project.el"))
	  (current (file-name-directory (buffer-file-name buffer)))
	  (last "")
	  (sentinel (if sentinel (file-name-as-directory sentinel) nil))
	  (stop (string= current sentinel))
	  (project-file (concat (file-name-as-directory current)
			  project-file-name)))
    (while (and
	     (not stop)
	     (not (file-exists-p project-file))
	     (not (string= last current)))
      (progn
	(setq last current
	  current (file-name-directory (directory-file-name current))
	  stop (string= current sentinel)
	  project-file (concat (file-name-as-directory current)
			 project-file-name))))
    (if (file-exists-p project-file)
      project-file)
    ))


(defun xorns-project-dir (&optional mode)
   "Return the project directory.
Optionally a programming MODE can be given.  If no MODE is given,
`generic-mode` definition is assumed."
   (let* ((res nil)
	  (top xorns-home-dir)
	  (selector (xorns-get-mode-tag :selector mode))
	  (selectors (if (stringp selector) (list selector) selector))
	  (current (buffer-file-name))
	  (preds (mapcar
		   #'(lambda (selector)
		       (or
			 (when (stringp selector)
			   (lambda (directory)
			     (directory-files directory t selector t)))
			 ;; otherwise return the selector it should be a function
			 selector))
		   selectors)))
      (while (and (not res) preds)
	 (let ((pred (car preds)))
	    (setq preds (cdr preds))))
      res))


;;; Python

(defcustom xorns-virtualenvs-dir
  (file-name-as-directory
    (or
      (getenv "WORKON_HOME")
      (concat (file-name-as-directory xorns-home-dir) ".virtualenvs")))
   "The directory where all the virtualenvs reside."
   :group 'xorns
   :type 'directory)


(defun xorns-find-project-virtualenv-name (&optional project-file-name sentinel
					    buffer)
   "Find the project's virtualenv name.

The PROJECT-FILE-NAME is the name of the file where the project definitions
are.  If it is not provided or is nil, it defaults to \".project.el\".

The project definitions files _must_ contain an \"Association List:\" like:

   ((project-virtualenv-name . \"VIRTUALENVNAME\").

This function will find the symbol 'project-virtualenv-name inside the
association list and return the CDR.

The SENTINEL is interpreted as xorns-find-project-def-file.

The BUFFER contains the buffer for which the project is searched. If not
provided defaults to the current buffer.

By convention, if the 'project-virtualenv-name is not present or is nil, the
virtualenv name will be the name of the directory where the project definition
file resides, or when any of the project file markers reside."
   (or
      (-when-let (project-file-name (xorns-find-project-def-file
				       project-file-name sentinel buffer))
	 (let* ((project-locals-class (dir-locals-read-from-file
					 project-file-name))
		  (project-locals-alist (dir-locals-get-class-variables
					   project-locals-class)))
	    (cdr (assoc 'project-virtualenv-name project-locals-alist))))
      ;; If the project-virtualenv-name
      (let ((res nil))
	 ;; TODO: Finish xorns-project-dir and use that
	 (-when-let (project-file-name (xorns-find-project-def-file
					  "setup.py" sentinel buffer))
	    (file-name-base (directory-file-name
			       (file-name-directory project-file-name)))))))


(defun xorns-find-project-virtualenv-dir (&optional project-file-name sentinel
					   buffer)
   "Find the project's virtualenv directory.

This function will find the project definitions file and find the configured
virtual environment name.

The PROJECT-FILE-NAME and SENTINEL are intrepreted as in
xorns-find-project-def-file.

The SENTINEL default to the user's home directory.  *Note*: Windows users
should be aware that sometimes the user's home directory does not contains it's
projects.

The BUFFER, if given, should be the buffer for which the project virtualenv
directory is to be found.  If not given, it defaults to the current buffer.

If either there's no project definition file, or the file does not contains a
project-virtualenv-name definition, or the virtualenv directory does not
exists, the funtion returns nil."
  (-when-let (virtualenv-name (xorns-find-project-virtualenv-name
				project-file-name sentinel buffer))
    (let ((virtualenv-dir
	    (file-truename
	      (concat (file-name-as-directory xorns-virtualenvs-dir)
		(xorns-find-project-virtualenv-name
		  project-file-name sentinel buffer)))))
      (if (file-directory-p virtualenv-dir) virtualenv-dir))))


(defun xorns-find-buildout-paths (&optional sentinel buffer)
   "Find the buildout paths for the project.

This *may* not work properly with multiversion eggs, cause we simply search for
a 'bin/buildout' file and then use the eggs (all of them).

The SENTINEL and BUFFER parameters have the same meaning that in
xorns-find-project-virtualenv-dir."
   (-when-let (eggs-dir (xorns-find-project-def-file
			   "buildout-cache/eggs/" sentinel buffer))
      (directory-files eggs-dir 'full-name
	 "[^\.]$" 'nosort)))


(defun xorns-buildout-mrdev-paths (&optional sentinel buffer)
   "Find the src/* path for a buildout project.

The project is detected by having a 'bin/buildout' file.  At that level the
'src' directory is searched and all of it's directories are returned as
mr.developer paths.

This *may* not work properly with multiversion eggs, cause we simply search for
a 'bin/buildout' file and then use the eggs (all of them).

The SENTINEL and BUFFER parameters have the same meaning that in
xorns-find-project-virtualenv-dir."
  (-when-let (project-dir (xorns-find-project-def-file
			    "bin/buildout" sentinel buffer))
    (let* ((src-dir (concat (file-name-directory    ; TODO: See `f-join'
			      (directory-file-name
				(file-name-directory project-dir))) "src"))
	    (src-dir-exists (file-directory-p src-dir)))
      (when src-dir-exists
	(directory-files src-dir 'full "[^\.]$" 'nosort)))))


(defun xorns-omelette-dirs  (&optional sentinel buffer)
   "Find the parts/omelette in a buildout setup.

The SENTINEL and BUFFER parameters have the same meaning that in
xorns-find-project-virtualenv-dir."
   (-when-let (project-dir (xorns-find-project-def-file
			      "bin/buildout" sentinel buffer))
      (let* ((parts-dir (file-name-as-directory
			   (concat (file-name-directory
				      (directory-file-name
					 (file-name-directory project-dir)))
			      "parts")))
	       (omelette-dir (concat parts-dir "omelette"))
	       (omelette-dir-exists (file-directory-p omelette-dir)))
	 (when omelette-dir-exists
	    omelette-dir))))


(defun xorns-exec-path-setup (&optional sentinel buffer)
  "Setup the `exec-path' to search in the buildout directory.

The SENTINEL and BUFFER parameters have the same meaning that in
xorns-find-project-virtualenv-dir."
  (let* ((virtualenv-dir (xorns-find-project-virtualenv-dir nil sentinel
			   buffer))
	  (buildout-executable (xorns-find-project-def-file
				   "bin/buildout" sentinel buffer))
	  (buildout-exec-path (if buildout-executable
				(file-name-directory buildout-executable)
				nil)))
    (progn    ; TODO: This `progn' is not necessary.
      (when virtualenv-dir
	(add-to-list 'exec-path virtualenv-dir))
      (when buildout-exec-path
	(add-to-list 'exec-path buildout-exec-path)))))


(defun xorns-project-jedi-setup (&optional project-file-name sentinel buffer)
  "Setup the jedi for the project's virtualenv.

The PROJECT-FILE-NAME, SENTINEL and BUFFER parameters have the same meaning
that in xorns-find-project-virtualenv-dir."
  (let* ((virtualenv-dir
	   (xorns-find-project-virtualenv-dir project-file-name sentinel buffer))
	 (buildout-eggs-paths
	   (xorns-find-buildout-paths sentinel buffer))
	 (mr.developer-paths
	   (xorns-buildout-mrdev-paths sentinel buffer))
	 (omelette-path
	   (xorns-omelette-dirs sentinel buffer))
	 (jedi-server-args
	   (append
	     (when virtualenv-dir (list "-v" virtualenv-dir))
	     (-mapcat
	       (lambda (item) (list "-p" item))
	       buildout-eggs-paths)
	     (when omelette-path (list "-p" omelette-path))
	     (when (and mr.developer-paths (null omelette-path))
	       (-mapcat
		 (lambda (item)
		   (list "-p" item))
		 mr.developer-paths)))))
    (when jedi-server-args
      (message "Jedi arguments are: '%s' for buffer '%s'"
	jedi-server-args (or buffer (current-buffer)))
      (set (make-local-variable 'jedi:server-args) jedi-server-args))))


(provide 'xorns-prog)
;;; xorns-prog.el ends here
