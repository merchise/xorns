;;; xorns-project --- Manage and navigate projects in Emacs easily

;; Copyright (C) 2014 Merchise Autrement

;; Author: Medardo Rodriguez <med@merchise.org>
;; URL: http://dev.merchise.org/emacs/xorns-project
;; Keywords: initialization, merchise, convenience
;; Version: 20140322.2213

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

;; Manage and navigate projects in Emacs easily

;; This module is automatically used when::
;;
;;     (require 'xorns)

;; Enjoy!


;;; Code:

(require 'dash nil 'noerror)
(require 'projectile nil 'noerror)

(require 'xorns-utils)


(when (and (featurep 'projectile) (xorns-configure-p 'general))
  (projectile-global-mode t)
  (add-to-list 'projectile-project-root-files "setup.py"))


(defcustom xorns-virtualenvs-dir
  (file-name-as-directory
    (or
      (getenv "WORKON_HOME")
      (xorns-path-join xorns-home-dir ".virtualenvs")))
   "The directory where all the virtualenvs reside."
   :group 'xorns
   :type 'directory)


(defcustom xorns-use-workspace-for-jedi 'subdirs
  "Have jedi include your `xorns-prefered-default-directory'.

Possible values are: nil, t or the symbol `subdirs'.  If t your preferred
default directory will be included alone.  If `subdirs` each of the
sub-directories in your preferred default directory will be included.  If nil,
then the preferred directory will not be included (unless other customizations
do it)."
  :group 'xorns
  :type '(choice
	   (const :tag "Don't include any thing" nil)
	   (const :tag "Include subdirs" subdirs)
	   (const :tag "Include top-directory" t)))



;;; Nice buffer names

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
  (let* ((project-file-name (or project-file-name ".project.el"))
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
	project-file)))



;; Python Specific

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
		    (xorns-find-project-virtualenv-name project-file-name
							sentinel
							buffer)))))
      (if (file-directory-p virtualenv-dir) virtualenv-dir))))


(defun xorns-omelette-dirs  (&optional sentinel buffer)
  "Find the parts/omelette in a buildout setup.

The SENTINEL and BUFFER parameters have the same meaning that in
xorns-find-project-virtualenv-dir."
  (-when-let (project-def-file
	      (xorns-find-project-def-file
	       "bin/buildout" sentinel buffer))
    (let* ((project-dir
	    (xorns-path-join (file-name-directory project-def-file) ".."))
	   (omelette-dir
	    (xorns-path-join project-dir "parts" "omelette"))
	   (omelette-dir-exists
	    (file-directory-p omelette-dir)))
      (when omelette-dir-exists
	omelette-dir))))


(defun xorns-exec-path-setup (&optional sentinel buffer)
  "Setup the `exec-path' to search in the buildout directory.

The SENTINEL and BUFFER parameters have the same meaning that in
xorns-find-project-virtualenv-dir."
  (let* ((virtualenv-dir
	  (xorns-find-project-virtualenv-dir nil sentinel buffer))
	 (buildout-executable
	  (xorns-find-project-def-file "bin/buildout" sentinel buffer))
	 (buildout-exec-path
	  (when buildout-executable (file-name-directory
				     buildout-executable))))
    (let ((local-exec-path (make-local-variable 'exec-path)))
      (when virtualenv-dir
	(add-to-list local-exec-path virtualenv-dir))
      (when buildout-exec-path
	(add-to-list local-exec-path buildout-exec-path)))))


(defun xorns-project-jedi-setup (&optional project-file-name sentinel buffer)
  "Setup the `jedi:server-args' for the project's virtualenv.

The PROJECT-FILE-NAME, SENTINEL and BUFFER parameters have the same meaning
that in `xorns-find-project-virtualenv-dir'."
  (if (featurep 'jedi)
    (let* ((virtualenv-dir
	     (xorns-find-project-virtualenv-dir project-file-name sentinel buffer))
	    (omelette-path
	      (xorns-omelette-dirs sentinel buffer))
	    (preferred-dirs
	      (cond
		((eq xorns-use-workspace-for-jedi t)
		  (xorns-prefered-default-directory))
		((eq xorns-use-workspace-for-jedi 'subdirs)
		  (cl-remove-if-not
		    (lambda (d) (if (file-directory-p d) d))
		    (cl-subseq
		      (directory-files
			(xorns-prefered-default-directory)
			'fullname)
		      2)))))
	    (jedi-server-args
	      (append
		(when preferred-dirs
		  (-mapcat
		    (lambda (dir) (list "-p" dir))
		    preferred-dirs))
		(when virtualenv-dir
		  (list "-v" virtualenv-dir))
		(when omelette-path
		  (list "-p" omelette-path)))))
      (when jedi-server-args
	(message "Jedi arguments are: '%s' for buffer '%s'"
	  jedi-server-args (or buffer (current-buffer)))
	(set (make-local-variable 'jedi:server-args) jedi-server-args)))
    ;; else
    (xorns-missing-feature 'jedi)))


(defun xorns-find-better-unique-buffer-name ()
  "Hook for `find-file-hook' to find a better buffer name."
  (let ((unique nil)
	(passes 0)
	(max-passes 10)
	(name (buffer-name))
	;; Defensively tests for projectile's functions since we could be
	;; called without it been installed.
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
			  (concat project-name ":" current-path-component)
			  current-path-component))
	     (setq unique (null (get-buffer name)))
	     (setq passes (1+ passes))
	     (setq stop (or unique (> passes max-passes)))
	     (not stop))
	   path-components)))
      (when unique
	(message "Found name '%s'" name)
	(rename-buffer name)))))


(defun xorns-python-shell-setup-completion ()
  "Setup the `python-shell-completion-setup-code' variable.

This adds the current's project directory to the \"sys.path\" when starting
the python shell."
  (if (and (featurep 'projectile)
	(functionp 'projectile-project-root))
    (let ((project-dir (projectile-project-root)))
      ;; Using python-shell-extra-pythonpaths is not working so
      ;; let's messup with python-shell-completion-setup-code.
      (make-local-variable 'python-shell-completion-setup-code)
      (setq
	python-shell-completion-setup-code
	(concat
	  python-shell-completion-setup-code
	  "\n"
	  (format "sys.path.append('''%s''')" project-dir)
	  "\n")))))



;;  Standard hooks for project integration

(when (xorns-configure-p 'minimum)
  (add-hook 'find-file-hook          ; after a buffer is loaded from a file
    'xorns-find-better-unique-buffer-name))



;; Project configuration stuff is basic-level by definition, but since these
;; are project configuration for programming in python the belong to the
;; general level.

(when (xorns-configure-p 'general)
  (add-hook
    'python-mode-hook        ; run when editing python source code
    (lambda ()
      (condition-case err
	(progn
	  (xorns-exec-path-setup)
	  (xorns-project-jedi-setup)
	  (xorns-python-shell-setup-completion))
	(error (message "error@python-mode-hook: %s" err))))))


(provide 'xorns-project)
;;; xorns-project.el ends here
