;;; xorns.el --- Merchise extensions for Emacs

;; Copyright (C) 2014  Merchise Autrement
;; All rights reserved.

;; Author: Medardo Rodriguez <med@merchise.org>
;; Created: 2014-02-05
;; Keywords: merchise, extensions, tools
;; URL: http://dev.merchise.org/emacs/xorns

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

;; "Xorns" provides routines and variables to use Emacs in a better way,
;; according to "Merchise Group" best practices and rules.
;;
;; The name "Xorns" comes from "horns" as a metaphor because "Emacs" is the
;; head of the "Gnu" and "Xorns" the horns of that head.  The "X" is due to
;; Merchise's projects start with that letter looking for words in Nahuatl or
;; those in English sometimes replacing the "H" for the "X" like in this case.
;;
;; Features:
;;
;; - Improved ...
;;
;; Getting Started:
;;
;; To start using this package, place it somewhere in your Emacs load-path and
;; add the line to require it.
;;
;;     (add-to-list 'load-path "/path/to/xorns")
;;     (require 'xorns)
;;     ...
;; TODO: Check autoload file generation like "elpa" does.
;;       See "package-refresh-contents" to ELPA setup
;;
;; Notes:
;;
;; "xorns" is designed to work ...
;;
;;
;;
;; Please email bug reports and suggestions to the authors, or submit
;; them at https://public.merchise.org/xorns/issues
;;
;; TODO: See "clojure-snippets.el" for add merchise yasnippets
;; "ein-20130310.811/ein-pkg.el" for package dependencies
;; "ein-loaddefs.el" for autoloads
;;
;; Enjoy!


;;; Code:

;; TODO: (unless (featurep 'git-commit-mode) (require 'magit-log-edit nil t))
;;(eval-when-compile
;;  (require 'xorns-extra nil 'noerror))


(defgroup xorns nil
  "Merchise extensions for Emacs."
  :prefix "xorns-"
  :group 'extensions
  :group 'convenience)


(defconst xorns-home-dir (purecopy (file-truename "~"))
   "Home directory full name.")


;; The LOCAL arg to `add-hook' is interpreted differently in Emacs and
;; XEmacs.  In Emacs we don't need to call `make-local-hook' first.
;; It's harmless, though, so the main purpose of this alias is to shut
;; up the byte compiler.
;; This is for "apt-utils" to work.
(defalias 'make-local-hook 'ignore)


;; TODO: This code must be removed when Emacs >= 24.3 is used for every one.
(unless (functionp 'file-name-base)
  (defun file-name-base (&optional filename)
     "Return the base name of the FILENAME: no directory, no extension.
FILENAME defaults to `buffer-file-name'."
     (file-name-sans-extension
	(file-name-nondirectory (or filename (buffer-file-name))))))



;;; Some simple string utilities


;;;###autoload
(defun xorns-str-trim (s)
  "Remove white-spaces at start and end of the string S."
  (let ((blanks split-string-default-separators))
    (replace-regexp-in-string
      (format "\\`%s\\|%s\\'" blanks blanks) "" s)))


(provide 'xorns)
;;; xorns.el ends here
