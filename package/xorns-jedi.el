;;; xorns-jedi --- Structures that are pending of review.

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Rodriguez <med@merchise.org>
;; URL: http://dev.merchise.org/emacs/xorns-jedi
;; Keywords: initialization, merchise, convenience
;; Version: 20181018.1010

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

;; Enjoy!


;;; Code:


;;; porg

(require 'python nil 'noerror)
(require 'jedi nil 'noerror)
(require 'tramp nil 'noerror)

(require 'xorns-project nil 'noerror)



(when (xorns-configure-p 'basic)
  (add-hook 'python-mode-hook
    (lambda ()
      (condition-case err
        (unless (tramp-connectable-p (buffer-file-name))
          (xorns-jedi-setup))
        (error (message "error@python-mode-hook: %s" err))))))


(defun xorns-jedi-setup ()
  "Setup `jedi' for current buffer."
  (if (featurep 'jedi)
    (progn
      (jedi:setup)
      (define-key python-mode-map "\C-ch" 'jedi:show-doc)
      (define-key python-mode-map "\M-." 'jedi:goto-definition)
      (define-key python-mode-map "\M-*" 'jedi:goto-definition-pop-marker))
    ;; else
    (xorns-missing-feature 'jedi)))



;;; project

(defcustom xorns-use-workspace-for-jedi nil
  "Have jedi include your `xorns-preferred-default-directory'.

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


(defun xorns-project-jedi-setup (&optional project-file-name sentinel buffer)
  "Setup the `jedi:server-args' for the project's virtualenv.

The PROJECT-FILE-NAME, SENTINEL and BUFFER parameters have the same meaning
that in `xorns-find-project-virtualenv-dir'."
  (if (featurep 'jedi)
    (let* ((virtualenv-dir (xorns-find-project-virtualenv-dir
                             project-file-name sentinel buffer))
            (buildout-txt (xorns-find-project-def-file
                            "buildout.project.el" sentinel buffer))
            (buildout-deveggs
              (when buildout-txt
                (with-temp-buffer
                  (progn
                    (insert-file-contents buildout-txt)
                    (unless (zerop (buffer-size))
                      (read (current-buffer)))
                    ))))
            (preferred-dirs
              (cond
                ((eq xorns-use-workspace-for-jedi t)
                  (xorns-preferred-default-directory))
                ((eq xorns-use-workspace-for-jedi 'subdirs)
                  (cl-remove-if-not
                    (lambda (d) (if (file-directory-p d) d))
                    (cl-subseq
                      (directory-files
                        (xorns-preferred-default-directory)
                        'fullname)
                      2)))))
            (jedi-server-args
              (append
                (when buildout-deveggs
                  (-mapcat
                    (lambda (dir) (list "-p" dir))
                    buildout-deveggs))
                (when preferred-dirs
                  (-mapcat
                    (lambda (dir) (list "-p" dir))
                    preferred-dirs))
                (when virtualenv-dir
                  (list "-v" virtualenv-dir))
                )))
      (when jedi-server-args
        (message "Jedi arguments are: '%s' for buffer '%s'"
          jedi-server-args (or buffer (current-buffer)))
        (set (make-local-variable 'jedi:server-args) jedi-server-args)))
    ;; else
    (xorns-missing-feature 'jedi)))


(when (xorns-configure-p 'general)
  (add-hook
    'python-mode-hook        ; run when editing python source code
    (lambda ()
      (condition-case err
        (progn
          (unless (tramp-connectable-p (buffer-file-name))
            (xorns-project-jedi-setup)
            (xorns-python-shell-setup-completion)))
        (error (message "error@python-mode-hook: %s" err))))))

(provide 'xorns-jedi)
;;; xorns-jedi.el ends here
