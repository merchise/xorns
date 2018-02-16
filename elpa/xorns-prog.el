;;; xorns-prog --- Programming language source code editing

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Rodriguez <med@merchise.org>
;; URL: http://dev.merchise.org/emacs/xorns-prog
;; Keywords: initialization, merchise, convenience
;; Version: 20150516.1620

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

;; This module is automatically used when::
;;
;;     (require 'xorns)

;; Enjoy!


;;; Code:

(require 'outline)
(require 'comint nil 'noerror)
(require 'flycheck nil 'noerror)
(require 'yasnippet nil 'noerror)
(require 'python nil 'noerror)
(require 'jedi nil 'noerror)
(require 'cc-mode nil 'noerror)
(require 'scala-mode nil 'noerror)
(require 'javadoc-lookup nil 'noerror)

(require 'xorns-text nil 'noerror)
(require 'xorns-utils nil 'noerror)


;;; Some variables
(xorns-set-values
  '(emacs-lisp-docstring-fill-column 78)
  '(lisp-indent-offset 2)
  '(make-backup-files nil)    ;; Use Version Control instead ;)
  )


;;;###autoload
(defun xorns-python-shell-send-paste (start end)
  "Send the region delimited by START and END wrapped with a %paste magic."
  ;; TODO: Try migrate this function to use 'ansi-term'
  (interactive "r")
  (unless (region-active-p)
    ;; If the region is not active, use the current line
    (save-excursion
      (end-of-line)
      (setq end (point))
      (beginning-of-line)
      (search-forward-regexp "[^\\s \t\n]" end 'noerror)
      (backward-char)  ;; search-forward ends after that
      (setq start (point))))
  ;; `python-shell-send-string' does too much magic trying to detect the
  ;; beginning of the output; using `comint-send-string' seems to be more
  ;; reliable.
  (save-excursion
    (kill-new (buffer-substring start end))
    (let*
      ((buffer (xorns-ansi-term))
       (process (get-buffer-process buffer)))
      (comint-send-string process "%paste\n"))))


;;; Hooks

(when (xorns-configure-p 'basic)
  (if (featurep 'flycheck)
    (progn
      (add-hook 'after-init-hook
        (lambda ()
          (unless (tramp-connectable-p (buffer-file-name))
            (global-flycheck-mode))))
      (xorns-set-value 'flycheck-idle-change-delay 60)
      )
    ;; else
    (xorns-missing-feature 'flycheck)))


(when (xorns-configure-p 'basic)
  (if (featurep 'yasnippet)
    (add-hook 'after-init-hook
      (lambda ()
        (yas-global-mode 1)))
    ;; else
    (xorns-missing-feature 'yasnippet)))


(add-hook 'prog-mode-hook          ; run for all programming modes
  (lambda ()
    (condition-case err
      (progn
        (xorns-fci-mode-on)
        (unless (tramp-connectable-p (buffer-file-name))
          (xorns-auto-complete-mode)
          (flyspell-prog-mode))
        (turn-on-auto-fill)
        (ispell-change-dictionary "english")
        (subword-mode nil)
        (linum-mode 1))
      (error (message "error@prog-mode-hook: %s" err)))))


;; FCI interacts badly with the Agda interactive features, so let's turn it
;; off.
(add-hook 'agda2-mode-hook
  (lambda()
    (xorns-fci-mode-off)))


(add-hook 'conf-unix-mode-hook          ; For configuration files
  (lambda ()
    (condition-case err
      (progn
        (xorns-fci-mode-on)
        (xorns-auto-complete-mode)
        (turn-on-auto-fill)
        (ispell-change-dictionary "english")
        (subword-mode nil)
        (linum-mode 1))
      (error (message "error@conf-unix-mode-hook: %s" err)))))



;;; Python

(add-hook 'python-mode-hook
  (lambda ()
    (condition-case err
      (progn
	(define-key python-mode-map (kbd "C-c C-r")
	  'xorns-python-shell-send-paste)
        (define-key python-mode-map "\C-m" 'newline-and-indent)
        (outline-minor-mode))
      (error (message "error@python-mode-hook: %s" err)))))


(when (xorns-configure-p 'basic)
  (add-hook 'python-mode-hook
    (lambda ()
      (condition-case err
        (unless (tramp-connectable-p (buffer-file-name))
          (xorns-jedi-setup))
        (error (message "error@python-mode-hook: %s" err))))))


(when (xorns-configure-p 'general)
  (add-hook 'inferior-python-mode-hook
    ;; Avoid sending TABs to ipython process, otherwise the ipython will
    ;; respond with autocompletion.
    (lambda ()
      (condition-case err
        (progn
          (xorns-set-value 'indent-tabs-mode nil)
          (linum-mode 0))
        (error (message "error@inferior-python-mode-hook: %s" err)))))

  (xorns-set-values
    ;; Configure `ipython` as shell when use function `run-python` and other
    ;; related commands.
    ;; This configuration is based in the way we, in Merchise, configure
    ;; IPython.  See README documentation for more information.
    '(python-shell-interpreter "ipython")
    '(python-shell-interpreter-args "-i --simple-prompt")
    '(python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: ")
    '(python-shell-prompt-pdb-regexp "i?pdb> ")
    '(python-shell-prompt-regexp "In \\[[0-9]+\\]: ")
     '(python-shell-completion-setup-code
	"import sys; from IPython.core.completerlib import module_completion")
    '(python-shell-completion-string-code
       (concat
	 "print(repr(str(';').join(str(ac) for ac in get_ipython()."
	 "Completer.all_completions('''%s''')).strip()))"
	 ""))))


(defun xorns-python-indent-rigidly (start end arg)
  "Indent rigidly the region.

START and END mark the region.  ARG will be used to tell whether to indent or
outdent.

This simply calls `indent-rigidly' using ±4 spaces."
  (interactive "r\np")

  ;; TODO: Take ±4 from a configuration/environmental feature
  (if (= arg 1)  ;; the non-arg
    (indent-rigidly start end 4)
    (indent-rigidly start end -4)))

(when (xorns-configure-p 'general)  ;; Make C-x C-tab indent rightly in Python
  (define-key python-mode-map (kbd "C-x <C-tab>") 'xorns-python-indent-rigidly)
  (define-key python-mode-map (kbd "C-x C-x <tab>") 'xorns-python-indent-rigidly))


;; Python for reST

(when (xorns-configure-p 'maximum)
  (add-hook 'text-mode-hook
    (lambda ()
      (define-key rst-mode-map (kbd "C-c C-r !")
        'xorns-python-shell-send-paste)
      (define-key rst-mode-map (kbd "C-c C-r C-r")
        'xorns-python-shell-send-cpaste))))


;;;###autoload
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



;; Java and Scala

(when (xorns-configure-p 'general)
  (if (featurep 'javadoc-lookup)
    (progn
      ;; TODO: Check for each item before adding
      (let ((roots
              (loop
                for dir in (list
                             "/usr/share/doc/openjdk-7-jdk/api"
                             "/usr/share/doc/scala-doc/html"
                             "/usr/share/doc/libjsoup-java/api"
                             "~/.local/share/doc/play/content/api/scala"
                             "~/.local/share/doc/akka-actor_2.10-2.2.0"
                             "~/.local/share/doc/akka-slf4j_2.10-2.2.0"
                             ;; Look for "akka-doc" and also our projects
                             )
                if (file-exists-p dir)
                collect dir)))
        (when roots (apply 'javadoc-add-roots roots)))
      )
    ;; else
    (xorns-missing-feature 'javadoc-lookup))
  (if (featurep 'scala-mode)
    (progn
      (define-key scala-mode-map "\C-ch" 'javadoc-lookup))
    ;; else
    (xorns-missing-feature 'scala-mode))
  (define-key java-mode-map "\C-ch" 'javadoc-lookup)
  )


;;; Javascript, CoffeeScript and LiveScript

(when (xorns-configure-p 'general)
  (progn
    ;; This requires you have the tern program installed in your system and in
    ;; the exec-path.
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
    (add-hook 'js-mode-hook (lambda () (tern-mode t)))
    (eval-after-load 'tern
      '(progn
         (require 'tern-auto-complete)
         (tern-ac-setup)))))




;;; Linux kernel programming

(defcustom xorns-linux-kernel-trees-path
  "~/src/linux-trees"
  "Where do you put the linux kernel source trees."
  :group 'xorns
  :type 'string)

(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces.  IGNORED is ignored."
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
      c-basic-offset)))

(when (xorns-configure-p 'general)
  (add-hook 'c-mode-common-hook
    (lambda ()
      ;; Add kernel style
      (c-add-style
        "linux-tabs-only"
        '("linux" (c-offsets-alist
                    (arglist-cont-nonempty
                      c-lineup-gcc-asm-reg
                      c-lineup-arglist-tabs-only))))))

  (add-hook 'c-mode-hook
    (lambda ()
      (let ((filename (buffer-file-name)))
        ;; Enable kernel mode for the appropriate files
        (when (and filename
                (string-match (expand-file-name xorns-linux-kernel-trees-path)
                  filename))
	  (xorns-set-values
	    '(indent-tabs-mode t)
	    '(show-trailing-whitespace t))
          (c-set-style "linux-tabs-only"))))) )




;;;###autoload
(defun xorns-prog-dependencies-install ()
  "Install all dependencies of text modes."
  (xorns-dependency-install 'flycheck)
  (xorns-dependency-install 'yasnippet)
  (xorns-dependency-install 'jedi)
  (xorns-dependency-install 'js2-mode)
  (xorns-dependency-install 'tern)
  (xorns-dependency-install 'tern-auto-complete)
  )


(global-set-key (kbd "C-M-,") 'completion-at-point)

(provide 'xorns-prog)
;;; xorns-prog.el ends here
