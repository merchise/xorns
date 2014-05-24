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

;; This module is automatically used when::
;;
;;     (require 'xorns)

;; Enjoy!


;;; Code:

(require 'outline)
(require 'dash nil 'noerror)   ; facilities like -when-let and -mapcat
(require 'flycheck nil 'noerror)
(require 'yasnippet nil 'noerror)
(require 'python nil 'noerror)
(require 'jedi nil 'noerror)
(require 'cc-mode nil 'noerror)
(require 'scala-mode nil 'noerror)
(require 'javadoc-lookup nil 'noerror)

(require 'xorns-text)


;;; Some variables
(setq
  emacs-lisp-docstring-fill-column 78
  lisp-indent-offset 2
   make-backup-files nil    ;; Use Version Control instead ;)
  )


;;;###autoload
(defun xorns-python-shell-send-cpaste (start end)
  "Send the region delimited by START and END wrapped with a %cpaste magic."
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
  (comint-send-string (python-shell-get-or-create-process)
    (concat
      "%cpaste\n"
      (buffer-substring start end)
      "\n")))


;;; Hooks

(when (xorns-configure-p 'basic)
  (if (featurep 'flycheck)
    (progn
      (add-hook 'after-init-hook
	(lambda ()
	  (global-flycheck-mode)))
      (setq
	flycheck-disabled-checkers '(rst-sphinx python-pylint)
	flycheck-idle-change-delay 1.5))
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
	(xorns-auto-complete-mode)
	(flyspell-prog-mode)
	(turn-on-auto-fill)
	(ispell-change-dictionary "english")
	(subword-mode nil)
	(linum-mode 1))
      (error (message "error@prog-mode-hook: %s" err)))))


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
      (error (message "error@prog-mode-hook: %s" err)))))



;;; Python

(add-hook 'python-mode-hook
  (lambda ()
    (condition-case err
      (progn
	(define-key python-mode-map "\C-m" 'newline-and-indent)
	(outline-minor-mode))
      (error (message "error@python-mode-hook: %s" err)))))


(when (xorns-configure-p 'basic)
  (add-hook 'python-mode-hook
    (lambda ()
      (condition-case err
	(xorns-jedi-setup)
	(error (message "error@python-mode-hook: %s" err))))))


(when (xorns-configure-p 'general)
  (add-hook 'inferior-python-mode-hook
    ;; Avoid sending TABs to ipython process, otherwise the ipython will respond
    ;; with autocompletion.
    (lambda ()
      (condition-case err
	(progn
	  (setq indent-tabs-mode nil)
	  (linum-mode 0))
	(error (message "error@inferior-python-mode-hook: %s" err)))))

  (setq
    ;; Configure `ipython` as shell when use function `run-python` and other
    ;; related commands.
    ;; This configuration is based in the way we, in Merchise, configure
    ;; IPython.  See README documentation for more information.
    python-shell-interpreter "ipython"
    ;; Next is essentially configured in `ipython_config.py` as:
    ;; c.PromptManager.in_template = r'\#> \u:\w \$\n>>> '
    python-shell-prompt-regexp "\\(^[0-9]+> .* [$]\\|>>> \\)"
    python-shell-prompt-pdb-regexp "i?pdb> "
    python-shell-prompt-output-regexp "\\(\\s-{0,4}\\|    \\)"
    python-shell-completion-setup-code
       "from IPython.core.completerlib import module_completion"
    python-shell-completion-module-string-code
      (concat
        "print(repr(str(';').join(str(ac) "
        "for ac in module_completion('''%s''')).strip()))\n")
    python-shell-completion-string-code
      (concat
	"print(repr(str(';').join(str(ac) for ac in get_ipython()."
	"Completer.all_completions('''%s''')).strip()))\n")))



;; Python for reST

(when (xorns-configure-p 'maximum)
  (add-hook 'text-mode-hook
    (lambda ()
      (define-key rst-mode-map (kbd "C-c C-r !")
	'xorns-python-shell-send-cpaste)
      (define-key rst-mode-map (kbd "C-c C-r C-r")
	'xorns-python-shell-send-cpaste))))


;;;###autoload
(defun xorns-jedi-setup ()
  "Setup `jedi' for current buffer."
  (if (featurep 'jedi)
    (progn
      (jedi:setup)
      (define-key python-mode-map "\C-ch" 'jedi:show-doc))
    ;; else
    (xorns-missing-feature 'jedi)))



;; Java and Scala

(when (xorns-configure-p 'general)
  (if (featurep 'javadoc-lookup)
    (progn
      ;; TODO: Check for each item before adding
      (javadoc-add-roots
	"/usr/share/doc/openjdk-7-jdk/api"
	"/usr/share/doc/scala-doc/html"
	"/usr/share/doc/libjsoup-java/api"
	"~/.local/share/doc/play/content/api/scala"
	"~/.local/share/doc/akka-actor_2.10-2.2.0"
	"~/.local/share/doc/akka-slf4j_2.10-2.2.0"
	;; Look for "akka-doc" and also our projects
	)
      (setq browse-url-browser-function 'browse-url-chromium)
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


;;;###autoload
(defun xorns-prog-dependencies-install ()
  "Install all dependencies of text modes."
  (xorns-dependency-install 'flycheck)
  (xorns-dependency-install 'yasnippet)
  (xorns-dependency-install 'jedi))


(provide 'xorns-prog)
;;; xorns-prog.el ends here
