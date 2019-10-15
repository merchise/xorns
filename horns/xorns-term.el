;;; xorns-term.el --- Terminal support

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;;; Commentary:

;; xorns-term is our interface to the `ansi-term' (general command
;; interpreter).  Shell categories are configured in `xorns-term-shells'; the
;; command `xorns-ansi-term' is used to launch (or select) a
;; terminal\-emulator; `xorns-ansi-term-paste' paste selected content into one
;; using `comint-mode'.
;;
;; Automatically key\-bindings in `xorns-term-launch-keys' are configured to
;; launch the terminal shells; in `xorns-term-paste-keys' to paste content
;; into a shell; and in `xorns-term-toggle-mode-key' to to toggle `ansi-term'
;; mode between `term-line-mode' and `term-char-mode'.
;;
;; This module is automatically used when:
;;
;;     (require 'xorns)

;; Enjoy!


;;; Code:

(require 's)
(require 'term)
(require 'advice)
(require 'xorns-utils)


(defun xorns-python-shell ()
  "Return the command to use as python shell.

To calculate the value, test first the custom value of equal name and
if not valid, looks up in a list of alternatives (in order):
`ipython', custom Emacs variable `python-command', environment
variable `PYTHON' and custom variables `python-python-command' and
`python-jython-command'."
  (xorns-executable-find
    (bound-and-true-p python-shell-interpreter)
    (getenv "PYTHON")
    (xorns-get-original-value 'python-shell-interpreter)
    "ipython" "python"))


(defun xorns-python3-shell ()
  "Command to use as python\-3 shell.

In this case there is not a paired custom variable.  To calculate the
value to return, this function tests first two alternatives:
`ipython3' and `python3'.  If none is valid, use the logic for the
python shell defined in function `xorns-python-shell'."
  (let ((py3 (xorns-executable-find "ipython3" "python3")))
    (or py3 (xorns-python-shell))))



;;; Remove in future version

(defun xorns-get-ansi-term-shell-name (&optional arg)
  "Get the shell name for a `ansi-term' (based in ARG)."
  (let*
    ((in-python (eq major-mode 'python-mode))
      (shell
        (cond
          ((null arg) (if in-python 'Python 'System))
          ((= (prefix-numeric-value arg) 3) 'Python-3)
          ((if in-python 'System 'Python)))))
    shell))



;;; Misc

(defun xorns-system-shell ()
  "Get the command to use as System Shell.

To calculate the value, looks up in a list of alternatives (in order):
environment variable `SHELL', custom Emacs variable `shell-file-name',
environment variable `ESHELL', and any of [`bash' `sh' `ksh' `zsh' `tclsh'
`csh' `tcsh']."
  (xorns-executable-find
    (getenv "SHELL")
    (bound-and-true-p shell-file-name)
    (xorns-get-original-value 'shell-file-name)
    (getenv "ESHELL")
    "bash" "sh" "ksh" "zsh" "tclsh" "csh" "tcsh"))


;;; Ansi Terminal


(defgroup xorns-term nil
  "Running `xorns' shell from within Emacs buffers."
  :prefix "xorns-term-"
  :group 'xorns
  :group 'shell)


(defcustom xorns-term-shells nil
  "Terminal shells (definition list).

The value is a list `((INDEX PROGRAM MODE BUFFER-NAME PASTE) ...)'.

INDEX (integer), used with numeric prefix arguments to select the shell kind.
Values must be customized in priority sorted: `0' for system; `1' for
preferred programmer language; `2' for working project (like 'xoeuf' in
Merchise), ...

PROGRAM (string), the command to execute when launching a new terminal shell,
for example '/bin/bash'.

MODE (symbol), preferred `major-mode'.

BUFFER-NAME (string), template to calculate the buffer name using `s-format'
function.  The placeholders `${index}', `${program}', `${mode}', and `${name}'
can be used.  If not given, default value is taken from
`xorns-term-default-buffer-name-template'.

NAME is generated either removing the suffix `-mode' from the MODE value, or
with the PROGRAM value.

PASTE (string or function), how to send content from any buffer to a terminal
shell process.  If not given, the content will be used literally; when the
value contains '%s', the content will be pre-formatted; on any other string
value, the content is yanked into the clipboard, and the specified value is
sent alone (this method is useful in shells like 'IPython' using '%paste'
magic macro); a function is used to process the content in a custom defined
way."
  :type '(repeat
           (list
             (integer :tag "Index")                ;; 0
             (choice :tag "Program"                ;; 1
               (string :tag "Executable")
               (cons :tag "With Options"
                 (string :tag "Executable")
                 (repeat :tag "Switches"
                   (string :tag "Option"))))
             (symbol :tag "Mode")                  ;; 2
             (string :tag "Buffer Name")           ;; 3
             (choice :tag "Paste"                  ;; 4
               (string :tag "Template")
               (function :tag "Function"))))
  :group 'xorns-term)


(defcustom xorns-term-default-buffer-name-template "<${index}> ${name}"
  "Default template when not given in a shell definition."
  :type 'string
  :group 'xorns-term)


(defcustom xorns-term-launch-keys (list (kbd "C-c t"))
  "List of key\-bindings, each one will launch a terminal shell."
  :type '(repeat  key-sequence)
  :group 'xorns-term)


(defcustom xorns-term-paste-keys (list (kbd "C-c C-t"))
  "List of key\-bindings, each one will paste content into a terminal shell."
  :type '(repeat key-sequence)
  :group 'xorns-term)


(defcustom xorns-term-toggle-mode-key (kbd "C-c C-t")
  "A key\-binding to toggle between `term-line-mode' and `term-char-mode'.

This could be included as one of those defined in `xorns-term-paste-keys'."
  :type 'key-sequence
  :group 'xorns-term)


(defvar xorns-term-shell-program nil
  "Local variable to store shell program executable.")


(defvar xorns-term-shell-index nil
  "Local variable to store shell index.")


(defadvice ansi-term (after xorns-register-shell-info
                       (program &optional new-buffer-name)
                       activate)
  "Store `xorns-term-shell-program' local variable."
  (unless xorns-term-shell-program
    (set (make-local-variable 'xorns-term-shell-program) program)))


(defadvice term-exec (before xorns--term-exec
                       (buffer name command startfile switches)
                       activate)
  "Set argument 'switches' if defined in local scope."
  (if (null switches)
    (let ((aux (bound-and-true-p xorns--program-switches)))
      (if aux
        (setq switches aux)))))



(defun xorns--shell-normalize (&optional shell)
  "Convert any SHELL context to a proper registry as in `xorns-term-shells'."
  (declare (pure t) (side-effect-free t))
  (cond
    ((null shell)
      ;; default
      (assq 0 xorns-term-shells))
    ((listp shell) shell)
    ((bufferp shell)
      (with-current-buffer shell
        (list xorns-term-shell-index xorns-term-shell-program)))
    ((integerp shell) (assq shell xorns-term-shells))))


(defun xorns--shell-get-index (shell)
  "Get the defined mode for the given SHELL."
  (declare (pure t) (side-effect-free t))
  (car (xorns--shell-normalize shell)))


(defun xorns--shell-get-program (shell)
  "Get the defined program for the given SHELL."
  (declare (pure t) (side-effect-free t))
  (nth 1 (xorns--shell-normalize shell)))


(defun xorns--shell-get-program-executable (shell)
  "Get the defined program executable part for the given SHELL."
  (declare (pure t) (side-effect-free t))
  (let ((res (xorns--shell-get-program shell)))
    (if (listp res)
      (car res)
      ;; else
      res)))


(defun xorns--shell-get-program-switches (shell)
  "Get the defined program arguments part for the given SHELL."
  (declare (pure t) (side-effect-free t))
  (let ((aux (xorns--shell-get-program shell)))
    (if (listp aux)
      (cdr aux))))


(defun xorns--shell-get-mode (shell)
  "Get the defined mode for the given SHELL."
  (declare (pure t) (side-effect-free t))
  (let ((res (nth 2 (xorns--shell-normalize shell))))
    (if (eq res '##) nil res)))


(defun xorns--shell-get-name (shell)
  "Get the defined program arguments part for the given SHELL."
  (declare (pure t) (side-effect-free t))
  (let ((mode (xorns--shell-get-mode shell)))
    (if mode
      (replace-regexp-in-string "-mode$" "" (symbol-name mode))
      ;; else
      (file-name-base (xorns--shell-get-program-executable shell)))))


(defun xorns--shell-get-buffer-name (shell)
  "Get the defined program arguments part for the given SHELL."
  (declare (pure t) (side-effect-free t))
  (let ((shell (xorns--shell-normalize shell)))
    (if shell
      (let ((res (nth 3 shell)))
        (if (or (null res) (string= res ""))
          (setq res xorns-term-default-buffer-name-template))
        (if (string-match "${" res)
          (setq res
            (s-format res 'aget
              (list
                (cons 'index (xorns--shell-get-index shell))
                (cons 'program (xorns--shell-get-program-executable shell))
                (cons 'mode (xorns--shell-get-mode shell))
                (cons 'name (xorns--shell-get-name shell))))))
        res))))



(defvar xorns-term-mode-shell-mapping nil
  "Store cache for `xorns-term-mode-shell-mapping' function.")


(defun xorns-term-mode-shell-mapping ()
  "Mapping between major modes and preferred shells.

Manage which `ansi-term' kind to favor in each `major-mode'.  Return an
association list `((MAJOR-MODE . SHELL-INDEX) ...)', and it is calculated from
the field 'Major modes' in `xorns-term-shells'."
  (or xorns-term-mode-shell-mapping
    (delq nil
      (mapcar
        (lambda (shell)
          (let ((mode (xorns--shell-get-mode shell)))
            (if mode
              (cons mode (xorns--shell-get-index shell)))))
        xorns-term-shells))))


(defun xorns--shell-get (index)
  "Obtain the registered or live shell data for the given INDEX."
  (or
    ;; Live buffer
    (car
      (delq nil
        (mapcar
          (lambda (buf)
            (with-current-buffer buf
              (if (eq index xorns-term-shell-index)
                buf)))
          (buffer-list))))
    ;; Registered shell
    (assq index xorns-term-shells)))


(defun xorns--current-mode-get-shell ()
  "Obtain the shell data that fits the `major-mode' for the current buffer.

Return either an index or program, if already in a terminal; or a list for a
shell definition registry.  nil represents that no shell is preferred for the
current mode."
  (or
    ;; Already in a terminal
    xorns-term-shell-index
    xorns-term-shell-program
    ;; Registered shell
    (let ((id (alist-get major-mode (xorns-term-mode-shell-mapping))))
      (assq id xorns-term-shells))))


(defun xorns--ansi-term (shell)
  "Start or select a SHELL."
  (let ((index (xorns--shell-get-index shell))
         (program (xorns--shell-get-program-executable shell))
         (switches (xorns--shell-get-program-switches shell))
         (buffer-name (xorns--shell-get-buffer-name shell)))
    (message
      ">>> Opening terminal with INDEX %s and PROGRAM %s" index program)
    (let ((xorns--program-switches switches))
      (ansi-term program buffer-name))))


;; (buffer-live-p buffer)
;; (term-check-proc buffer)


(defun xorns--shell-get-new-index ()
  "Calculate an unused new shell index."
  (let ((buffer-indexes
          (delq nil
            (mapcar
              (lambda (buf)
                (with-current-buffer buf
                  xorns-term-shell-index))
              (buffer-list)))))
    (let ((index 0) res)
      (while (null res)
        (if (or (memq index buffer-indexes) (assq index xorns-term-shells))
          ;; next one
          (setq index (1+ index))
          ;; when not found, it is a match
          (setq res index)))
      res)))


(defun xorns--launch-ansi-term (&optional shell)
  "Launch or select a terminal shell.

SHELL definition must be an integer (index on the registry); an string
representing a program executable; or nil to launch the default shell with
index 0."
  (let
    ((shell*
       (cond
         ((null shell) (xorns--shell-get 0))
         ((integerp shell) (xorns--shell-get shell))
         ((stringp shell) (list (xorns--shell-get-new-index) shell))
         (t shell)    ;; assuming `(bufferp shell)'
         )))
    (if (listp shell*)
      (xorns--ansi-term shell)
      ;; else, bufferp
      (switch-to-buffer shell))))


(defun xorns--clone-ansi-term (source)
  "Clone a current terminal SHELL from a SOURCE index or program definition."
  nil
  )


(defun xorns--default-ansi-term ()
  "Launch or select a terminal shell depending on current `major-mode'."
  (let ((shell (xorns--current-mode-get-shell)))
    (cond
      ((null shell) (xorns--launch-ansi-term))
      ((or (integerp shell) (stringp shell)) (xorns--clone-ansi-term shell))
      (t (xorns--launch-ansi-term (car shell)))))    ;; assuming list
  )


(defun xorns-ansi-term* (&optional arg)
  "Launch or select a terminal shell in a buffer.

A shell could be either launched depending on the current `major-mode', a
configured IDENTIFIER in `xorns-term-shells', cloning the shell parameters in
a live buffer, or asking the user the command to execute.  Default shell is
system shell (identified by '0').

The prefix ARG could be:

- None (nil); current `major-mode' is used to determine which shell to launch.

- An integer; IDENTIFIER to either select a live shell, launch a
  registered one, or use the current `major-mode' logic to generate a new
  buffer with that index.

- Plain prefix argument; a new shell is launched with a new index.  In case of
  `universal-argument' (\\[universal-argument]), the command to execute is
  asked to the user; in case of `negative-argument' (\\[negative-argument]),
  the current shell context is cloned."
  (interactive "P")
  (cond
    ((null arg) (xorns--default-ansi-term))
    ((integerp arg) (xorns--shell-get arg))
    ((listp arg) (xorns--current-mode-get-shell))    ;; TODO: ????
    ((symbolp arg) (xorns-completing-read "Run program" nil))    ;; '-
    ))


(defun xorns-ansi-term (&optional arg)
  "Start a terminal\-emulator in a new buffer (based in ARG)."
  (interactive "P")
  (let*
    (
      (shell (xorns-get-ansi-term-shell-name arg))
      (cmd
        (cond
          ((eq shell 'System)
            (xorns-system-shell))
          ((eq shell 'Python)
            (xorns-python-shell))
          (;else
            (xorns-python3-shell))))
      (buf-name (format "%s Shell" shell))
      (starred (format "*%s*" buf-name))
      (cur-buf (get-buffer starred))
      (cur-proc (get-buffer-process cur-buf)))
    (if cur-buf
      (if cur-proc
        (progn
          (setq cmd nil)
          (switch-to-buffer cur-buf))
                                        ;else
        (message ">>> Killing buffer: %s" starred)
        (kill-buffer cur-buf)))
    (if cmd
      (progn
        (message ">>> Opening: %s" starred)
        (ansi-term cmd buf-name))
    ;else
      cur-buf)))


(defun xorns-ansi-term-paste (&optional arg)
  "Paste content into a `ansi-term' shell (based in ARG)."
  (interactive "P")
  (message ">>>> PASTE-ARG = '%s' of type: %s" arg (type-of arg)))


(defun xorns-toggle-term-mode ()
  "Toggle term-mode between `term-line-mode' and `term-char-mode'."
  (interactive)
  (if (term-in-char-mode)
    (term-line-mode)
    ;;else
    (term-char-mode)))


(dolist (key xorns-term-launch-keys)
  (global-set-key key 'xorns-ansi-term))
(dolist (key xorns-term-paste-keys)
  (global-set-key key 'xorns-ansi-term-paste))


(if xorns-term-toggle-mode-key
  (add-hook 'term-mode-hook
    (lambda ()
      (define-key term-mode-map
	xorns-term-toggle-mode-key 'xorns-toggle-term-mode)
      (define-key term-raw-map
	xorns-term-toggle-mode-key 'xorns-toggle-term-mode))))


(provide 'xorns-term)
;;; xorns-term.el ends here
