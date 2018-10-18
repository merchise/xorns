;;; xorns-sketch --- Structures that are pending of review.

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Rodriguez <med@merchise.org>
;; URL: http://dev.merchise.org/emacs/xorns-sketch
;; Keywords: initialization, merchise, convenience
;; Version: 20181018.0944

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


(require 'flycheck nil 'noerror)


(defun xorns-python-shell ()
  "Return the command to use as python shell.

To calculate the value, test first the custom value of equal name and
if not valid, looks up in a list of alternatives (in order):
`ipython', custom Emacs variable `python-command', environment
variable `PYTHON' and custom variables `python-python-command' and
`python-jython-command'."
  (xorns-executable-find
    (xorns-get-value 'python-shell-interpreter)
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


(defun xorns-python-shell-send-paste (start end)
  "Send the region delimited by START and END wrapped with a %paste magic."
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
      ;; (with-current-buffer "*Python Shell*" (term-send-raw-string "x = 1\n")?
      (comint-send-string process "%paste\n"))))


(add-hook 'python-mode-hook
  (lambda ()
    (condition-case err
      (progn
        (define-key python-mode-map (kbd "C-c C-r")
          'xorns-python-shell-send-paste))
      (error (message "error@python-mode-hook: %s" err)))))


(when (xorns-configure-p 'maximum)
  (add-hook 'text-mode-hook
    (lambda ()
      (define-key rst-mode-map (kbd "C-c C-r !")
        'xorns-python-shell-send-paste)
      (define-key rst-mode-map (kbd "C-c C-r C-r")
        'xorns-python-shell-send-cpaste))))


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



;;; flycheck

(when nil    ;; (xorns-configure-p 'basic)
  (if (featurep 'flycheck)
    (progn
      (add-hook 'after-init-hook
        (lambda ()
          (unless (tramp-connectable-p (buffer-file-name))
            (global-flycheck-mode))))
      (xorns-set-value 'flycheck-idle-change-delay 60)
      )
    ;; else
    ;; (xorns-missing-feature 'flycheck)
    ))



(provide 'xorns-sketch)
;;; xorns-sketch.el ends here
