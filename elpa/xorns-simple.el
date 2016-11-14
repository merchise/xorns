;;; xorns-simple --- Merchise basic editing commands for Emacs

;; Copyright (C) 2014-2016 Merchise Autrement [~º/~]

;; Author: Medardo Rodriguez <med@merchise.org>
;; URL: http://dev.merchise.org/emacs/xorns-simple
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

;; Basic Emacs commands not specifically related to any specific major mode
;; or to file-handling.

;; This module is automatically used when::
;;
;;     (require 'xorns)

;; Enjoy!


;;; Code:


(require 'ido)


;;; Custom Variables and Settings

(setq
  column-number-mode t
  ido-auto-merge-delay-time 1.5
  )



;;; Some simple functions

(defun -set-buffer-read-only ()
   "Private function to be used in `xorns-next-grep-result'."
   (setq buffer-read-only t))


(defun xorns-next-grep-result (&optional arg reset)
   "Visit next grep result.

If no grep process is active, find next error in occur buffer.

A prefix ARG specifies how many error messages to move; negative means move
back to previous error messages.  Just \\[universal-argument] as a prefix
means reparse the error message buffer and start at the first error.

The RESET argument specifies that we should restart from the beginning.

In the case of grep results, each visited buffer is marked read-only."
  (interactive "P")
  (if (get-buffer "*grep*")
    (progn
      (add-hook 'next-error-hook '-set-buffer-read-only)
      (ido-visit-buffer "*grep*" 'other-window)
      ;; Previous statement could be generalized like::
      ;; (ido-visit-buffer
      ;;   (next-error-find-buffer 'AVOID-CURRENT)
      ;;   'other-window)
      (next-error arg reset)
      (delete-other-windows)
      (remove-hook 'next-error-hook '-set-buffer-read-only))
    (next-error arg reset)))


(defun xorns-yank-filename ()
  "Make buffer file-name the latest kill in the kill ring."
  (interactive)
  (save-excursion
    (kill-new (or buffer-file-truename (buffer-name)))))



(defun xorns-yank-default-directory ()
  "Make default directory the latest kill in the kill ring."
  (interactive)
  (save-excursion
    (kill-new default-directory)))


(global-set-key (kbd "C-M-g") 'xorns-next-grep-result)
(define-key global-map (kbd "C-c M-w") 'xorns-yank-filename)
(define-key global-map (kbd "C-c C-w") 'xorns-yank-default-directory)

(provide 'xorns-simple)
;;; xorns-simple.el ends here
