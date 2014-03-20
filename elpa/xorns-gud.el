;;; xorns-gud --- Merchise extensions for "Grand Unified Debugger mode"

;; Copyright (C) 2014 Merchise Autrement

;; Author: Medardo Rodriguez <med@merchise.org>
;; URL: http://dev.merchise.org/emacs/xorns-gud
;; Keywords: initialization, merchise, convenience
;; Version: 20140320.1140

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

;; Merchise extensions for "Grand Unified Debugger mode" for running GDB and
;; other debuggers.  Also uses `realgud' if installed.

;; TODO: This module debugging stuff with `realgud' can be removed when
;; `realgud' has this stuff natively.

;; Enjoy!


;;; Code:


(require 'realgud nil 'noerror)
(require 'dash nil 'noerror)
(require 'grizzl nil 'noerror)


(defcustom xorns-use-realgud nil
   "Indicates whether to use realgud instead of the default `gud'.

Even if this set you must have `realgud', `dash' and `grizzl' installed for
the `xorns' extensions to work.

Currently there's only one extension to attach a source buffer to a command
buffer."
   :group 'xorns
   :type 'boolean)


(defconst xorns-realgud-enabled
  (and xorns-use-realgud
    (functionp 'realgud-srcbuf-init-or-update)    ; Verify `realgud'
    (functionp '-select)                          ; Verify `dash'
    (functionp 'grizzl-make-index))               ; Verify `grizzl'
  "If really `realgud' can be used.")


(when xorns-realgud-enabled

  (defun xorns-grizzl-select-cmdbuf()
    "Lets the user select a `realgud' command buffer, unless there's a single
command buffer, in which case returns the buffer directly."
    (interactive)
    (let ((cmdbuffers (-select #'realgud-cmdbuf? (buffer-list))))
      (if (> (length cmdbuffers) 1)
	(let* ((cmdbuffers-names (-map #'buffer-name cmdbuffers))
		(cmdbuffers-index (grizzl-make-index cmdbuffers-names))
		(selection (grizzl-completing-read
			     "Debugger Buffer: " cmdbuffers-index)))
	  (when selection
	    (message "Selected debugger %s" selection)
	    (get-buffer selection)))
	    ;; else (no buffer or a single one)
	(car cmdbuffers))))

  ;; TODO: [manu] Remove warning "the function `xorns-grizzl-select-cmdbuf'
  ;; is not known to be defined.

  (defun xorns-attach-to-cmdbuf ()
    "Attaches current buffer to a debugging session."
    (interactive)
    (-when-let (cmdbuf (xorns-grizzl-select-cmdbuf))
      (message "Attaching current buffer %s to command buffer %s"
	(current-buffer) cmdbuf)
      (realgud-srcbuf-init-or-update (current-buffer) cmdbuf)))
  )


(provide 'xorns-gud)
;;; xorns-gud.el ends here
