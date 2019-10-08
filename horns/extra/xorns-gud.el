;;; xorns-gud.el --- Merchise extensions for "Grand Unified Debugger mode"

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;;; Commentary:

;; Merchise extensions for "Grand Unified Debugger mode" for running GDB and
;; other debuggers.  Also uses `realgud' if installed.

;; Enjoy!


;;; Code:


(require 'realgud)
(require 'dash)
(require 'grizzl)


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


(defun xorns-grizzl-select-cmdbuf()
  "Lets the user select a `realgud' command buffer, unless there's a single
command buffer, in which case returns the buffer directly."
  (interactive)
  (when xorns-realgud-enabled
                                        ; TODO: Generalize this to use `gud' if `realgud' is not present.
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
        (car cmdbuffers)))))

(defun xorns-attach-to-cmdbuf ()
  "Attaches current buffer to a debugging session."
  (interactive)
  (when xorns-realgud-enabled
    (-when-let (cmdbuf (xorns-grizzl-select-cmdbuf))
      (message "Attaching current buffer %s to command buffer %s"
        (current-buffer) cmdbuf)
      (realgud-srcbuf-init-or-update (current-buffer) cmdbuf))))


(provide 'xorns-gud)
;;; xorns-gud.el ends here
