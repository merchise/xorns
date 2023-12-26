;;; xorns-window.el --- Window tree extensions and buffers support  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Window tree functions, configurations related to windows and buffers,
;; definitions to manage a toolbox panel,

;; Enjoy!


;;; Code:

(eval-and-compile
  (require 'xorns-tools))


;;; Variables

(defvar >>=|toolbox/display-buffer-action nil
  "Determine how to switch to toolbox buffers.

The value of this variable is used by `>>=toolbox/switch-to-buffer' (the main
toolbox function) to call the `display-buffer' function.

The following values are possible:
- Any valid value for the ACTION argument of the `display-buffer' function.
- Symbol `other-window', or boolean t: To display the buffer in a window other
  than the selected one.
- A number, nil, or symbol `bottom': To pop up a window at the bottom of the
  selected frame (see `>>=!toolbox/bottom-action' on how this option is
  evaluated).  A number means the window height; nil and `bottom' use
  `>>=|toolbox/default-bottom-height' as the default value for height.  A
  `floatp' is a fraction of the frame's height; and an `integerp' (not
  recommended) is in units of the frame's canonical character height (see
  `frame-height' function).
- Any other symbol (or string) will be used to try an ACTION-FUNCTION by
  formatting `display-buffer-%s'; for example `same-window', to use
  `display-buffer-same-window'.")


(defconst >>=!toolbox/bottom-action
  '(cons
     'display-buffer-in-direction
     ((direction . bottom) `(window-height ,height)))
  "Form to `eval' window height to display toolbox panel at `bottom'.
See `>>=|toolbox/display-buffer-action' configuration variable.")


(defvar >>=|toolbox/default-bottom-height 0.4
  "Default value for height when toolbox panel is displayed at `bottom'.
See `>>=|toolbox/display-buffer-action' configuration variable.")


(defvar >>-toolbox/properties nil
  "Local property list to store options of toolbox panel buffers.
A newly created toolbox panel buffer should call `>>=toolbox/setup-new-buffer'
to initialize this variable.")



;;; Utility functions

(defun >>=toolbox-p (buffer-or-name)
  "Return non-nil when BUFFER-OR-NAME is a toolbox buffer."
  (buffer-local-value '>>-toolbox/properties (get-buffer buffer-or-name)))


(defun display-buffer-in-visible-window (buffer alist)
  "Return a window currently visible and displaying BUFFER.

ALIST is an association list of action symbols and values.  When it has a
non-nil `inhibit-same-window' entry, the selected window is not eligible.  If
it contains a `reusable-frames' entry, its value has the same semantics as the
ALL-FRAMES argument of the `get-buffer-window' function.  All other values are
ignored.

This is an ACTION function, so we don't use the `xorns' naming convention."
  (let* ((frames (cdr (assq 'reusable-frames alist)))
         (not-same (cdr (assq 'inhibit-same-window alist)))
         (windows (get-buffer-window-list buffer 'nomini frames))
         (window (car windows)))
    (when (and not-same (eq window (selected-window)))
      (setq window (cadr windows)))
    window))


(defun display-buffer-reuse-toolbox-window (buffer alist)
  "Return a window displaying a toolbox buffer if the given BUFFER is one.

ALIST is an association list of action symbols and values.  When it has a
non-nil `inhibit-same-window' entry, the selected window is not eligible.  If
it contains a `reusable-frames' entry, its value has the same semantics as the
ALL-FRAMES argument of the `get-buffer-window' function.  All other values are
ignored.

This is an ACTION function, so we don't use the `xorns' naming convention."
  (when (>>=toolbox-p buffer)
    (let* ((frames (cdr (assq 'reusable-frames alist)))
           (not-same (cdr (assq 'inhibit-same-window alist)))
           (windows (window-list-1 nil 'nomini frames))
           result)
      (while (and (not result) windows)
        (let ((window (car windows)))
          (when (>>=toolbox-p (window-buffer window))
            (unless (and not-same (eq window (selected-window)))
              (setq result window))))
        (setq windows (cdr windows)))
      result)))


(defun >>-toolbox/normalized-switch-mode ()
  "Normalize value of `>>=|toolbox/display-buffer-action'."
  (let ((mode >>=|toolbox/display-buffer-action))
    (cond
      ((or (null mode) (eq mode 'bottom))
        >>=|toolbox/default-bottom-height)
      ((or (numberp mode) (memq mode '(other-window same-window)))
        mode)
      (t
        (error ">>= switch to buffer mode invalid value: %s" mode)))))


(defun >>-toolbox/reverse-height ()
  "Reverse toolbox height relative to `window-text-height'."
  (let ((height (>>-toolbox/normalized-switch-mode)))
    (if (floatp height)
      (- 1.0 height (/ (float (line-pixel-height)) (window-pixel-height)))
      ;; else
      (- (window-text-height) height))))


(defsubst >>=toolbox/get-alist (direction &optional height)
  "Return an `alist' to use as parameter to display a buffer at DIRECTION.
An optional HEIGHT can be used."
  (cons `(direction . ,direction) (if height `((window-height . ,height)))))


(defun >>=toolbox/setup-new-buffer (buffer &rest properties)
  "Setup a newly created BUFFER which should be part of the toolbox panel.
Optional PROPERTIES could be given to set some extra initial options.  The
`:toolbox-kind' property is set to the value of `major-mode' unless given in
PROPERTIES."
  (let ((props (>>=plist-fix properties)))
    (unless (plist-get props :toolbox-kind)
      (setq props `(:toolbox-kind ,major-mode ,@(>>=plist-fix properties))))
    (with-current-buffer buffer
      (set (make-local-variable '>>-toolbox/properties) props)))
  buffer)


(defun >>=toolbox/hide-windows (windows &optional exception)
  "Hide all members of WINDOWS with the EXCEPTION of the optional argument.
Returns the list of all windows that were not deleted."
  (delete nil
    (mapcar
      (lambda (win)
        (condition-case nil
          (if (equal win exception)
            win
            ;; else
            (delete-window win)
            nil)
          (error win)))    ; for example, attempting to delete the main window
      windows)))


(defun >>=toolbox/switch-to-linked (buffer-or-name)
  "Select BUFFER-OR-NAME at the alternate side of the selected window."
  (when-let ((buf (get-buffer buffer-or-name)))
    (select-window
      (or
        (when (eq (length (window-list)) 1)
          (display-buffer-in-direction buf
            (>>=toolbox/get-alist 'top (>>-toolbox/reverse-height))))
        (display-buffer-in-previous-window buf nil)
        (display-buffer buf)))))


(defun >>=toolbox/switch-to-bottom (buffer-or-name &optional height)
  "Select BUFFER-OR-NAME in a window at the bottom of the selected frame.
Argument window HEIGHT is optional."
  (when-let ((buf (get-buffer buffer-or-name)))
    (select-window
      (display-buffer-in-direction buf
        (>>=toolbox/get-alist 'bottom height)))))


(defun >>=toolbox/switch-to-same-window (buffer-or-name)
  "Select BUFFER-OR-NAME in the selected window."
  (when-let ((buf (get-buffer buffer-or-name)))
    (select-window
      (display-buffer-same-window buf nil))))


(defun >>=toolbox/switch-to-buffer (buffer-or-name)
  "Select BUFFER-OR-NAME in the toolbox panel.
The optional argument MODE will take precedence over the variable
`>>=|toolbox/display-buffer-action'."
  (let* ((buf (get-buffer buffer-or-name))
         (wins (get-buffer-window-list buf)))
    (when (and (> (length wins) 1) (>>=toolbox-p buf))
      (setq wins (>>=toolbox/hide-windows wins (car wins))))
    (if wins
      (select-window (car wins))
      ;; else
      (let ((mode (>>-toolbox/normalized-switch-mode)))
        (cond
          ((eq mode 'other-window)
            (switch-to-buffer-other-window buf))
          ((eq mode 'same-window)
            (>>=toolbox/switch-to-same-window buf))
          (t
            (if (>>=toolbox-p buf)
              (>>=toolbox/switch-to-bottom buf mode)
              ;; else
              (>>=toolbox/switch-to-linked buf))))))))


(provide 'xorns-window)
;;; xorns-window.el ends here
