;;; xorns-window.el --- Window tree extensions and buffers support  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Window tree functions, configurations related to windows, buffers,
;; manage a toolbox panel, and the use of tabs.

;; Enjoy!


;;; Code:

(eval-and-compile
  (require 'use-package)
  (require 'xorns-tools))
(require 'tab-line)


;;; Configuration variables

;; base

(defvar buffer-setup-hook nil
  "Normal hook run after creating a buffer to configure it.")


;; toolbox

(defvar >>=|toolbox/match-buffer-condition
  '(or
     "[*]scratch[*]"
     (derived-mode . term-mode)
     (derived-mode . eshell-mode))
  "Determine how to switch to toolbox buffers.")


(defvar >>=|toolbox/display-buffer-action nil
  "Determine how to switch to toolbox buffers.

The value of this variable is used by `>>=toolbox/switch-to-buffer' (the main
toolbox function) to call the `display-buffer' function.

The following values are possible:
- Any valid value for the ACTION argument of the `display-buffer' function.
- Symbol `other-window', or boolean t: To display the buffer in a window other
  than the selected one.
- A number, nil, or symbol `bottom': To display window at the bottom of the
  selected frame (see `>>-toolbox/cast-height' on how this option is
  evaluated).  A number means the window height; nil and `bottom' use
  `>>=|toolbox/default-bottom-height' as the default value.  A `floatp' is a
  fraction of the frame's height; and an `integerp' (not recommended) is in
  units of the frame's canonical character height (see `frame-height'
  function).
- Any other symbol (or string) will be used to try an ACTION-FUNCTION by
  formatting `display-buffer-%s'; for example `same-window', to use
  `display-buffer-same-window'.")


(defvar >>=|toolbox/default-bottom-height 0.4
  "Default value for height when toolbox panel is displayed at `bottom'.
See `>>=|toolbox/display-buffer-action' configuration variable.")


(defvar >>=|toolbox/base-action
  '((display-buffer-if-in-visible-window display-buffer-reuse-toolbox-window))
  "Default action for ‘display-buffer’ executed before any logic.")


(defvar >>=|toolbox/fallback-action nil
  "Override `display-buffer-fallback-action' if not nil.")


(defvar >>=|toolbox/same-in-both-directions
  '(display-buffer-same-window)
  "Actions that are the same in both directions.")


(defvar >>=|toolbox/reverse-bottom-action
  '((display-buffer-reuse-mode-window display-buffer-reuse-window))
  "Action to reverse from a bottom toolbox buffer.
When nil, a `top' window with reversed height is used.")


;; tabs

(defvar >>=|tab-line/kind 'toolbox
  "Mode to configure tabs.
This value will only take effect when using the `>>=tab-line/configure'
function.  The following are the possible options:
- If nil, tabs will be disabled.
- Symbol `toolbox', to enable tabs only for toolbox buffers.
- Symbol `extended', to enable tabs for toolbox, for editing, and programming
  related buffers.  See `>>=|tab-line/extended-modes' for more information.
- Symbol `global', to globally enable the tabs for all valid buffers.
- Boolean value t, to use tabs only for buffers with `tab-line-mode' manually
  enabled.")


(defvar >>=|tab-line/extended-modes '(text-mode prog-mode)
  "Modes for which tabs are enabled when `>>=|tab-line/kind' is `extended'.")



;;; Base configuration

(use-package window
  :init
  (defconst >>-window-coach-mode-keys
    '((shrink-window "<up>" "p")
      (enlarge-window "<down>" "n")
      (enlarge-window-horizontally "<right>" "f")
      (shrink-window-horizontally "<left>" "b")
      (other-window "o")
      (>>=window/split-toggle "t")
      (>>=window-coach-mode "C-g" "<RET>")))

  (define-minor-mode >>=window-coach-mode
    "A simple window-coach minor mode."
    :init-value nil
    :lighter " xwc"
    :global t
    :keymap
      (let ((map (make-sparse-keymap)))
        (dolist (item >>-window-coach-mode-keys)
          (let ((fn (car item))
                (keys (cdr item)))
            (dolist (key keys)
              (keymap-set map key fn))))
        map)
    :group 'window)

  (defun >>=window/split-toggle (&optional arg)
    "Toggle horizontal/vertical layout of 2 windows (use ARG to restore)."
    (interactive "P")
    (if (= (count-windows) 2)
      (let* ((tree (car (window-tree)))
              (one (nth 2 tree))
              (two (nth 3 tree))
              (aux (car tree))                ;; t: vertical -> horizontal
              (v2h (if arg (not aux) aux))    ;; (xor arg v2h)
              (state (window-state-get two)))
        (delete-other-windows one)
        (window-state-put
          state
          (funcall
            (if v2h
              #'split-window-horizontally
              ;; else
              #'split-window-vertically))))
      ;; else
      (warn "Only can toggle two windows!")))
  :custom
  (split-width-threshold 120)
  :bind
  ("C-c C-`" . >>=window-coach-mode)
  (:map ctl-x-4-map
    ("t" . >>=window/split-toggle)))


(use-package windmove
  :custom
  (windmove-wrap-around t)
  :config
  (windmove-default-keybindings 'ctrl))


(use-package winner
  :config
  (winner-mode +1))



;;; Basic tools

(defun >>=kill-buffer-and-window (&optional buffer)
  "Kill the specified BUFFER, and delete the window currently displaying it.
Argument nil or omitted means kill the current buffer.  Similar to standard
`kill-buffer-and-window' function but it is just a function to specify a
buffer."
  (unless buffer
    (setq buffer (current-buffer)))
  (let ((win (get-buffer-window buffer)))
    (prog1
      (kill-buffer buffer)
      (when win
        (ignore-errors
          (delete-window win))))))


(defsubst >>=buffer-major-mode (buffer)
  "Return the `major-mode' of BUFFER."
  (buffer-local-value 'major-mode buffer))


(defalias 'standard-buffer-p '>>=standard-buffer-p)
(defun >>=standard-buffer-p (buffer)
  "Define whether the BUFFER is standard.
A buffer is not considered standard if it is temporary, or if its name begins
with a space, or if its `major-mode' is `fundamental-mode'."
  (not
    (or
      (minibufferp buffer)
      (string-match-p "\\` " (buffer-name buffer))
      (eq (>>=buffer-major-mode buffer) 'fundamental-mode))))


(defalias 'buffer-in-mode-p '>>=buffer-in-mode-p)
(defun >>=buffer-in-mode-p (buffer &rest check-modes)
  "Non-nil if `major-mode' of BUFFER is one of the CHECK-MODES."
  (>>=mode-find
    (>>=buffer-major-mode buffer)
    (>>=fix-rest-list check-modes)))


(defalias 'buffer-in-parent-mode-p '>>=buffer-in-parent-mode-p)
(defun >>=buffer-in-parent-mode-p (buffer &rest check-modes)
  "Non-nil if `major-mode' of BUFFER is derived from one of the CHECK-MODES."
  (>>=derived-mode-p
    (>>=buffer-major-mode buffer)
    (>>=fix-rest-list check-modes)))


(defun >>=same-mode-buffers (modes &optional frame)
  "Return a list of all live buffers filtered by MODES.
If the optional argument FRAME is a frame, return the buffer list in the
proper order for that frame: the buffers shown in FRAME come first, followed
by the rest of the buffers."
  (match-buffers 'buffer-in-mode-p (buffer-list frame) modes))


(defun >>=derived-mode-buffers (modes &optional frame)
  "Return a list of all live buffers filtered by derived MODES.
If the optional argument FRAME is a frame, return the buffer list in the
proper order for that frame: the buffers shown in FRAME come first,
followed by the rest of the buffers."
  (match-buffers 'buffer-in-parent-mode-p (buffer-list frame) modes))


(defsubst >>-buffer-key (buffer)
  "Join `major-mode' and name of the BUFFER to form a key for sorting."
  (format "%s/%s" (>>=buffer-major-mode buffer) (buffer-name buffer)))


(defun >>-sort-buffers (one two)
  "Predicate to `sort' buffers ONE and TWO."
  (string< (>>-buffer-key one) (>>-buffer-key two)))


(defun >>-setup-current-buffer ()
  "Configure `current-buffer' by running `buffer-setup-hook'."
  (when (>>=standard-buffer-p (current-buffer))
    (run-hooks 'buffer-setup-hook)))



;;; Toolbox

(defvar >>-toolbox/properties nil
  "Local property list to store options of toolbox panel buffers.
This is set by `>>=toolbox/setup-buffer'.")


(defalias 'toolbox-p '>>=toolbox-p)
(defun >>=toolbox-p (&optional buffer-or-name)
  "Return non-nil when BUFFER-OR-NAME is a toolbox buffer."
  (buffer-local-value
    '>>-toolbox/properties
    (get-buffer (or buffer-or-name (current-buffer)))))


(defun >>=toolbox-buffers ()
  "Return a sorted list of all live toolbox buffers."
  (sort (match-buffers 'toolbox-p) '>>-sort-buffers))


(defun >>=toolbox/setup-buffer (buffer)
  "Configure BUFFER to be part of the toolbox panel."
  (unless (>>=toolbox-p buffer)
    (with-current-buffer buffer
      (set
        (make-local-variable '>>-toolbox/properties)
        `(:toolbox-kind ,major-mode)))
    buffer))


(defun >>=toolbox/set-properties (buffer &rest properties)
  "Update PROPERTIES in a toolbox BUFFER."
  (let ((target (>>=toolbox-p buffer)))
    (if target
      (>>=plist-update target properties)
      ;; else
      (user-error ">>= '%s' is not a toolbox buffer" (buffer-name buffer)))))


(defun display-buffer-if-in-visible-window (buffer alist)
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
           window)
      (while (and (not window) windows)
        (let ((win (car windows)))
          (when (>>=toolbox-p (window-buffer win))
            (unless (and not-same (eq win (selected-window)))
              (setq window win))))
        (setq windows (cdr windows)))
      (when (window-live-p window)
        (prog1 (window--display-buffer buffer window 'reuse alist)
          (unless (cdr (assq 'inhibit-switch-frame alist))
            (window--maybe-raise-frame (window-frame window))))))))


(defun >>-toolbox/cast-action (action)
  "Process a preliminary form for the ACTION value."
  (cond
    ((or (null action) (eq action 'bottom))
      >>=|toolbox/default-bottom-height)
    ((eq action 'other-window)
      t)
    ((or (>>=real-symbol action) (stringp action))
      (>>=check-function (format "display-buffer-%s" action) 'strict))
    (t
      action)))


(defun >>-toolbox/reverse-height (height)
  "Reverse toolbox HEIGHT relative to `window-text-height'."
  (if (floatp height)
    (- 1.0 height (/ (float (line-pixel-height)) (window-pixel-height)))
    ;; else
    (- (window-text-height) height)))


(defun >>-toolbox/cast-height (direction height)
  "Convert HEIGHT to an action for a given DIRECTION."
  (cons
    'display-buffer-in-direction
    `((direction . ,direction) (window-height . ,height))))


(defun >>-toolbox/get-action (buffer)
  "Get action to use `display-buffer' action functions to switch to BUFFER."
  (let ((action (>>-toolbox/cast-action >>=|toolbox/display-buffer-action))
        (is-toolbox (>>=toolbox-p buffer)))
    (cond
      ((eq action t)
        t)
      ((numberp action)
        (if is-toolbox
          (>>-toolbox/cast-height 'bottom action)
          ;; else
          (let ((count (count-windows 'nomini)))
            (if (and >>=|toolbox/reverse-bottom-action (> count 1))
              >>=|toolbox/reverse-bottom-action
              ;; else
              (>>-toolbox/cast-height 'top
                (>>-toolbox/reverse-height action))))))
      ((memq action >>=|toolbox/same-in-both-directions)
        `(,action))
      ((functionp action)
        (if is-toolbox
          `(,action)
          ;; else
          display-buffer-fallback-action))
      (t
        action))))


(defun >>=toolbox/switch-to-buffer (buffer-or-name)
  "Select BUFFER-OR-NAME in the toolbox panel.
The optional argument MODE will take precedence over the variable
`>>=|toolbox/display-buffer-action'."
  (let ((org-fba display-buffer-fallback-action)
        (display-buffer-fallback-action nil)
        (buffer (get-buffer buffer-or-name)))
    (select-window
      (or
        (display-buffer buffer >>=|toolbox/base-action)
        (display-buffer buffer (>>-toolbox/get-action buffer))
        (display-buffer buffer (or >>=|toolbox/fallback-action org-fba))))))


(define-obsolete-function-alias '>>=scratch/force
  '>>=toolbox/scratch-buffer "0.9.8")
(defun >>=toolbox/scratch-buffer ()
  "Switch to the *scratch* toolbox buffer, creating a new one if needed."
  (interactive)
  (let ((buffer (get-scratch-buffer-create)))
    (>>=toolbox/switch-to-buffer buffer)))



;;; Tabs

(use-package tab-line
  :custom
  (tab-line-new-button-show nil)
  (tab-line-close-button-show nil)
  (tab-line-switch-cycling t)
  :config
  (setq tab-line-separator " | ")
  (let ((fg (face-attribute 'default :foreground))
        (bg (face-attribute 'default :background))
        (dark-fg (face-attribute 'shadow :foreground)))
    ;; background behind tabs
    (set-face-attribute 'tab-line nil
      :inherit nil
      :foreground dark-fg
      :background bg
      :height 0.95
      :box nil)
    ;; active tab in current window
    (set-face-attribute 'tab-line-tab-current nil
      :inherit nil
      :foreground fg
      :background bg
      :weight 'extra-bold
      :underline t
      :box nil)
    ;; inactive tab
    (set-face-attribute 'tab-line-tab-inactive nil
      :inherit nil
      :foreground dark-fg
      :background bg
      :weight 'light
      :height 0.85
      :box nil)
    ;; active tab in another window
    (set-face-attribute 'tab-line-tab nil
      :inherit nil
      :foreground dark-fg
      :background bg
      :weight 'extra-bold
      :box nil)
    ;; mouse over
    (set-face-attribute 'tab-line-highlight nil
      :foreground 'unspecified)
      :background bg))


(defun >>-tab-line/valid-buffer (&optional buffer)
  "Return whether BUFFER can be included in the tab line."
  (unless buffer
    (setq buffer (current-buffer)))
  (let ((mode (>>=buffer-major-mode buffer)))
    (and
      (>>=standard-buffer-p buffer)
      (not
        (or
          (memq mode tab-line-exclude-modes)
          (get mode 'tab-line-exclude)
          (buffer-local-value 'tab-line-exclude buffer))))))


(defalias 'extended-mode-buffer-p '>>-tab-line/extended-mode-buffer-p)
(defun >>-tab-line/extended-mode-buffer-p (&optional buffer)
  "Return whether BUFFER is in one of the `extended' modes."
  (buffer-in-parent-mode-p
    (or buffer (current-buffer))
    >>=|tab-line/extended-modes))


(defalias 'extended-buffer-p '>>-tab-line/extended-buffer-p)
(defun >>-tab-line/extended-buffer-p (&optional buffer)
  "Return whether BUFFER is valid for the `extended' tabs."
  (or (toolbox-p buffer) (extended-mode-buffer-p buffer)))


(defalias 'tab-buffer-p '>>-tab-buffer-p)
(defun >>-tab-buffer-p (&optional buffer)
  "Return whether BUFFER must be configured in tabs."
  (pcase >>=|tab-line/kind
    ('toolbox
      (toolbox-p buffer))
    ('extended
      (extended-buffer-p buffer))
    ('global
      t)))


(defun >>=extended-mode-buffers ()
  "All live buffers in one of the extended modes but not a toolbox."
  (sort
    (cl-remove-if 'toolbox-p (match-buffers 'extended-buffer-p))
    '>>-sort-buffers))


(defun >>-xtabs/tab-line-enable ()
  "Turn on `tab-line-mode' in all `xtabs' buffers.
See functions `>>-tab-line/valid-buffer' and `>>-tab-buffer-p' to find which
buffers are eligible."
  (when (and (>>-tab-line/valid-buffer) (tab-buffer-p))
    (tab-line-mode +1)))


(define-globalized-minor-mode xtabs-mode tab-line-mode
  >>-xtabs/tab-line-enable
  :link '(variable-link '>>=|tab-line/kind)
  :group 'xtabs)


(define-minor-mode >>-tab-line-mode
  "Toggle whether tab line is used for `toolbox' and/or `extended' buffers.
This global minor mode is used internally based on the value of the
`>>=|tab-line/kind' variable and should only be configured using the
`>>=tab-line/configure' function."
  :init-value nil
  :lighter " xtabs"
  :global t
  :group 'toolbox
  (unless global-tab-line-mode     ; global is higher-level than toolbox mode
    (let ((arg (if >>-tab-line-mode +1 -1)))
      (dolist (buffer (match-buffers 'tab-buffer-p))
        (with-current-buffer buffer
          (tab-line-mode arg))))))


(defun >>=tab-line/buffers ()
  "List of tabs to display in the `tab-line'.
To be used with `tab-line-tabs-function' variable."
  (cond
    ((toolbox-p)
      (>>=toolbox-buffers))
    ((extended-mode-buffer-p)
      (>>=extended-mode-buffers))
    (t
      (funcall (>>=get-original-value tab-line-tabs-function)))))


(defun >>-tab-line/set-function ()
  "Set `tab-line-tabs-function' to our function."
  (unless (eq tab-line-tabs-function '>>=tab-line/buffers)
    (setq tab-line-tabs-function '>>=tab-line/buffers)))


(defun >>-tab-line/reset-function ()
  "Reset `tab-line-tabs-function' to its original value."
  (when (eq tab-line-tabs-function '>>=tab-line/buffers)
    (>>=restore-original-value tab-line-tabs-function)))


(defun >>-tab-line/set-kind (kind)
  "Change `>>=|tab-line/kind' variable to the value of KIND."
  (if (null kind)
    (progn
      (>>-tab-line-mode -1)
      (global-tab-line-mode -1)
      (>>-tab-line/reset-function))
    ;; else
    (>>-tab-line/set-function)
    (pcase kind
      ((or 'toolbox 'extended)
        (>>-tab-line-mode +1))
      ('global
        (global-tab-line-mode +1)))))


(defun >>=tab-line/configure (kind)
  "Configure tabs using KIND.
In addition to the types defined in the `>>=|tab-line/kind' variable, the
special type `init' can be used for initial configuration."
  (if (eq kind 'init)
    (when >>=|tab-line/kind
      (setq kind >>=|tab-line/kind)
      (>>-tab-line/set-kind kind))
    ;; else
    (setq >>=|tab-line/kind kind)
    (>>-tab-line/set-kind kind)))



;;; Module configuration (hooks, ...)

(defun >>-check-buffer ()
  "Setup `current-buffer' the first time its mode is set."
  (let ((buffer (current-buffer)))
    (cond
      ((buffer-match-p >>=|toolbox/match-buffer-condition buffer)
        (>>=toolbox/setup-buffer buffer)
        (when >>-tab-line-mode
          (tab-line-mode +1)))
      ((and
         >>-tab-line-mode
         (eq >>=|tab-line/kind 'extended)
         (extended-buffer-p buffer))
        (tab-line-mode +1)))))


(defun >>=configure-window-module ()
  "Initial window module configuration."
  (>>=tab-line/configure 'init))


(add-hook 'after-change-major-mode-hook '>>-setup-current-buffer)
(add-hook 'buffer-setup-hook '>>-check-buffer)
(add-hook 'after-init-hook '>>=configure-window-module)


(provide 'xorns-window)
;;; xorns-window.el ends here
