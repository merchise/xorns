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
  (require 'xorns-tools))
(require 'tab-line)



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



;;; Configure `tab-line' use

(defvar >>=|tab-line/initial-mode 'toolbox
  "Initial mode for `tab-line' configuration.

The following options are possible:
- If nil no configure the `tab-line'.
- Symbol `toolbox', or boolean t, to enable `toolbox-locked-tab-line-mode'.
- Symbol `global' to enable `global-tab-line-mode'.")


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


(defun >>=filter-buffer-list-by-mode (mode &optional frame)
  "Return a list of all live buffers filtered by MODE.
If the optional arg FRAME is a frame, return the buffer list in the
proper order for that frame: the buffers shown in FRAME come first,
followed by the rest of the buffers."
  (delq nil
    (mapcar
      (lambda (buffer)
        (when (eq (buffer-local-value 'major-mode buffer) mode)
          buffer))
      (buffer-list frame))))



;;; Toolbox variables

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


(defvar >>-toolbox/properties nil
  "Local property list to store options of toolbox panel buffers.
A newly created toolbox panel buffer should call `>>=toolbox/setup-new-buffer'
to initialize this variable.")



;;; Toolbox utility functions

(define-minor-mode toolbox-locked-tab-line-mode
  "Toggle whether `tab-line' is used only for toolbox buffers."
  :init-value nil
  :lighter " tbox-tabs"
  :global t
  :group 'toolbox
  (let ((orgfun (>>=get-original-value tab-line-tabs-function)))
    (if toolbox-locked-tab-line-mode
      (progn
        ;; change `tabs-function'
        (unless (eq tab-line-tabs-function orgfun)
          (put 'toolbox-locked-tab-line-mode :function-backup
            tab-line-tabs-function))
        (setq tab-line-tabs-function '>>=toolbox/tab-line-buffer-list)
        ;; check `global-mode'
        (when global-tab-line-mode
          (warn ">>= `%s' must be disabled to enable `%s' mode"
            'global-tab-line-mode 'toolbox-locked-tab-line-mode)
          (put 'toolbox-locked-tab-line-mode :global-mode t)
          (global-tab-line-mode -1))
        ;; check all live buffers
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer
            (tab-line-mode (if (>>=toolbox-p buffer) +1 -1))))
        ;; advice both standard mode functions
        (advice-add 'tab-line-mode
          :before '>>-toolbar/tab-line-mode)
        (advice-add 'global-tab-line-mode
          :before '>>-toolbar/global-tab-line-mode))
      ;; else
      (progn
        ;; remove advice functions
        (advice-remove 'global-tab-line-mode '>>-toolbar/global-tab-line-mode)
        (advice-remove 'tab-line-mode '>>-toolbar/tab-line-mode)
        ;; restore `tabs-function'
        (setq tab-line-tabs-function
          (or (get 'toolbox-locked-tab-line-mode :function-backup) orgfun))
        (put 'toolbox-locked-tab-line-mode :function-backup nil)
        ;; disable `tab-line' in all live buffers
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer
            (tab-line-mode -1)))
        ;; restore `global-mode' if it was originally set
        (when (get 'toolbox-locked-tab-line-mode :global-mode)
          (put 'toolbox-locked-tab-line-mode :global-mode nil)
          (global-tab-line-mode +1))))))


(defun >>=toolbox-p (buffer-or-name)
  "Return non-nil when BUFFER-OR-NAME is a toolbox buffer."
  (buffer-local-value '>>-toolbox/properties (get-buffer buffer-or-name)))


(defun >>=toolbox/setup-new-buffer (buffer &rest properties)
  "Setup a newly created BUFFER which should be part of the toolbox panel.
Optional PROPERTIES could be given to set some extra initial options.  The
`:toolbox-kind' property is set to the value of `major-mode' unless given in
PROPERTIES."
  (let ((props (>>=plist-fix properties)))
    (unless (plist-get props :toolbox-kind)
      (setq props `(:toolbox-kind ,major-mode ,@(>>=plist-fix properties))))
    (with-current-buffer buffer
      (set (make-local-variable '>>-toolbox/properties) props)
      (when toolbox-locked-tab-line-mode
        (tab-line-mode +1))))
  buffer)


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


(defun >>=toolbox/buffer-list ()
  "Return a list of all live toolbox buffers."
  (delq nil
    (mapcar
      (lambda (buffer)
        (if (>>=toolbox-p buffer) buffer))
      (buffer-list))))


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



;;; Some basic toolbox buffers

(define-obsolete-function-alias '>>=scratch/force
  '>>=toolbox/scratch-buffer "0.9.8")
(defun >>=toolbox/scratch-buffer ()
  "Switch to the *scratch* toolbox buffer, creating a new one if needed."
  (interactive)
  (let ((buffer (get-scratch-buffer-create)))
    (unless (>>=toolbox-p buffer)
      (>>=toolbox/setup-new-buffer buffer))
    (>>=toolbox/switch-to-buffer buffer)))



;;; Configure `tab-line' integration with toolbox buffers

(defun >>=toolbox/tab-line-buffer-list ()
  "Sort list of all live toolbox buffers ready to be used in the `tab-line'."
  (if toolbox-locked-tab-line-mode
    (sort
      (>>=toolbox/buffer-list)
      (lambda (one two)
        (string< (buffer-name one) (buffer-name two))))
    ;; else
    (user-error ">>= `%s' should not be the `%s' function"
      'tab-line-tabs-function '>>=toolbox/tab-line-buffer-list)))


(defun >>-toolbar/tab-line-mode (&optional _)
  "Advice for `tab-line-mode'."
  (when toolbox-locked-tab-line-mode
    (unless (>>=toolbox-p (current-buffer))
      (user-error
        ">>= `%s' only allowed for toolbox buffers if `%s' is enabled"
        'tab-line-mode 'toolbox-locked-tab-line-mode))))


(defun >>-toolbar/global-tab-line-mode (&optional _)
  "Advice for `global-tab-line-mode'."
  (when toolbox-locked-tab-line-mode
    (user-error ">>= `%s' can not be used if `%s' is enabled"
      'global-tab-line-mode 'toolbox-locked-tab-line-mode)))



;;; Configure initial `tab-line' mode

(pcase >>=|tab-line/initial-mode
  ((or 'toolbox 't) (toolbox-locked-tab-line-mode +1))
  ('global (global-tab-line-mode +1)))


(provide 'xorns-window)
;;; xorns-window.el ends here
