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
  "Normal hook run after creating a buffer to configure it.
Only buffers that match condition `standard-buffer-p' are configured.")


;; toolbox

(defvar >>=|toolbox/buffer-match-condition
  '(or
     "[*]scratch[*]"
     (derived-mode . comint-mode)
     (derived-mode . shell-mode)
     (derived-mode . eshell-mode)
     (derived-mode . term-mode)
     (derived-mode . vterm-mode))
  "Determine whether a buffer should be configured to be part of the toolbox.
The value of this variable must be a valid CONDITION for the function
`buffer-match-p'.  Once a buffer is configured, the `toolbox-p' function can
be used to check whether it is a toolbox buffer or not.")


(defvar >>=|toolbox/display-buffer-action nil
  "Determine how to switch to/from toolbox buffers.

The following values are possible options:
- Symbol `bottom', nil, or a number: display toolbox buffer at the bottom of
  the selected frame.  A number means the window height; nil and `bottom' use
  `>>=|toolbox/default-bottom-height' as the default value.  A `floatp' is a
  fraction of the frame's height; and an `integerp' (not recommended) is in
  units of the frame's canonical character height (see `frame-height'
  function).
- Symbols `other', `other-window', or boolean t: to display the buffer in a
  window other than the selected one.
- Symbol `same': display buffer in the selected window.")


(defvar >>=|toolbox/default-bottom-height 0.4
  "Default value for height when toolbox panel is displayed at `bottom'.
See `>>=|toolbox/display-buffer-action' configuration variable.")


;; tabs

(defvar >>=|xtabs/kind 'toolbox
  "Configuration kind for `xtabs' (intuitive use of `tab-line').
At Emacs initialization, the `>>=xtabs/configure' function is used to
configure `xtabs'.  You can use this same function at any time to reconfigure
this concept.  If nil, `xtabs' will be disabled, if a symbol, it will be a
buffer group, see `>>=|xtabs/buffer-groups' variable for more information.")


(defvar >>=|xtabs/buffer-groups
  '((toolbox . toolbox-p)
    (text . (derived-mode . text-mode))
    (prog . (derived-mode . prog-mode)))
  "Rules for grouping buffers into hierarchical levels.
The value of this variable must be an association list where each element is a
rule of the form (GROUP . CONDITION).  See `buffer-match-p' on how to define a
CONDITION.  See also variables `>>=|xtabs/suitable-buffer-condition' and
`>>=|xtabs/global-group'.")


(defvar >>=|xtabs/suitable-buffer-condition 'xtabs/suitable-buffer-p
  "Condition matching if a buffer is valid for `xtabs'.
The value must be a valid CONDITION argument for `buffer-match-p'.  Buffers
that do not match this condition will always be excluded before trying any
group defined in the `>>=|xtabs/buffer-groups' variable.")


(defvar >>=|xtabs/global-group 'global
  "Special group that matches all standard buffers.
See variable `>>=|xtabs/buffer-groups' for more information.")



;;; Basic configuration

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
      (>>=alist-do (fn keys >>-window-coach-mode-keys map)
        (dolist (key keys)
          (keymap-set map key fn))))
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

(defun >>=buffer-major-mode (&optional buffer)
  "Return the `major-mode' of BUFFER."
  (buffer-local-value 'major-mode (or buffer (current-buffer))))


(defalias 'standard-buffer-p '>>=standard-buffer-p)
(defun >>=standard-buffer-p (&optional buffer)
  "Match if a BUFFER is standard.
A buffer is non-standard if it is temporary, or if its name begins with a
space, or if its `major-mode' is `fundamental-mode'.  The mechanism for
setting a buffer up when it is initially opened uses this condition."
  (not
    (or
      (minibufferp buffer)
      (string-match-p "\\` " (buffer-name buffer))
      (eq (>>=buffer-major-mode buffer) 'fundamental-mode))))


(defalias 'buffer-in-mode-p '>>=buffer-in-mode-p)
(defun >>=buffer-in-mode-p (buffer &rest check-modes)
  "Match if `major-mode' of BUFFER is one of the CHECK-MODES."
  (>>=mode-find
    (>>=buffer-major-mode buffer)
    (>>=fix-rest-list check-modes)))


(defalias 'buffer-in-parent-mode-p '>>=buffer-in-parent-mode-p)
(defun >>=buffer-in-parent-mode-p (buffer &rest check-modes)
  "Match if `major-mode' of BUFFER is derived from one of the CHECK-MODES."
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
  "Create a string key from a BUFFER by joining `major-mode' and name."
  (format "%s/%s" (>>=buffer-major-mode buffer) (buffer-name buffer)))


(defun >>-buffer< (one two)
  "Non-nil if buffer ONE is less than buffer TWO in lexicographic order.
This function uses `>>-buffer-key' and `string<' to compare.  It is used to
`sort' list of buffers in `xtabs'."
  (string< (>>-buffer-key one) (>>-buffer-key two)))


(defun >>-setup-current-buffer ()
  "Run `buffer-setup-hook' on `current-buffer'."
  (when (>>=standard-buffer-p (current-buffer))
    (run-hooks 'buffer-setup-hook)))


(defun >>-window/find-first (predicate &optional all-frames)
  "Find the first item satisfying PREDICATE in current window list.
See optional argument ALL-FRAMES meaning on `window-list-1' function."
  (let ((windows (window-list-1 nil 'nomini all-frames))
        res)
    (while (and (not res) windows)
      (let ((win (car windows)))
        (if (funcall predicate win)
          (setq res win)
          ;; else
          (setq windows (cdr windows)))))
    (when (window-live-p res)
      res)))


(defun >>=safe-kill-buffer-and-window (&optional buffer)
  "Kill BUFFER and delete and delete the window currently displaying it.
If BUFFER is not given, `current-buffer' is used instead."
  (ignore-errors
    (with-current-buffer (or buffer (current-buffer))
      (kill-buffer-and-window))))


(defalias '>>=frame/pixel-width 'frame-pixel-width)
(defalias '>>=frame/pixel-height 'frame-pixel-height)
(defalias '>>=frame/outer-width 'frame-outer-width)
(defalias '>>=frame/outer-height 'frame-outer-height)
(defalias '>>=frame/inner-width 'frame-inner-width)


(defun >>=frame/inner-height (&optional frame)
  "Return inner height of FRAME in pixels.
FRAME defaults to the selected frame.  There is a difference between this
function and standard `frame-inner-height', in this case The height of
`minibuffer-window' window is also subtracted."
  (-
    (frame-inner-height frame)
    (window-old-pixel-height (minibuffer-window))))


(defalias '>>=window/body-width 'window-body-width)
(defalias '>>=window/pixel-width 'window-pixel-width)
(defalias '>>=window/total-width 'window-old-pixel-width)
(defalias '>>=window/body-height 'window-body-height)
(defalias '>>=window/pixel-height 'window-pixel-height)
(defalias '>>=window/total-height 'window-old-pixel-height)


(defun >>=window/width-percentage (&optional window frame)
  "Percentage of WINDOW width with respect to the FRAME."
  (/
    (* 100.0 (>>=window/total-width window))
    (>>=frame/outer-width frame)))


(defun >>=window/height-percentage (&optional window frame)
  "Percentage of WINDOW height with respect to the FRAME."
  (/
    (* 100.0 (>>=window/total-height window))
    (>>=frame/outer-height frame)))


(defun >>=window/inner-width-percentage (&optional window frame)
  "Percentage of WINDOW width with respect to the FRAME inner width."
  (/
    (* 100.0 (>>=window/total-width window))
    (>>=frame/inner-width frame)))


(defun >>=window/inner-height-percentage (&optional window frame)
  "Percentage of WINDOW height with respect to the FRAME inner height."
  (/
    (* 100.0 (>>=window/total-height window))
    (>>=frame/inner-height frame)))



;;; Toolbox

(defvar >>-toolbox/properties nil
  "Local property list to store options of toolbox panel buffers.
This variable is local and not-nil in toolbox buffers, it must be nil in all
other buffers.  This is set up by `>>=toolbox/setup-buffer'.")


(defalias 'toolbox-p '>>=toolbox-p)
(defun >>=toolbox-p (&optional buffer-or-name)
  "Return non-nil when BUFFER-OR-NAME is a toolbox buffer."
  (if buffer-or-name
    (buffer-local-value '>>-toolbox/properties (get-buffer buffer-or-name))
    ;; else
    >>-toolbox/properties))


(defalias 'toolbox-window-p '>>=toolbox-window-p)
(defun >>=toolbox-window-p (&optional window)
  "Return non-nil when WINDOW is showing a toolbox buffer."
  (>>=toolbox-p (window-buffer window)))


(defalias 'from-toolbox-p '>>=from-toolbox-p)
(defun >>=from-toolbox-p (&optional buffer-or-name)
  "Not null if BUFFER-OR-NAME is not a toolbox buffer but the current one is."
  (and
    (not (toolbox-p buffer-or-name))
    (toolbox-p)))


(defalias 'toolbox-env-p '>>=toolbox-env-p)
(defun >>=toolbox-env-p (&optional buffer-or-name)
  "Not null if BUFFER-OR-NAME is in a toolbox environment.
A buffer is in a toolbox environment if either the one given as an argument or
the current one is a toolbox buffer."
  (or
    (and buffer-or-name (toolbox-p buffer-or-name))
    (toolbox-p)))


(defun >>=toolbox/property (key &optional buffer)
  "Return the value of a property KEY for a toolbox BUFFER."
  (when-let ((props (>>=toolbox-p buffer)))
    (plist-get props key)))


(defun >>=toolbox-buffers ()
  "Return a list of all live toolbox buffers."
  (sort (match-buffers 'toolbox-p) '>>-buffer<))


(defun >>=toolbox/setup-buffer (buffer)
  "Configure BUFFER to be part of the toolbox panel."
  (unless (>>=toolbox-p buffer)
    (with-current-buffer buffer
      (set
        (make-local-variable '>>-toolbox/properties)
        `(:toolbox ,major-mode)))
    buffer))


(defun >>=toolbox/set-properties (buffer &rest properties)
  "Update PROPERTIES in a toolbox BUFFER."
  (let ((target (>>=toolbox-p buffer)))
    (if target
      (>>=plist-update target properties)
      ;; else
      (user-error ">>= '%s' is not a toolbox buffer" (buffer-name buffer)))))


(defun >>-toolbox/normalize-action (action)
  "Normalize a raw form for the ACTION value."
  (cond
    ((or (null action) (eq action 'bottom))
      >>=|toolbox/default-bottom-height)
    ((numberp action)
      action)
    ((memq action '(t other other-window))
      t)
    ((eq action 'same)
      '(display-buffer-same-window))
    (t
      (user-error ">>= invalid display buffer action: %s" action))))


(defsubst >>-toolbox/normalize-configured-action ()
  "Normalize the configured action value.
See `>>=|toolbox/display-buffer-action' variable for more information."
  (>>-toolbox/normalize-action >>=|toolbox/display-buffer-action))


(defun >>-toolbox/reverse-height (height)
  "Reverse toolbox HEIGHT relative to `window-text-height'."
  (if (floatp height)
    (- 1.0 height (/ (float (line-pixel-height)) (window-pixel-height)))
    ;; else
    ;; TODO: this is only valid for one visible window
    (- (window-text-height) height)))


(defun >>-toolbox/cast-height (direction height)
  "Convert HEIGHT to an action for a given DIRECTION."
  (cons
    'display-buffer-in-direction
    `((direction . ,direction) (window-height . ,height))))


(defun >>-toolbox/get-action (buffer)
  "Get action to use `display-buffer' action functions to switch to BUFFER."
  (let ((action (>>-toolbox/normalize-configured-action)))
    (if (numberp action)
      (if (>>=toolbox-p buffer)
        (>>-toolbox/cast-height 'bottom action)
        ;; else
        (let ((count (count-windows 'nomini)))
          (if (> count 1)
            '((display-buffer-reuse-mode-window
               display-buffer-in-previous-window))
            ;; else
            (>>-toolbox/cast-height 'top
              (>>-toolbox/reverse-height action)))))
      ;; else
      action)))


(defun >>-get-bottom-end-window ()
  "Internal function to get the window at the bottom end."
  (let (res)
    (when-let ((aux (window-in-direction 'below)))
      (while aux
        (setq
          res aux
          aux (window-in-direction 'below aux))))
    res))


(defun display-buffer-reuse-toolbox-window (buffer alist)
  "Return a window displaying a toolbox buffer if the given BUFFER is one.

ALIST is an association list of action symbols and values.  When it has a
non-nil `inhibit-same-window' entry, the selected window is not eligible.  If
it contains a `reusable-frames' entry, its value has the same semantics as the
ALL-FRAMES argument of the `get-buffer-window' function.  All other values are
ignored.

This is an ACTION function, so we don't use the `xorns' naming convention."
  (when (>>=toolbox-p buffer)
    (let ((frames (cdr (assq 'reusable-frames alist)))
          (not-same (cdr (assq 'inhibit-same-window alist)))
          window)
      (setq window
        (>>-window/find-first
          (lambda (win)
            (and
              (>>=toolbox-window-p win)
              (not (and not-same (eq win (selected-window))))))
          frames))
      (when window
        (prog1
          (window--display-buffer buffer window 'reuse alist)
          (unless (cdr (assq 'inhibit-switch-frame alist))
            (window--maybe-raise-frame (window-frame window))))))))


(defun >>=toolbox/switch-to-buffer (buffer-or-name)
  "Select BUFFER-OR-NAME in the toolbox panel.
The optional argument MODE will take precedence over the variable
`>>=|toolbox/display-buffer-action'."
  (let ((org-fba display-buffer-fallback-action)
        (display-buffer-fallback-action nil)
        (buffer (get-buffer buffer-or-name)))
    (select-window
      (or
        (display-buffer buffer
          '((display-buffer-reuse-window
             display-buffer-reuse-toolbox-window)))
        (display-buffer buffer (>>-toolbox/get-action buffer))
        (display-buffer buffer org-fba)))))


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


(defun >>=xtabs/toggle-switch-cycling ()
  "Toggle cycling tab switch."
  (interactive)
  (setq tab-line-switch-cycling (not tab-line-switch-cycling)))


(defalias 'xtabs/suitable-buffer-p '>>-xtabs/suitable-buffer-p)
(defun >>-xtabs/suitable-buffer-p (&optional buffer)
  "Return whether BUFFER can be included in the tab line."
  (unless buffer
    (setq buffer (current-buffer)))
  (and
    (>>=standard-buffer-p buffer)
    (let ((mode (>>=buffer-major-mode buffer)))
      (not
        (or
          (memq mode tab-line-exclude-modes)
          (get mode 'tab-line-exclude)
          (buffer-local-value 'tab-line-exclude buffer))))))


(defsubst >>-xtabs/op-cond (operator condition)
  "Head a CONDITION with an OPERATOR."
  (when condition
    (let ((tail (cdr condition)))
      (if (null tail)
        (car condition)
        ;; else
        (cons operator condition)))))


(defun >>-xtabs/kind-condition (kind)
  "Look a condition up in `>>=|xtabs/buffer-groups' based on a KIND.
Return a `cons' (FOUND-CONDITION . UPPER-CONDITIONS)."
  (when kind
    (if (memq kind `(t ,>>=|xtabs/global-group))
      '(t)
      ;; else
      (let ((groups >>=|xtabs/buffer-groups)
            upper
            res)
        (while (and groups (not res))
          (setq upper (cons (cdar groups) upper))
          (if (eq (caar groups) kind)
            (setq res upper)
            ;; else
            (setq groups (cdr groups))))
        (if res
          res
          ;; else
          (switch-to-buffer "*Messages*")
          (user-error ">>= invalid `xtabs' kind: %s" kind))))))


(defun >>-xtabs/buffer-condition (buffer)
  "Look a condition up in `>>=|xtabs/buffer-groups' based on a BUFFER.
Return a `cons' (FOUND-CONDITION . UPPER-CONDITIONS)."
  (let ((groups >>=|xtabs/buffer-groups)
        upper
        res)
    (while (and groups (not res))
      (let ((current (cdar groups)))
        (setq upper (cons current upper))
        (if (buffer-match-p current buffer)
          (setq res upper)
          ;; else
          (setq groups (cdr groups)))))
    res))


(defun >>-xtabs/get-condition (key)
  "Calculate a condition based on the given KEY.
KEY could be a valid `>>=|xtabs/kind', a buffer, or a `buffer-name'."
  (if (or (stringp key) (bufferp key))
    (let* ((pair (>>-xtabs/buffer-condition key))
           (head (car pair))
           (tail (>>-xtabs/op-cond 'or (cdr pair))))
      (if head
        (if tail
          `(and ,head (not ,tail) ,>>=|xtabs/suitable-buffer-condition)
          ;; else
          `(and ,head ,>>=|xtabs/suitable-buffer-condition))
        ;; else
        t))
    ;; else
    (when-let ((pair (>>-xtabs/kind-condition key)))
      (if (eq (car pair) t)
        >>=|xtabs/suitable-buffer-condition
        ;; else
        (setq pair (>>-xtabs/op-cond 'or pair))
        `(and ,pair ,>>=|xtabs/suitable-buffer-condition)))))


(defsubst >>-xtabs/current-condition ()
  "Calculate the condition to match a buffer based on `>>=|xtabs/kind'."
  (>>-xtabs/get-condition >>=|xtabs/kind))


(defun >>-xtabs/enable-on-buffer ()
  "Turn on `tab-line-mode' in a `current-buffer' if pertinent.
Advice to :override `tab-line-mode--turn-on'."
  (when (buffer-match-p (>>-xtabs/current-condition) (current-buffer))
    (tab-line-mode +1)))


(defun >>=xtabs/buffers ()
  "List of tabs to display in the `tab-line'.
To be used with `tab-line-tabs-function' variable."
  (when-let ((condition (>>-xtabs/get-condition (current-buffer))))
    (if (eq condition t)
      (funcall (>>=get-original-value tab-line-tabs-function))
      ;; else
      (sort (match-buffers condition) '>>-buffer<))))


(defun >>-xtabs/enable ()
  "Enable the mechanisms of `xtabs'."
  (unless (eq tab-line-tabs-function '>>=xtabs/buffers)
    (setq tab-line-tabs-function '>>=xtabs/buffers))
  (unless (advice-member-p '>>-xtabs/enable-on-buffer 'tab-line-mode--turn-on)
    (advice-add 'tab-line-mode--turn-on :override '>>-xtabs/enable-on-buffer))
  (global-tab-line-mode +1))


(defun >>-xtabs/disable ()
  "Deactivate the mechanisms of `xtabs'."
  (global-tab-line-mode -1)
  (when (advice-member-p '>>-xtabs/enable-on-buffer 'tab-line-mode--turn-on)
    (advice-remove 'tab-line-mode--turn-on '>>-xtabs/enable-on-buffer))
  (when (eq tab-line-tabs-function '>>=xtabs/buffers)
    (>>=restore-original-value tab-line-tabs-function)))


(defun >>-xtabs/set-kind (kind)
  "Activate/deactivate `xtabs' according to KIND."
  (if (null kind)
    (>>-xtabs/disable)
    ;; else
    (>>-xtabs/enable)))


(defalias 'xtabs '>>=xtabs/configure)
(defun >>=xtabs/configure (kind)
  "Configure tabs using KIND.
In addition to the types defined in the `>>=|xtabs/kind' variable, the
special type `init' can be used for initial configuration."
  (if (eq kind 'init)
    (when >>=|xtabs/kind
      (setq kind >>=|xtabs/kind)
      (>>-xtabs/set-kind kind))
    ;; else
    (setq >>=|xtabs/kind kind)
    (>>-xtabs/set-kind kind)))



;;; Module configuration (hooks, navigation, ...)

(defun >>=switch-to-next-buffer ()
  "Switch to the next tab/buffer."
  (interactive)
  (or
    (and
      tab-line-mode
      (tab-line-switch-to-next-tab))
    (>>=call? '>>=exwm/send-last-key)
    (next-buffer)))


(defun >>=switch-to-previous-buffer ()
  "Switch to the previous tab/buffer."
  (interactive)
  (or
    (and
      tab-line-mode
      (tab-line-switch-to-prev-tab))
    (>>=call? '>>=exwm/send-last-key)
    (previous-buffer)))


(>>=bind-global-keys
  "C-<next>" >>=switch-to-next-buffer
  "C-<prior>" >>=switch-to-previous-buffer)


(defun >>-check-buffer ()
  "Setup `current-buffer' the first time its mode is set."
  (let ((buffer (current-buffer)))
    (when (buffer-match-p >>=|toolbox/buffer-match-condition buffer)
      (>>=toolbox/setup-buffer buffer)))
  (>>-xtabs/enable-on-buffer))


(defun >>-configure-window-module ()
  "Initial window module configuration."
  (>>=xtabs/configure 'init))


(add-hook 'after-change-major-mode-hook '>>-setup-current-buffer)
(add-hook 'buffer-setup-hook '>>-check-buffer)
(add-hook 'after-init-hook '>>-configure-window-module)


(provide 'xorns-window)
;;; xorns-window.el ends here
