;;; xorns-exwm.el --- EXWM is a window manager based on Emacs  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Emacs can be configured as a Desktop Environment using this module, our
;; configuration around EXWM package (Emacs X Window Manager).

;; Session Files
;; -------------

;; To start Emacs as a Windows manager make sure you create symbolic-links for
;; `exwm/*.desktop' files into `/usr/share/xsessions':
;;
;;   sudo ln -f ~/.emacs.d/exwm/*.desktop /usr/share/xsessions/

;; If you are using a Version Control System to maintain your `~/.emacs.d'
;; directory, `/usr/share/xsessions/' files may become outdated.

;; NOTE that `user-emacs-directory' could use another location, for example if
;; the XDG convention is used in Emacs 27 `$XDG_CONFIG_HOME/emacs/'.

;; If you need to use `startx', add the follwing entry to your `get_session'
;; function in the `~/.xinitrc' file (remove the escape char starting the
;; Emacs name):
;;
;;     \emacs) dbus_args+=(\emacs) ;;

;; Startup Applications
;; --------------------

;; Every Desktop Environment defines a set of applications that will be
;; automatically launched during startup after the user has logged in.  These
;; applications are defined in the variable `>>=|exwm/startup-applications',
;; and managed by function `>>=exwm/run-startup-applications'.

;; Each command definition must be a string, e.g. "nm-applet"; a list could be
;; used for backward compatibility, e.g. '("GDK_BACKEND=x11" "pamac-tray").

;; For this feature, Linux commonly uses XDG standard: `.desktop' files are
;; defined in the autostart directories, `~/.config/autostart/' and
;; `/etc/xdg/autostart/'.  We do not use these definitions because there may
;; be applications that are not required or could be incompatible with EXWM.

;; There are two models to execute a command configured in the
;; `>>=|exwm/start-process-model' variable.: as a separate process
;; `>>=exwm/start-process', or as a sub-process `>>=exwm/start-subprocess'.
;; The selected mode is used for both, the startup applications and any
;; launched with `>>=exwm/start-command'.

;; Enjoy!


;;; Code:

(eval-and-compile
  (require 'use-package nil 'noerror))

(require 'xorns-tools)
(require 'xorns-bindings)


;;; Main definitions

(defvar >>=|exwm/url-keys
  `(("<s-f2>" . "http://")    ;; Empty browser
    ("C-s-f" . "https://facebook.com")
    ("C-s-t" . "https://translate.google.com")
    ("C-s-c" . "https://web.telegram.org"))
  "Pairs of (KEY . URL) to be used with `browse-url'  inner EXWM.")


(defvar >>=|exwm/startup-applications nil
  "List of applications to be executed when EXWM starts.")


(defvar >>=|mode-line/battery-icon-command nil
  "Command to show battery as a try icon.
This command will be used instead of the function `display-battery-mode'.")


(defvar >>=|exwm/start-process-model nil
  "Model to execute EXWM processes.
Value could be a function receiving an unique argument string; or nil to use
`>>=exwm/start-process', or t to use `>>=exwm/start-subprocess'.")


(defvar >>=|exwm/systemtray-icons t
  "Count of system-tray icons (useful to set `mini-modeline-right-padding').
Could be an integer or a boolean value, if t is calculated with the length of
`>>=|exwm/startup-applications'.)")


(defun >>=exwm/start-process (command)
  "Call COMMAND synchronously in a separate process returning immediately."
  (unless (stringp command)
    (setq command (mapconcat 'identity command  " ")))
  (call-process-shell-command command nil 0))


(defun >>=exwm/start-subprocess (command &optional name)
  "Call COMMAND synchronously in a sub-process returning immediately.
A process NAME can bee given as an optional argument."
  (unless (stringp command)
    (setq command (mapconcat 'identity command  " ")))
  (unless name
    (setq name (if (string-match "[[:space:]]" command) "EXWM-SP" command)))
  (start-process-shell-command name nil command))


(defun >>=exwm/start-command (command)
  "Start a COMMAND synchronously in separate process."
  (interactive (list (read-shell-command ">>= ")))
  (cond
    ((functionp >>=|exwm/start-process-model)
      (funcall >>=|exwm/start-process-model command))
    ((null >>=|exwm/start-process-model)
      (>>=exwm/start-process command))
    (t
      (>>=exwm/start-subprocess command))))


(defun >>=exwm/enlarge-window-horizontally (&optional delta)
  "Make the selected window DELTA*50 pixels wider."
  (interactive "p")
  (declare-function exwm-layout-enlarge-window-horizontally 'exwm-layout)
  (exwm-layout-enlarge-window-horizontally (* +50 delta)))


(defun >>=exwm/reduce-window-horizontally (&optional delta)
  "Make the selected window DELTA*50 pixels narrower."
  (interactive "p")
  (declare-function exwm-layout-enlarge-window-horizontally 'exwm-layout)
  (exwm-layout-enlarge-window-horizontally (* -50 delta)))


(defun >>-url-browser (url)
  "Create a web browser for a given URL."
  (lambda ()
    (interactive)
    (browse-url url)))


(defun >>=exwm/run-startup-applications ()
  "Run all startup applications defined in `>>=|exwm/startup-applications'."
  (dolist (cmd >>=|exwm/startup-applications)
    (condition-case-unless-debug err
      (>>=exwm/start-command cmd)
      (error
        (message ">>= error executing '%s' startup application:\n    %s"
          cmd (error-message-string err))))))


(defun >>=exwm/configure-system-try ()
  "Run all startup applications defined in `>>=|exwm/startup-applications'."
  (require 'exwm-systemtray)
  (if >>=|mode-line/battery-icon-command
    (>>=exwm/start-command >>=|mode-line/battery-icon-command)
    ;; else
    (display-battery-mode +1))
  (setq-default display-time-24hr-format t)
  (display-time-mode +1)
  (exwm-systemtray-enable))



;;; Configuration

(use-package exwm
  :ensure t
  :demand t
  :preface
  (declare-function exwm-enable 'exwm)
  (declare-function exwm-workspace-rename-buffer 'exwm-workspace)
  :init
  ;; (require 'exwm-config)
  (defun >>-exwm/config ()
    "Xorns configuration of EXWM (replaces `exwm-config-example')."
    ;; We don't call `exwm-config-misc' to disable dialog boxes and
    ;; hourglass pointer here using because in `xorns' this is done in
    ;; `early-init.el' or `/xorns-core.el' if Emacs version < 27.  Also,
    ;; `exwm-config-ido' is not used because we configure IDO, if demanded,
    ;; in `xorns-minibuffer.el'.
    (unless (get 'exwm-workspace-number 'saved-value)
      (setq exwm-workspace-number 4))
    (add-hook 'exwm-update-class-hook
      ;; TODO: maybe move this to `exwm-workspace'
      (lambda ()    ; make class name the buffer name
        (exwm-workspace-rename-buffer exwm-class-name)))
    (unless (get 'exwm-input-global-keys 'saved-value)
      (setq exwm-input-global-keys
        `(
           ([?\s-&] . >>=exwm/start-command)
           ,@(mapcar    ; 's-N' . switch to workspace
               (lambda (i)
                 `(,(kbd (format "s-%d" i)) .
                    (lambda ()
                      (interactive)
                      (exwm-workspace-switch-create ,i))))
               (number-sequence 0 9)))
        ))
    (unless (get 'exwm-input-simulation-keys 'saved-value)
      (setq exwm-input-simulation-keys
        '(([?\C-b] . [left])
           ([?\C-f] . [right])
           ([?\C-p] . [up])
           ([?\C-n] . [down])
           ([?\C-a] . [home])
           ([?\C-e] . [end])
           ([?\M-v] . [prior])
           ([?\C-v] . [next])
           ([?\C-d] . [delete])
           ([?\C-k] . [S-end delete]))))
    (exwm-enable))
  :config
  (eval-when-compile    ; this is only needed in local compile
    (declare-function >>-exwm/config 'xorns-exwm)    ; WTF
    (declare-function exwm-systemtray-enable 'exwm-systemtray))
  (message ">>= using Emacs as the Desktop Window Manager.")
  (->? >>=window-manager/init)
  (>>=exwm/run-startup-applications)
  (>>=exwm/configure-system-try)
  (>>-exwm/config)    ;; (exwm-config-example)
  )


(use-package exwm-input
  :after exwm
  :demand t
  :commands exwm-reset
  :preface
  (declare-function exwm-input-send-next-key 'exwm-input)

  (defun >>-exwm/swap-last-buffers ()
    "Switch currently visible buffer by last one."
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer))))
  :config
  (setq >>-global-set-key 'exwm-input-set-key)
  (>>=global-set-keys
    ;; Like on `i3' window manager.  We use a new command because at this
    ;; level `(key-binding (kbd "s-&"))' returns nil
    "s-d" '>>=exwm/start-command
    "s-r" 'exwm-reset
    "<s-tab>" 'other-frame
    "s-o" 'other-window
    "s-;" '>>-exwm/swap-last-buffers
    "s-{" '>>=exwm/reduce-window-horizontally
    "s-}" '>>=exwm/enlarge-window-horizontally
    "C-s-/" 'browse-url-at-point)
  (apply '>>=global-set-keys
    (mapcan
      (lambda (pair) (list (car pair) (>>-url-browser (cdr pair))))
      >>=|exwm/url-keys))
  (let ((suspend-key ?\C-z))
    ;; Prefix key to send next literally to the application.  Default value
    ;; is `C-z' because is used for `suspend-frame' in terminals.
    (add-to-list 'exwm-input-prefix-keys suspend-key)
    (define-key exwm-mode-map (vector suspend-key)
      #'exwm-input-send-next-key))
  (setq exwm-input-simulation-keys
    `(
       ;; general, movement
       ([?\C-b] . [left])
       ([?\M-b] . [C-left])
       ([?\C-f] . [right])
       ([?\M-f] . [C-right])
       ([?\C-\S-b] . [S-left])
       ([?\M-\S-b] . [C-S-left])
       ([?\C-\S-f] . [S-right])
       ([?\M-\S-f] . [C-S-right])
       ([?\C-p] . [up])
       ([?\C-n] . [down])
       ([?\C-a] . [home])
       ([?\C-e] . [end])
       ([?\M-v] . [prior])
       ([?\C-v] . [next])
       ([?\C-m] . [return])
       ([?\C-i] . [tab])
       ;; cut/paste, selection
       ([?\C-d] . [delete])
       ([?\C-w] . [?\C-x])
       ([?\M-w] . [?\C-c])
       ([?\C-y] . [?\C-v])
       ([?\M-d] . [C-S-right ?\C-x])
       ([?\C-k] . [S-end ?\C-x])
       ([M-backspace] . [C-S-left ?\C-x])
       ;; search
       ([?\C-s] . [?\C-f])
       ([?\C-\S-s] . [?\C-g])
       ;; escape
       ([?\C-g] . [escape])
       ([?\s-q] . [?\C-w])
       ))
  (require 'xorns-linux))


(use-package buffer-move
  :ensure t
  :after exwm-input
  :demand t
  :config
  (>>=global-set-keys
    "<C-S-up>" 'buf-move-up
    "<C-S-down>" 'buf-move-down
    "<C-S-left>" 'buf-move-left
    "<C-S-right>" 'buf-move-right))


(use-package exwm-workspace
  :after exwm
  :demand t
  :preface
  (declare-function exwm-workspace-switch 'exwm-workspace)

  (defun >>-exwm/switch-workspace-0 ()
    "Move to first workspace."
    (interactive)
    (exwm-workspace-switch 0))

  (defun >>-exwm/ws-switch-left ()
    "Move to left workspace. "
    (interactive)
    (let ((current (exwm-workspace--position exwm-workspace--current)))
      (exwm-workspace-switch
        (1- (if (> current 0) current (exwm-workspace--count))))))

  (defun >>-exwm/ws-switch-right ()
    "Move to left workspace. "
    (interactive)
    (let ((current (exwm-workspace--position exwm-workspace--current))
          (maxws (1- (exwm-workspace--count))))
      (exwm-workspace-switch
        (if (< current maxws) (1+ current) 0))))

  :custom
  (exwm-workspace-show-all-buffers t)
  (exwm-layout-show-all-buffers t)
  :config
  (>>=global-set-keys
    "s-." '>>-exwm/switch-workspace-0    ; TODO: change this
    "s-`" '>>-exwm/switch-workspace-0
    "s-w" 'exwm-workspace-switch
    "<C-s-left>" '>>-exwm/ws-switch-left
    "<C-s-right>" '>>-exwm/ws-switch-right)
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'exwm-workspace-switch)
    (setq global-mode-string
      (nconc global-mode-string
        `(""
           (:propertize
             (:eval (format " <%d>" exwm-workspace-current-index))
             local-map ,map
             face bold
             mouse-face mode-line-highlight
             help-echo "EXWM workspace.\nclick: switch/add/delete.")))))
  )


(provide 'xorns-exwm)
;;; xorns-exwm.el ends here
