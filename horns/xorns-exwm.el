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
;; applications are defined in the variable `>>=|exwm/startup-applications'.

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

;; TODO: configure `exwm-randr' to use multiple monitors

;; Enjoy!


;;; Code:

(eval-and-compile
  (require 'use-package)
  (require 'xorns-tools)
  (require 'browse-url))



;;; Main definitions

(defvar >>=|exwm/browse-url-workspace 2
  "If not nil, workspace number to browse URLs inner EXWM.")


(defvar >>=|exwm/url-keys
  `(("<s-f2>" . "http://")    ;; Empty browser
    ("C-s-t" . "https://translate.google.com")
    ("C-s-c" . "https://web.telegram.org"))
  "Pairs of (KEY . URL) to be used with `browse-url'.")


(defvar >>=|exwm/startup-applications nil
  "List of applications to be executed when EXWM starts.")


(defvar >>=|exwm/start-process-model nil
  "Model to execute EXWM processes.
Value could be a function receiving an unique argument string; or nil to use
`>>=exwm/start-process', or t to use `>>=exwm/start-subprocess'.")


(defvar >>=|exwm/web-names
  '("browser" "brave-browser" "firefox" "chromium" "microsoft-edge")
  "Class names for web browser applications.
Used together with `>>=|exwm/web-alts' to complement `>>=|exwm/name-alist'.")


(defvar >>=|exwm/web-alts '("web-main" "web-misc" "web")
  "Alternative names for web browser applications.
Used together with `>>=|exwm/web-alts' to complement `>>=|exwm/name-alist'.")


(defvar >>=|exwm/name-alist '(("VirtualBox Machine" . "vm"))
  "Association list of buffer names replacements.
The key of each cons cell (`car') will be the windows class-name, and the
associated value (`cdr') the proposed alternative names.  Values could be a
string or a list of strings.")


(defvar >>-exwm/name-alist nil
  "Private variable with the complete list of buffer name replacements.
This value is obtained by appending `>>=|exwm/name-alist' with calculated
pairs from `>>=|exwm/web-names' and `>>=|exwm/web-alts'.")


(defvar >>-exwm/system-tray-chars-width nil
  "Internal variable storing system-tray width in chars units.
Can be estimated before loading startup applications.  Its value is a `cons'
\(WIDTH . ESTIMATED).")


(defvar exwm-systemtray-update-hook nil
  "Run when system-tray parameters needs update.")


(defun >>=exwm/start-process (command)
  "Call COMMAND synchronously in a separate process returning immediately."
  (unless (stringp command)
    (setq command (mapconcat 'identity command " ")))
  (call-process-shell-command command nil 0))


(defun >>=exwm/start-subprocess (command &optional name)
  "Call COMMAND synchronously in a sub-process returning immediately.
A process NAME can bee given as an optional argument."
  (unless (stringp command)
    (setq command (mapconcat 'identity command " ")))
  (unless name
    (setq name (if (string-match "[[:space:]]" command) "EXWM-SP" command)))
  (start-process-shell-command name nil command))


(defun >>=exwm/start-command (command)
  "Start a COMMAND synchronously in separate process."
  (interactive (list (read-shell-command ">>= ")))
  (cond
    ((and (stringp command) (string-match "\\`https?[:]" command))
      (browse-url command))
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


(defun >>-exwm/create-url-keys ()
  "Return pairs of (KEY . URL) used by `browse-url'."
  (mapcan
    (lambda (pair)
      (list
        (cons
          (car pair)
          `(lambda () (interactive) (browse-url ,(cdr pair))))))
    >>=|exwm/url-keys))



;;; Configuration

(use-package exwm
  :ensure t
  :demand t
  :commands exwm-enable
  :preface
  (defun >>-exwm/class-name ()
    "Get the class name (WM_CLASS) for a newly created EXWM buffer."
    (or
      (>>=str-trim exwm-class-name)
      (>>=str-trim exwm-instance-name)
      "unnamed"))

  (defun >>-exwm/name-alist ()
    "Get the class name (WM_CLASS) for a newly created EXWM buffer."
    (or
      >>-exwm/name-alist
      (setq >>-exwm/name-alist
        (append
          >>=|exwm/name-alist
          (when (and >>=|exwm/web-names >>=|exwm/web-alts)
            (mapcar
              (lambda (name) (cons name >>=|exwm/web-alts))
              >>=|exwm/web-names))))))


  (defun >>-exwm/rename-new-buffer ()
    "Rename a newly created EXWM buffer."
    (let* ((name (>>-exwm/class-name))
           (replacements (>>-exwm/name-alist))
           (rep (cdr (assoc-string name replacements 'case-fold))))
      (when rep
        (if (stringp rep)
          (setq name rep)
          ;; else
          (while rep
            (setq
              name (car rep)
              rep (if (get-buffer name) (cdr rep) nil)))))
      (rename-buffer name 'unique)))

  (defun >>-exwm/update-class ()
    "Run when window class is updated."
    (>>-exwm/rename-new-buffer))

  (defun >>-exwm/switch-to-workspace-functions ()
    "Generate (KEY . switch-function) pairs."
    (mapcar
      (lambda (idx)
        (let ((key (>>=key-parse (format "s-%d" idx)))
              (name (intern (format ">>=exwm/switch-workspace-%s" idx)))
              (doc (format "Switch to workspace '%s'." idx)))
          (cons key
            (defalias name
              (lambda ()
                (interactive)
                (exwm-workspace-switch-create (car `(,idx))))
              doc))))
      (number-sequence 0 9)))

  (defun >>-exwm/config ()
    "Xorns configuration for EXWM (replaces `exwm-config-example')."
    ;; We don't call `exwm-config-misc' to disable dialog boxes and
    ;; hourglass pointer here using because in `xorns' this is done in
    ;; `early-init.el'.  Also, `exwm-config-ido' is not used because we
    ;; configure IDO, if demanded, in `xorns-minibuffer.el'.
    (unless (>>=customized? 'exwm-workspace-number)
      (setq exwm-workspace-number 4))
    (unless (>>=customized? 'exwm-input-global-keys)
      (setq exwm-input-global-keys
        `(([?\s-&] . >>=exwm/start-command)
           ,@(>>-exwm/switch-to-workspace-functions))))
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
  :hook
  (exwm-update-class . >>-exwm/update-class)
  :custom
  (exwm-workspace-warp-cursor t)
  (exwm-workspace-display-echo-area-timeout 5)
  (exwm-floating-border-width 3)
  :config
  (message ">>= using Emacs as the Desktop Window Manager.")
  (->? >>=window-manager/init)
  (>>-exwm/config))


(use-package exwm-input
  :after exwm
  :demand t
  :commands
  exwm-reset
  exwm-input-set-key
  exwm-input-send-next-key
  exwm-workspace-switch-create
  :preface
  (defun >>-exwm/swap-last-buffers ()
    "Switch currently visible buffer by last one."
    ;; TODO: move this to `xorns-window'
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer))))
  :config
  (defun >>-exwm/input-set-key (key command)
    "Advice for `exwm-input-set-key' to give KEY a global binding as COMMAND."
    (exwm-input-set-key (>>=key-parse key) command))

  (defun >>-exwm/browse-url (url &rest args)
    "Before to `browse-url' using (URL ARGS) to change workspace."
    ;; TODO: (interactive (browse-url-interactive-arg ">>= URL: "))
    (ignore url args)
    (when >>=|exwm/browse-url-workspace
      (exwm-workspace-switch-create >>=|exwm/browse-url-workspace)))

  (advice-add '>>=bind-global-key :override '>>-exwm/input-set-key)
  (advice-add 'browse-url :before '>>-exwm/browse-url)

  (>>=bind-global-keys
    ;; Like on `i3' window manager.  We use a new command because at this
    ;; level `(key-binding (kbd "s-&"))' returns nil
    "s-d" >>=exwm/start-command
    "s-r" exwm-reset
    "<s-tab>" other-frame
    "s-o" other-window
    "s-;" >>-exwm/swap-last-buffers
    "s-{" >>=exwm/reduce-window-horizontally
    "s-}" >>=exwm/enlarge-window-horizontally
    "C-s-/" browse-url-at-point)

  (eval `(>>=bind-global-keys ,@(>>-exwm/create-url-keys)))

  (let* ((suspend-key ?\C-z))
    ;; Prefix key to send next key-press literally to the application.
    ;; Default value is `C-z' because is used for `suspend-frame' in terminals.
    (add-to-list 'exwm-input-prefix-keys suspend-key)
    (add-to-list 'exwm-input-prefix-keys
      (car (mapcar 'identity (>>=key-parse "M-X"))))
    (keymap-set exwm-mode-map
      (key-description (vector suspend-key)) 'exwm-input-send-next-key))
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
       ([?\C-D] . [delete])
       ([?\C-w] . [?\C-x])
       ([?\C-W] . [?\C-x])
       ([?\M-w] . [?\C-c])
       ([?\M-W] . [?\C-c])
       ([?\C-y] . [?\C-v])
       ([?\C-Y] . [?\C-v])
       ([?\M-d] . [C-S-right ?\C-x])
       ([?\M-D] . [C-S-right ?\C-x])
       ([?\C-k] . [S-end ?\C-x])
       ([?\C-K] . [S-end ?\C-x])
       ([M-backspace] . [C-S-left ?\C-x])
       ;; search
       ([?\C-s] . [?\C-f])
       ([?\C-\S-s] . [?\C-g])
       ;; escape
       ([?\C-g] . [escape])
       ([?\s-q] . [?\C-w])
       ))
  (require 'xorns-linux))


(use-package exwm-systemtray
  :after exwm
  :demand t
  :commands exwm-systemtray-enable exwm-systemtray--set-background-color
  :preface
  (defsubst >>-exwm/mini-modeline-background-color ()
    "Get background mode-line color if `mini-modeline' is active."
    (plist-get (bound-and-true-p mini-modeline-face-attr) :background))

  (defun >>-exwm/system-tray-width ()
    "System-tray pixels width."
    (let ((gap exwm-systemtray-icon-gap)
          (res 0.0))
      (dolist (pair exwm-systemtray--list)
        (setq res (+ res (slot-value (cdr pair) 'width) gap)))
      (if (zerop res) nil (+ res gap))))

  (defun >>-exwm/estimated-system-tray-width ()
    "Estimate system-tray pixels width before startup applications."
    ;; TODO: There may be startup apps that are not iconic.
    (let* ((gap exwm-systemtray-icon-gap)
           (size (+ (float (frame-char-height)) gap)))
      (+ (* size (length >>=|exwm/startup-applications)) gap)))

  (defun >>-exwm/startup-applications ()
    "Run all startup applications defined in `>>=|exwm/startup-applications'."
    (dolist (cmd >>=|exwm/startup-applications)
      (condition-case-unless-debug err
        (>>=exwm/start-command cmd)
        (error
          (message ">>= error executing '%s' startup application:\n    %s"
            cmd (error-message-string err))))))

  (defun >>-exwm/update-system-tray-chars-width (width &optional estimated)
    "Run hook to update system-tray WIDTH (can be ESTIMATED)."
    (setq >>-exwm/system-tray-chars-width
      (cons (round (/ width (frame-char-width))) estimated))
    (run-hooks 'exwm-systemtray-update-hook))

  (defun >>-exwm/systemtray--init ()
    "Advice for `exwm-systemtray--init' to initialize system tray module."
    (>>-exwm/startup-applications)
    (when-let ((color (>>-exwm/mini-modeline-background-color)))
      ;; this should only happen if `mini-modeline' is configured.
      (setq exwm-systemtray-background-color color)
      (exwm-systemtray--set-background-color))
    (let ((width (>>-exwm/system-tray-width)))
      (if width
        (>>-exwm/update-system-tray-chars-width width)
        ;; else
        (>>-exwm/update-system-tray-chars-width
          (>>-exwm/estimated-system-tray-width))
        (let ((count (length >>=|exwm/startup-applications)))
          (unless (zerop count)
            (run-with-timer (1+ count) nil
              (lambda ()
                (when-let ((width (>>-exwm/system-tray-width)))
                  (>>-exwm/update-system-tray-chars-width width)))))))))
  :config
  (setq-default display-time-24hr-format t)
  (display-battery-mode +1)
  (display-time-mode +1)
  (advice-add 'exwm-systemtray--init :after '>>-exwm/systemtray--init)
  (exwm-systemtray-enable))


(use-package exwm-workspace
  :after exwm
  :demand t
  :functions exwm-workspace-switch >>=exwm/switch-workspace-0
  :preface
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
  (>>=bind-global-keys
    "s-." >>=exwm/switch-workspace-0    ; TODO: change this
    "s-`" >>=exwm/switch-workspace-0
    "s-w" exwm-workspace-switch
    "<C-s-left>" >>-exwm/ws-switch-left
    "<C-s-right>" >>-exwm/ws-switch-right)
  (let ((map (make-sparse-keymap)))
    (keymap-set map "<mode-line> <mouse-1>" 'exwm-workspace-switch)
    (setq global-mode-string
      (nconc
        '(" ")
        global-mode-string
        `(""
           (:propertize
             (:eval (format " <%d>" exwm-workspace-current-index))
             local-map ,map
             face bold
             mouse-face mode-line-highlight
             help-echo "EXWM workspace.\nclick: switch/add/delete."))))))


(provide 'xorns-exwm)
;;; xorns-exwm.el ends here
