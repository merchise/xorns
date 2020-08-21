;;; xorns-exwm.el --- EXWM is a window manager based on Emacs  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; https://wiki.archlinux.org/index.php/EXWM

;; https://github.com/ch11ng/exwm/wiki

;; Enjoy!


;;; Code:

(eval-when-compile
  (require 'cl))
(require 'use-package)


;;; Main definitions

(defvar >>=|exwm/url-keys
  `(("<s-f2>" . "http://")    ;; Empty browser
    ("C-s-f" . "https://facebook.com")
    ("C-s-t" . "https://translate.google.com")
    ("C-s-c" . "https://web.telegram.org"))
  "Pairs (KEY . URL) to be used with `browse-url' inner `exwm'.")


(defvar >>=|exwm/startup-applications nil
  ;; For example: '("nm-applet" "pamac-tray")
  "Startup applications to be executed with `start-process-shell-command'.")


(defvar >>=|exwm/systemtray-icons t
  "Count of system-tray icons (useful to set `mini-modeline-right-padding').
Could be an integer or a boolean value, if t is calculated with the length of
`>>=|exwm/startup-applications'.)"  )


;; Executing `(key-binding (kbd "s-&"))' returns nil
(defun >>=exwm/start-command (command)
  "Start a COMMAND in a sub-process."
  (interactive (list (read-shell-command ">>= ")))
  (start-process-shell-command command nil command))


(defun >>-url-browser (url)
  "Create a web browser for a given URL."
  (lexical-let ((url url))
    (lambda ()
      (interactive)
      (browse-url url))))



;;; Configuration

(use-package exwm
  :ensure t
  :demand t
  :config
  (progn
    (eval-when-compile
      ;; TODO: deprecate this, only needed in local compile
      (declare-function exwm-systemtray-enable 'exwm-systemtray)
      (declare-function exwm-config-example 'exwm-config)
      )
    (message ">>= using Emacs as the Desktop Window Manager.")
    (dolist (cmd >>=|exwm/startup-applications)
      ;; Run all startup applications
      (let ((name (car (split-string cmd))))
	(condition-case-unless-debug err
	  (start-process-shell-command name nil cmd)
	  (error
	    (message ">>= error executing '%s' startup application:\n    %s"
	      cmd (error-message-string err))))))
    (->? >>=window-manager/init)
    (require 'exwm-config)
    (require 'exwm-systemtray)
    (exwm-systemtray-enable)
    (exwm-config-example)    ; TODO: review how to define a customized config
    ))


(use-package exwm-input
  :after exwm
  :demand t
  :commands exwm-reset
  :preface
  (progn
    (defun >>-exwm/swap-last-buffers ()
      "Switch currently visible buffer by last one."
      (interactive)
      (switch-to-buffer (other-buffer (current-buffer))))
    )
  :config
  (progn
    (require 'xorns-term)

    (exwm-input-set-key
      ;; Like in `i3' windows manager
      (kbd "s-d") #'>>=exwm/start-command)
    (exwm-input-set-key (kbd "<s-return>")
      ;; Like in i3 window manager
      #'>>=ansi-term)
    (exwm-input-set-key (kbd "s-r") #'exwm-reset)
    (exwm-input-set-key (kbd "<s-tab>") #'other-frame)
    (exwm-input-set-key (kbd "s-o") #'other-window)
    (exwm-input-set-key (kbd "s-;") #'>>-exwm/swap-last-buffers)
    (dolist (pair >>=|exwm/url-keys)
      (exwm-input-set-key (kbd (car pair)) (>>-url-browser (cdr pair))))
    (exwm-input-set-key (kbd "C-s-/") #'browse-url-at-point)
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
	 ;; TODO: ([?\C-\[] . [escape])
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
	 ))))


(use-package xorns-linux
  :demand t
  :after exwm-input)


(use-package buffer-move
  :ensure t
  :after exwm-input
  :demand t
  :config
  (progn
    (exwm-input-set-key (kbd "<C-S-up>") #'buf-move-up)
    (exwm-input-set-key (kbd "<C-S-down>") #'buf-move-down)
    (exwm-input-set-key (kbd "<C-S-left>") #'buf-move-left)
    (exwm-input-set-key (kbd "<C-S-right>") #'buf-move-right)))


(use-package exwm-workspace
  :after exwm
  :demand t
  :preface
  (progn
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
    )
  :custom
  (exwm-workspace-show-all-buffers t)
  (exwm-layout-show-all-buffers t)
  :config
  (progn
    (exwm-input-set-key (kbd "s-.") #'>>-exwm/switch-workspace-0)
    (exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)
    (exwm-input-set-key (kbd "<C-s-left>") #'>>-exwm/ws-switch-left)
    (exwm-input-set-key (kbd "<C-s-right>") #'>>-exwm/ws-switch-right)))


(provide 'xorns-exwm)
;;; xorns-exwm.el ends here
