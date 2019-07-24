;;; xorns-startup.el --- Xorns Initialization Library

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This library defines several utility functions used in Emacs main
;; initialization process.  Including all definitions for user custom
;; configuration.

;;; Code:

(require 'xorns-tools)
(require 'xorns-ui)
(require 'xorns-fonts)


(defvar >>=!emacs-initialized nil
  "Whether or not Xorns has finished the startup process.
This is set to true when executing `emacs-startup-hook'.")


(defun >>=init ()
  "General startup initialization."
  (hidden-mode-line-mode)    ;; xorns-ui
  (>>=ui/remove-rubbish)
  (prefer-coding-system 'utf-8)
  (>>=user-config/load)  ; calls `>>=settings/init'
  (>>=-start-maximized)
  (->? >>=custom/user-init)
  ; (>>=initialize-building-blocks)
  (>>=frame-title-init)
  ; load-default-theme
  (>>=configure-font)
  (setq inhibit-startup-screen t)
  (setq
    inhibit-startup-screen t
    initial-scratch-message nil)
  (require 'xorns-packages)
  ; (>>=building-blocks/load)
  )


(defun >>=-start-maximized ()
  "Start Emacs maximized."
  (set-frame-parameter nil 'undecorated t)
  (add-to-list 'default-frame-alist '(undecorated . t))
  (unless (frame-parameter nil 'fullscreen)
    (toggle-frame-maximized))
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))


(defun >>=frame-title-init ()
  "Configure template for displaying the title bar of visible frames.
See `frame-title-format' variable."
  ;; TODO: Spacemacs uses a function to prepare variable value
  (when (and (display-graphic-p) >>=|frame-title-format)
    (require 'format-spec)
    (setq frame-title-format >>=|frame-title-format)))


(defun >>=setup-package-system ()
  "Initialize `package.el' and bootstrap `use-package' if needed."
  (unless (boundp 'package--initialized)
    (require 'package)
    (setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("org" . "https://orgmode.org/elpa/")
	("gnu" . "https://elpa.gnu.org/packages/"))))
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package))


(defun >>=setup-emacs-startup-hook ()
  "Add post initializtion processing."
  (add-hook
   'emacs-startup-hook
   (defun >>=startup-hook ()
     (->? >>=user-config)
     ; TODO: initialize-custom-file-sync
     (>>=configure-font)
     (setq >>=!emacs-initialized
       (float-time (time-subtract nil emacs-start-time))))))



;; User Customization
;; configurations.  Xorns looks for its user configuration file in the
;; following order: "$XDG_CONFIG_HOME" (defaults to "~/.config/"), "$HOME"
;; user directory ("~").
;;
;; The file-name in the destination folder will be "xorns" (without
;; extension), but when the "$HOME" user directory is used, it is prefixed
;; with a dot ".").


(defconst >>=!xdg-config-home
  (find-dir (getenv "XDG_CONFIG_HOME") (dir-join "~" ".config"))
  "Default directory for user configurations.")

(defconst >>=!config/location
  (let ((dir >>=!xdg-config-home)
	(name "xorns"))
    (if (not dir)
      (setq dir "~"
	    name (format ".%s" name)))
    (expand-file-name name dir))
  "User specific configuration file location.")

(defconst >>=!config//template-location
  (expand-file-name "custom.el" (dir-join >>=!base-directory "etc"))
  "User specific configuration template file location.")


(defun >>=user-config/load ()
  "Load user private configuration init file if it exists."
  (>>=-user-config//ensure-file)
  (if (not custom-file)
    (setq custom-file >>=!config/location))
  (load >>=!config/location)
  (->? >>=settings/init))


(defun >>=-user-config//ensure-file ()
  "Ensure user private configuration file properly exists."
  (if (not (file-exists-p >>=!config/location))
    (let ((dir (file-name-directory >>=!config/location)))
      (if (not (file-directory-p dir))
	(make-directory dir 'parents))
      (copy-file >>=!config//template-location >>=!config/location t)
      (message "%s has been installed." >>=!config/location))))


(defmacro ->? (func &rest args)
  "Call FUNC with our remaining ARGS, only if it is bound."
  `(when (fboundp ',func)
     (if init-file-debug
       (message ">>= calling: %s"
	 (or (documentation ',func) ,(symbol-name func))))
     (condition-case-unless-debug err
       (,func ,@args)
       (error
	 (message "Xorns Error in '%s': %s\n"
	   ',(symbol-name func) (error-message-string err))))))



;; Configuration Variables

(defvar >>=|frame-title-format
  '(multiple-frames "%b"
     ("" invocation-name " -- "
       (:eval (abbreviate-file-name default-directory))))
  "Template for displaying the title bar of visible frames.")

(defvar >>=|default-font
  '("Source Code Pro" :size 13.5 :weight normal :width normal)
  "Default font or prioritized list of fonts.")

(defvar >>=|mode-line-unicode-symbols t
  "If non nil unicode symbols are displayed in the mode-line.
If you use Emacs as a daemon and wants unicode characters only in GUI set
the value to quoted `display-graphic-p'.")

(defvar >>=|enable-server t
  "If non-nil, start an Emacs server if one is not already running.")

(defvar >>=|server-socket-dir nil
  "Set the Emacs server socket location.
If nil, uses whatever the Emacs default is, otherwise a directory path like
'~/.emacs.d/server'.  Has no effect if `>>=|enable-server' is nil.")


(provide 'xorns-startup)
;;; xorns-startup.el ends here
