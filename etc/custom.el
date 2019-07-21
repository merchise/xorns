;;; custom.el --- User custom configuration for new age -*- mode: emacs-lisp -*-

;; Copyright (C) Merchise Autrement [~º/~]

;; Author: Medardo Rodriguez <med@merchise.org>

;;; Commentary:
;;
;; This file is the base for the user-specific customization file.  By
;; default, Xorns looks for its user configuration file in the following
;; order: "$XDG_CONFIG_HOME" (defaults to "~/.config/"), "$HOME" user
;; directory ("~").
;;
;; The file-name in the destination folder will be "xorns" (without
;; extension), but when the "$HOME" user directory is used, it is prefixed
;; with a dot ".").


;;; Code:

; Initialization at the very startup beginning (called before building-blocks
; configuration).  It should only modify the values of setting-variables."
(defun >>=settings/init ()
  "Initialization call for user settings customization."
  (setq-default
    ;; Format specification for setting the frame title.
    ; >>=|frame-title-format
    ;	'(multiple-frames "%b"
    ;	   ("" invocation-name " -- "
    ;	     (:eval (abbreviate-file-name default-directory))))

    ;; Default font or prioritized list of fonts.
    ; >>=|default-font '("Source Code Pro"
    ;			    :size 13.5 :weight normal :width normal)

    ;; If non-nil unicode symbols are displayed in the mode line.
    ;; If you use Emacs as a daemon and wants unicode characters only in GUI
    ;; set the value to quoted `display-graphic-p'. (default t)
    ; >>=|mode-line-unicode-symbols t

    ;; If non-nil, start an Emacs server if one is not already running.
    ; >>=|enable-server t

    ;; Set the emacs server socket location.
    ;; If nil, uses whatever the Emacs default is, otherwise a directory path
    ;; like '~/.emacs.d/server\'.  It has no effect if `>>=|enable-server' is
    ;; nil.
    ; >>=|server-socket-dir nil
    ))


; This function is called immediately after `>>=settings/init', before layer
; configuration.  It''s mostly for variables that should be set before
; package-system is loaded.
(defun >>=custom/user-init ()
  "User code as part of initialization process."
  )


(defun >>=user-config ()
  "User code after initialization process."
  )


;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
