;;; custom.el --- User custom configuration for new age -*- mode: emacs-lisp -*-

;; Copyright (C) Merchise Autrement [~º/~]

;; Author: Medardo Rodriguez <med@merchise.org>

;;; Commentary:
;;
;; This file is a base template to generate the user-specific customization
;; file.  See `>>=user-config/load' function for more information.


;;; Code:

(defun >>=units/configuration ()
  "Initialization code for building-blocks (units) configuration."
  (setq-default
    ;; It should only modify building-block setting-variables (those prefixed
    ;; with ">>=+") when their default values are not suitable for your
    ;; configuration.  For example:
    ; >>=|base/extra-packages '(autorevert recentf gcmh)
    ))


(defun >>=settings/init ()
  "Initialization code for user-settings customization."
  (setq-default
    ;; Called at the very beginning of the startup process, before building
    ;; blocks configuration.  It should only modify modify setting-variables
    ;; (those prefixed with ">>=|") when their default values are not suitable
    ;; for your configuration.  For example:
    ; >>=|default-font '(:size 12.0 :weight normal :width normal)
    ; >>=|make-backup-files t
    ; >>=|user-mail-address-template "${USER}@gmail.com"
    ; >>=|show-title-in-header-line t
    ;
    ;; Also, you can configure here most user customizations, for example:
    ; frame-title-format
    ))


(defun >>=custom/user-init ()
  "User-code as part of initialization process."
  ; This function is called immediately after `>>=settings/init', before
  ; building-blocks (units) configuration.  It''s mostly for variables that
  ; should be set before package-system is loaded.
  )


(defun >>=user-config ()
  "User-code after initialization process."
  )


;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
