;;; xorns-preface.el --- Common Systems Tools  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The visual initialization module is loaded first of all, even before
;; package system (`xorns-packages'), it is processed in two parts: preface
;; and epilogue.
;;
;; This is because there are visual features that need to be configured in the
;; first place, and other that would need a frame to be created, or some
;; packages loaded, in order to function accurately.
;;
;; There are some extra problems with the load order of some resources (like
;; the frame/fonts); for regular Emacs launches (non-daemon), are loaded
;; *before* the Emacs configuration is read; but when Emacs is launched as a
;; daemon (using emacsclient), the fonts are not actually loaded until the
;; hook `after-make-frame-functions' is run; but even at that point, the frame
;; is not yet selected (for the daemon case).  Without a selected frame, the
;; `find-font' will not work correctly!
;;
;; So we do the font setup when the main frame gains focus instead, by which
;; time in the Emacs startup process, all of the below are true:
;;
;; - Fonts are loaded (in both daemon and non-daemon cases).
;;
;; - The frame is also selected, and so `find-font' calls work correctly.
;;
;; http://lists.gnu.org/archive/html/help-gnu-emacs/2016-05/msg00148.html
;;
;; In the preface, the mode-line is hidden, useless-bars are removed, and the
;; coding-system is defined to UTF-8.

;; Enjoy!


;;; Code:


(require 'xorns-tools)


(defconst >>-obsolete-focus-in-hook
  (unless (boundp 'after-focus-change-function)
    (intern "focus-in-hook"))
  "This hook is obsolete since 27.1.")


(defvar >>-visual-epilogue-called nil
  "Function `>>-visual/epilogue' must be executed only once.")


(with-eval-after-load 'xorns-preface
  (>>-visual/hidden-mode-line)
  (when (version< emacs-version "27")
    ;; Disable useless GUI: menu, toolbar, scroll-bars, and tool-tips.
    ;; As of Emacs 27, this is done in the `early-init.el' file.
    (unless (eq system-type 'darwin)    ; No disable menu in MacOs
      (menu-bar-mode -1))
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    (tooltip-mode -1))
  ;; Preface and Epilogue
  (>>-visual/preface)
  ;; `focus-in-hook' is obsolete since 27.1
  (if >>-obsolete-focus-in-hook
    (add-hook >>-obsolete-focus-in-hook #'>>-visual/epilogue)
    ;; else: new method
    (add-function :after after-focus-change-function #'>>-visual/epilogue)))



(defun >>-visual/hidden-mode-line ()
  "Hide the mode-line in the initial buffer.
It will be restored later on by `xorns-mode-line' module."
  ;; Based on: http://bzg.fr/emacs-hide-mode-line.html
  (when mode-line-format
    (setq mode-line-format nil)
    (force-mode-line-update)))


(defun >>-visual/preface ()
  "Run at the very beginning."
  ;; internationalization
  (prefer-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8))


(defun >>-visual/epilogue ()
  "Run after the frame is created and focus."
  (unless >>-visual-epilogue-called
    (setq >>-visual-epilogue-called t)
    ;; font configuration
    (require 'xorns-display)
    (declare-function >>=configure-font 'xorns-display)
    (>>=configure-font)))


(provide 'xorns-preface)
;;; xorns-preface.el ends here
