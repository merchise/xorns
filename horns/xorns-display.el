;;; xorns-display.el --- Default Display-System Support  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This library defines several utilities to configure display-system.

;; Enjoy!


;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'xorns-tools)


(defconst >>=!font/default-name "Source Code Pro"
  "Default font-name.")


(defconst >>=!font/sizes
  '((nil . 12.8) (small . 11.3) (medium . 12.8) (large . 14.3))
  "Mapping used for `>>=select-font' to convert a symbol to a font size.")


(defvar >>=font/configured nil
  "If default-font is configured or not on a graphic display.")


(defvar >>=|default-font 'medium
  "Default font or prioritized list of fonts.
See `>>=select-font' function for more information.")


(defun >>-display-system-p ()
  "Return if the display-system is initialized."
  (cond
    ((boundp 'ns-initialized)
      ns-initialized)
    ;; on Windows, check the list of fonts instead because w32 gets
    ;; initialized earlier than the graphics system
    ((boundp 'w32-initialized)
      (font-family-list))
    ((boundp 'x-initialized)
      x-initialized)
    ;; fallback to normal loading behavior only if in a GUI
    (t
      (display-graphic-p))))


(defun >>=select-font (option)
  "Select or create a font configuration from the given OPTION.

OPTION can be either nil to use all properties as default; or one of the
symbols `small', `medium', or `large' (see `>>=!font/sizes'); or a
non-negative integer or floating point number specifying the font size; the
form ([FONT-NAME] PROPERTIES) or a list of such (the first font that can be
found with `find-font' will be used).

The return value is a property-list that can be used with the `font-spec'
function, or nil if no font was found."
  (when (symbolp option)
    (cl-assert (setq option (cdr (assq option >>=!font/sizes)))))
  (if (numberp option)
    `(:name ,>>=!font/default-name :size ,option :weight normal :width normal)
    ;; else
    (let ((choices (if (listp (car option)) option (list option)))
          res)
      (while (and (not res) choices)
        (let* ((choice (car choices))
               (name (car choice))
               op)
          (if (stringp name)
            (setq op 'cons)
            ;; else
            (unless (setq name (plist-get choice :name))
              (setq
                name (or (plist-get choice :family) >>=!font/default-name)
                op 'nconc)))
          (if (find-font (font-spec :name name))
            (progn
              (setq res
                (cond
                  ((eq op 'cons)
                    (cl-assert (not (plist-member (cdr choice) :name)))
                    (cons :name choice))
                  ((eq op 'nconc)
                    (nconc `(:name ,name) choice))
                  (t
                    choice)))
              (let ((size (plist-get res :size)))
                (when (symbolp size)
                  (cl-assert (setq size (cdr (assq size >>=!font/sizes))))
                  (plist-put res :size size)))
              (when (not (plist-member res :weight))
                (plist-put res :weight 'normal))
              (when (not (plist-member res :width))
                (plist-put res :width 'normal)))
            ;; else
            (setq choices (cdr choices)))))
      res)))


(defun >>=get-spacemacs-fallbacks ()
  "Build a list of fallback forms as applied in `spacemacs/set-default-font'."
  (let ((font-names
          (plist-get
            '(gnu/linux ("NanumGothic" . "NanumGothic")
              darwin ("Arial Unicode MS" . "Arial Unicode MS")
              cygwin ("MS Gothic" . "Lucida Sans Unicode")
              windows-nt ("MS Gothic" . "Lucida Sans Unicode"))
            system-type)))
    (when font-names
      (list
        `(,(car font-names)
           (#x2776 . #x2793)        ; window numbers
           (#x24b6 . #x24fe)        ; mode-line circled letters
           (#x2295 . #x22a1))       ; mode-line additional characters
        `(,(cdr font-names)
           (#x2190 . #x2200))       ; new version lighter
        ))))


(defun >>=configure-font ()
  "Find and set the default font."
  (when (and (not >>=font/configured) >>=|default-font)
    (if (display-graphic-p)
      (when (>>-display-system-p)
        ;; if display is not ready, this takes another try in startup hook
        (if-let ((res (>>=set-default-font >>=|default-font)))
          (setq >>=font/configured res)
          ;; else
          (warn ">>= warning: cannot find any of the specified fonts.")))
      ;; else
      (setq >>=font/configured 'text-only-terminal))))


(if (boundp 'after-focus-change-function)
  (add-function :after after-focus-change-function '>>=configure-font)
  ;; else
  (with-no-warnings    ; `focus-in-hook' is obsolete since 27.1
    (add-hook focus-in-hook '>>=configure-font)))


(defun >>=set-default-font (option)
  ;; This implementation is based in `spacemacs/set-default-font'.
  "Set the font defined by OPTION.
See `>>=select-font' for posible values of argument OPTION.

If FALLBACK is specified, it must be either the symbol `spacemacs' (see
`>>=get-spacemacs-fallbacks'), or a form containing a font-name and a set of
targets valid for `set-fontset-font', or a sequence of such forms.

FALLBACK if given
must be a `list' with a font-name as its `car' and a sequence of targets as
specified for `' `'Modify fontset NAME to use FONT-SPEC for TARGET characters."
  (>>=on-debug-message "setting default font...")
  (let* ((props (>>=select-font option))
         (fallback (plist-get props :fallback)))
    (when props
      (setq props
        (>>=plist-remove props :fallback :powerline-scale :powerline-offset))
      (set-frame-font (apply 'font-spec props) nil t)
      (let ((font (frame-parameter nil 'font))
            (cell (assq 'font default-frame-alist)))
        (if cell
          (setcdr cell font)
          ;; else
          (nconc default-frame-alist `((font . ,font))))
        (when fallback
          ;; to be able to scale the fallback fonts with the default one
          ;; (for zoom-in/out for instance)
          (setq props (>>=plist-remove props :name :size :height :family))
          (if (eq fallback 'spacemacs)
            (setq fallback (>>=get-spacemacs-fallbacks))
            ;; else
            (when (not (listp (car fallback)))
              (setq fallback (list fallback))))
          (dolist (fb fallback)
            (let ((spec (apply 'font-spec :name (car fb) props)))
              (dolist (target (cdr fb))
                (set-fontset-font
                  "fontset-default" target spec nil 'prepend)))))
        font))))


(provide 'xorns-display)
;;; xorns-display.el ends here
