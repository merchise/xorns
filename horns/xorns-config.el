;;; xorns-config.el --- Configure and load information file  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:


;; This module manages user options configuration, custom initialization code,
;; and font settings.
;;
;; The location for user options and custom initialization code is set to the
;; first valid option of:
;; - a file, or a directory, "{XDG_CONFIG_HOME}/xorns", or "~/.xorns";
;; - a file inside `user-emacs-directory', "custom-{USER}.el", or "custom.el";
;; - the Emacs initialization file (see `user-init-file').
;;
;; Recommended method is when the location is a directory, using one of the
;; first two options, because it can contain multiple files:
;; - "init.el" for initialization code;
;; - "custom.el" for user options; and
;; - several code files conforming to the "{MAJOR-MODE}-init.el" pattern.
;;
;; If the `>>=|font-settings' variable is not null, this module also
;; configures the font settings using `xorns' approach for that (see
;; `>>=set-font').


;;; Code:

(require 'warnings)
(require 'cus-edit)
(eval-and-compile
  (require 'cl-lib)
  (require 'xorns-tools))



;;; constants and variables

(defvar >>=|font-settings 'medium
  "Variable to configure the default font to be used at Emacs initialization.
See `>>=set-font' function for details about allowed values.")


(defconst >>-!font/default-name "Source Code Pro"
  "Default font-name when none is specified.")


(defconst >>-!font/default-size 'medium
  "Default font-size when none is specified.")


(defconst >>-!font/sizes
  '((tiny . 9.8) (small . 11.3) (medium . 12.8) (large . 14.3) (extra . 15.8))
  "Semantic symbol aliases for several standard font-sizes.")


(defvar >>-font/configured nil
  "If font-settings is configured or not on a graphic display.")



;;; custom user options

(defsubst >>-xdg-config-home ()
  "Get XDG configuration directory."
  (>>=path/find-directory
    (getenv "XDG_CONFIG_HOME")
    (>>=path/join "~" ".config")))


(defun >>-config-file-name ()
  "Return target location for `custom-file'."
  (let ((xdg (>>-xdg-config-home)))
    (expand-file-name
      (if xdg "xorns" ".xorns")    ; file-exists-p  file-directory-p
      (or xdg "~"))))


(defun >>-copy-from-template ()
  "Create new `custom-file' from template."
  (let ((template
          (expand-file-name
            "user-config"
            (>>=path/join
              (>>=value-of >>=!xorns/lib-dir) "templates"))))
    (when (file-exists-p template)
      (copy-file template custom-file t)
      (message ">>= new `custom-file' '%s' has been created." custom-file)
      template)))



;;; font configuration

(defun >>-display-graphic-p ()
  "Return non-nil if the display-system has been initialized."
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


(defun >>-font/raw-option->name (option)
  "Get a font-name from a raw OPTION."
  (cond
    ((stringp option)
      option)
    ((and option (listp option))
      (let ((head (car option)))
        (if (stringp head)
          head
          ;; else
          (>>-font/raw-option->name
            (or
              (plist-get option :name)
              (plist-get option :family))))))))


(defsubst >>-font/alias->size (alias)
  "Convert a font-size ALIAS to its numeric value."
  (let ((size (assq alias >>-!font/sizes)))
    (if size
      (cdr size)
      ;; else
      (error "Invalid font-size alias: %s" alias))))


(defsubst >>-font/cast-size (option)
  "Cast OPTION as a valid number intended as a font-size value."
  (if (numberp option)
    option
    ;; else
    (>>-font/alias->size option)))


(defun >>-font/complement-plist (option)
  "Complement a font property-list OPTION with default values."
  (let ((head (car option)))
    (if (stringp head)
      (if (not (plist-member (cdr option) :name))
        (setq option (cons :name option))
        ;; else
        (error "Double font name specified: %s" head))
      ;; else
      (unless (plist-member option :name)
        (let ((name (or (plist-get option :family) >>-!font/default-name)))
          (setq option (nconc `(:name ,name) option))))))
  (let ((size (plist-get option :size)))
    (unless (numberp size)
      (unless size
        (setq size >>-!font/default-size))
      (plist-put option :size (>>-font/cast-size size))))
  (unless (plist-member option :weight)
    (plist-put option :weight 'normal))
  (unless (plist-member option :width)
    (plist-put option :width 'normal))
  option)


(defun >>-font/find-valid-option (choices)
  "Find an existing font option from a list of CHOICES.
First option must be a list, tails options could be either a string or a
list."
  (seq-find
    (lambda (option)
      (let ((name (>>-font/raw-option->name option)))
        (if (stringp name)
          (find-font (font-spec :name name))
          ;; else
          (error "Missing or wrong name for option: %s" option))))
    choices))


(defun >>-font/option->plist (&optional option)
  "Build default font properties based on possibly incomplete OPTION."
  (cond
    ((null option)
      (>>-font/option->plist (list >>-!font/default-name)))
    ((stringp option)
      (>>-font/option->plist (list option)))
    ((symbolp option)
      (>>-font/option->plist (>>-font/alias->size option)))
    ((numberp option)
      (>>-font/option->plist `(:size ,option)))
    ((listp option)
      (let ((head (car option)))
        (cond
          ((stringp head)
            (>>-font/option->plist (cons :name option)))
          ((listp head)
            (let ((valid (>>-font/find-valid-option option)))
              (if valid
                (>>-font/option->plist valid)
                ;;
                (error "No valid font option found"))))
          (t
            ;; assert head is a :prop
            (>>-font/complement-plist option)))))))


(defun >>-font/get-default-fallbacks ()
  "Build a fallback form to apply as default by `>>=set-font'."
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


(defun >>=set-font (&optional option)
  "Set the font defined by OPTION.
The given OPTION will be normalized to a property-list as used for the
`font-spec' function arguments.

OPTION could be:

- nil, all properties are set to default values.

- A non-negative number, integer or floating point, specifies a font-size that
  is complemented with some extra default values.

- A symbol, a size alias, the mapping `>>-!font/sizes' is used to get the
  equivalent non-negative number.

- A property-list, the standard format.  If the first element is a string, it
  is considered the font-name, and the key `:name' is added as the new `car'.

- A prioritized set of choices, a list that uses another list as its first
  element.  Each item must have a font-name specification, the function
  `find-font' will be used until a valid value is found.

If `:fallback' is specified, it must be either t for a default form (see
`>>-font/get-default-fallbacks'), or a form containing a font-name and a set
of targets valid for `set-fontset-font', or a sequence of such forms."
  (>>=on-debug-message "setting default font...")
  (let* ((props (>>-font/option->plist option))
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
          (if (eq fallback t)
            (setq fallback (>>-font/get-default-fallbacks))
            ;; else
            (when (not (listp (car fallback)))
              (setq fallback (list fallback))))
          (dolist (fb fallback)
            (let ((spec (apply 'font-spec :name (car fb) props)))
              (dolist (target (cdr fb))
                (set-fontset-font
                  "fontset-default" target spec nil 'prepend)))))
        font))))


(defun >>=configure-font ()
  "Find and set the default font."
  (condition-case err
    (when (and (not >>-font/configured) >>=|font-settings)
      (if (display-graphic-p)
        (when (>>-display-graphic-p)
          ;; if display is not ready, this takes another try in startup hook
          (if-let ((res (>>=set-font >>=|font-settings)))
            (setq >>-font/configured res)
            ;; else
            (warn ">>= warning: cannot find any of the specified fonts.")))
        ;; else
        (setq >>-font/configured 'text-only-terminal)))
    (error
      (if init-file-debug
        (signal (car err) (cdr err))
        ;; else
        (message ">>= %s"  (error-message-string err)))
      (setq >>-font/configured err))))


(defun >>=font/configure ()
  "Called after `xorns' is completely initialized to configure fonts."
  (if (boundp 'after-focus-change-function)
    (add-function :after after-focus-change-function '>>=configure-font)
    ;; else
    (with-no-warnings    ; `focus-in-hook' is obsolete since 27.1
      (add-hook focus-in-hook '>>=configure-font)))
  (>>=configure-font))



;;; main configuration

(defun >>-main/configuration ()
  "Execute main configuration."
  (setq custom-file (>>-config-file-name))
  (let ((exists (file-exists-p custom-file))
         save)
    (unless exists
      (setq exists (>>-copy-from-template))
      (let ((old (>>=locate-user-emacs-file "custom-${USER}.el" "custom.el")))
        (when (file-exists-p old)
          (message ">>= migrating old `custom-file' '%s'." old)
          (>>=load old)
          (setq save t))))
    (when (and (not init-file-debug) (eq warning-minimum-level :warning))
      (setq
        warning-minimum-level :error
        warning-minimum-log-level :error))
    (when exists
      (>>=load custom-file))
    (->? >>=settings/init)
    (>>=font/configure)
    (when save
      (if exists
        (let ((make-backup-files nil))
          (message ">>= saving migrated variables.")
          (custom-save-all))
        ;; else
        (warn (concat ">>= migrated variables not saved because a template "
                "was not found to create the new style `custom-file'; "
                "Fix config file manually."))))
    (->? >>=building-blocks/configuration)))


(with-eval-after-load 'xorns-config
  (if (not custom-file)
    (>>-main/configuration)
    ;; else
    (warn ">>= `custom-file' already assigned: '%s'" custom-file)))


(provide 'xorns-config)
;;; xorns-config.el ends here
