;;; xorns-config.el --- Configure and load information file  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module manages user options configuration, custom initialization code,
;; and font settings.
;;
;; A folder (usually "{XDG_CONFIG_HOME}/xorns") can be used to separate these
;; concepts into different files:
;;   - "user-config.el" for the new `xorns' style user options, and
;;   - "custom.el" to set `custom-file' variable, the standard customization
;;     information of Emacs.
;;
;; If your operating system does not comply with XDG standards, this folder
;; can be located in the user home ("~/.xorns").
;;
;; For backward compatibility, you can still use a single file in one of the
;; several locations:
;;   1. {XDG_CONFIG_HOME}/xorns
;;   2. ~/.xorns
;;   3. `user-emacs-directory'/custom.el
;;
;; When using a folder, in addition to the two basic files, you can add code
;; that will be executed every time a major mode is entered.  These files must
;; have a name fulfilling "`major-mode'-config.el".  Normally, a `major-mode'
;; configuration module is loaded only the first time a file is opened in this
;; mode.  This behavior can be changed by assigning nil to the variable named
;; ">>=config/`major-mode'-loaded", which is automatically created when the
;; mode is initialized.  You can see an example for `python-mode' user
;; configuration in the directory "xorns/horns/templates".
;;
;; Optionally, a `major-mode' configuration module can define two functions:
;;   - ">>=config/on-load-`major-mode'": Executed after a mode configuration
;;     module is loaded.
;;   - ">>=config/on-visit-`major-mode'-file": executed every time a file is
;;     visited in the configured mode.
;;
;; If the `>>=|font-settings' variable is not nil, this module also configures
;; the font settings using `xorns' approach for that (see `>>=set-font').


;;; Code:

(require 'cus-edit)
(eval-and-compile
  (require 'cl-lib)
  (require 'xorns-tools))



;;; constants and variables

(defvar >>=config/user-folder nil
  "Location for user options and custom initialization code.
If not nil, a folder is used for several files including the `custom-file'.")


(defconst >>-!config/custom-file "custom.el"
  "Name used for file storing customization information (see `custom-file').")


(defconst >>-!config/user-file "user-config.el"
  "Name used for file storing user options.")


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

(defun >>-config/settle-warning-minimum-level ()
  "Settle `warning-minimum-level' to `:error' if not in debug mode."
  (require 'warnings)
  (when (and (not init-file-debug) (eq warning-minimum-level :warning))
    (setq
      warning-minimum-level :error
      warning-minimum-log-level :error)))


(defsubst >>-config/xdg-home ()
  "Get XDG configuration directory."
  (>>=path/find-directory
    (getenv "XDG_CONFIG_HOME")
    (>>=path/join "~" ".config")))


(defsubst >>-config/home-location ()
  "Prospective location for `custom-file' using user home."
  (let ((xdg (>>-config/xdg-home)))
    (expand-file-name
      (if xdg "xorns" ".xorns")
      (or xdg "~"))))


(defun >>-config/settle-location ()
  "Settle `custom-file' location and user options folder."
  (let ((home-config (>>-config/home-location))
        (cfname >>-!config/custom-file))
    (cond
      ((file-directory-p home-config)
        (setq
          >>=config/user-folder home-config
          custom-file (expand-file-name cfname home-config)))
      ((file-readable-p home-config)
        (setq custom-file home-config))
      (t
        (setq
          custom-file (expand-file-name cfname user-emacs-directory))))))


(defsubst >>-config/expand-user-file (name)
  "Convert file NAME to absolute using `>>=config/user-folder' to expand it.
Only return the name if the file is readable."
  (when >>=config/user-folder
    (let ((res (expand-file-name name >>=config/user-folder)))
      (when (file-readable-p res)
        res))))


(defsubst >>-config/user-file ()
  "Get the value for the user options file if it exists and is readable."
  (>>-config/expand-user-file >>-!config/user-file))


(defsubst >>-config/check-major-mode-load ()
  "Check if there is a `major-mode' user code file should be loaded."
  (let ((symbol (intern (format ">>=config/%s-loaded" major-mode))))
    (if (boundp symbol)
      (not (symbol-value symbol))
      ;; else
      (set symbol t))))    ; always load first time


(defsubst >>-config/major-mode-load-function ()
  "Function to be executed any time a `major-mode' is loaded."
  (>>=check-function (format ">>=config/on-load-%s" major-mode)))


(defsubst >>-config/major-mode-visit-file-function ()
  "Function to be executed any time a `major-mode' file is visited."
  (>>=check-function (format ">>=config/on-visit-%s-file" major-mode)))


(defsubst >>-config/mode-file ()
  "Get the file name for a `major-mode' code hook."
  (>>-config/expand-user-file (format "%s-config.el" major-mode)))


(defun >>-config/run-mode-enter-hooks ()
  "This function is executed when entering a `text-mode' or a `prog-mode'."
  (when-let ((name (>>-config/mode-file)))
    (when (>>-config/check-major-mode-load)
      (>>=load name)
      (>>=call? (>>-config/major-mode-load-function)))
    (>>=call? (>>-config/major-mode-visit-file-function))))


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
    ((consp option)
      (let ((head (car option)))
        (cond
          ((stringp head)
            head)
          ((>>=real-symbol head)
            (face-attribute head :family))
          (t
            (>>-font/raw-option->name
            (or
              (plist-get option :name)
              (plist-get option :family)))))))))


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



;;; fonts

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
                  "fontset-default-toplevel-value" target spec nil 'prepend)))))
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
  (add-function :after after-focus-change-function '>>=configure-font)
  (>>=configure-font))



;;; main configuration

(defun >>-main/configuration ()
  "Execute main configuration."
  (>>-config/settle-warning-minimum-level)
  (>>-config/settle-location)
  (let ((user-file (>>-config/user-file)))
    (when user-file
      (>>=load user-file)
      (->? >>=settings/init)
      (>>=font/configure))
    (when (file-exists-p custom-file)
      (>>=load custom-file))
    (unless user-file
      ;; backward compatibility
      (->? >>=settings/init)
      (>>=font/configure)
      (warn
        (concat
          ">>= user options file '%s' does not exists, "
          "check the `xorns-config' module documentation.")
        >>-!config/user-file))
    (when >>=config/user-folder
      (add-hook 'text-mode-hook '>>-config/run-mode-enter-hooks)
      (add-hook 'prog-mode-hook '>>-config/run-mode-enter-hooks))
    (->? >>=building-blocks/configuration)))


(with-eval-after-load 'xorns-config
  (if (not custom-file)
    (>>-main/configuration)
    ;; else
    (warn ">>= `custom-file' already assigned: '%s'" custom-file)))


(provide 'xorns-config)
;;; xorns-config.el ends here
