;;; xorns-display.el --- Default Display-System Support  -*- lexical-binding: t -*-

;; Copyright (c) Merchise Autrement [~º/~]

;; Author: Medardo Antonio Rodriguez <med@merchise.org>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This library defines several utilities to configure display-system.

;; Enjoy!


;;; Code:


(require 'xorns-tools)


(defvar >>=!font-configured nil
  "If default-font is configured or not in a graphic display.")


(defvar >>=|default-font nil
  ;; `(:size ,(/ (* 13.8 (display-pixel-width)) 1920.0)
  ;;   :weight normal :width normal)
  "Default font or prioritized list of fonts.")


(defun >>=configure-font ()
  "Find and set the default font."
  (when (and >>=|default-font (not >>=!font-configured))
    (if (display-graphic-p)
      (if (font-family-list)
	(if (>>=set-default-font >>=|default-font)
	  (setq >>=!font-configured t)
          ;; else
	  (message
	    ">>= warning: cannot find any of the specified fonts (%s)!."
	    (let ((aux (car >>=|default-font)))
	      (if (listp aux)
		(mapconcat 'car >>=|default-font ", ")
		; else
		aux))))
	;; if display is not initialized,
	;; this takes another try in emacs-startup hook
	)
      ;; else
      (setq >>=!font-configured 'is-not-a-graphic-display))))


(defun >>=set-default-font (plists)
  "Set the font given the passed PLISTS.

PLISTS has either the form (\"font-name\" :prop1 value1 :prop2 value2 ...), or
is a list of such.  The first font that can be found will be used.
\"font-name\" is optional, if not specified \"Source Code Pro\" is assumed,
when specify more than one form, only one can have default font-name.

NOTE: This implementation is based in Spacemacs, but improved.

The return value is nil if no font was found, truthy otherwise."
  (>>=on-debug-message "setting default font...")
  (unless (listp (car plists))
    (setq plists (list plists)))
  (catch 'break
    (dolist (plist plists)
      (let ((font (car plist)))
	(if (stringp font)
	  (setq plist (cdr plist))
	  ;; else: default font-name
	  (setq font "Source Code Pro"))
	(when (find-font (font-spec :name font))
	  (let* ((props (>>=plist-remove plist
			  :powerline-scale :powerline-offset))
		 (fontspec (apply 'font-spec :name font props)))
	    (set-frame-font fontspec nil t)
	    (push `(font . ,(frame-parameter nil 'font)) default-frame-alist)
	    (let ((fallback-font-names
		    (plist-get
		      '(gnu/linux ("NanumGothic" . "NanumGothic")
			darwin ("Arial Unicode MS" . "Arial Unicode MS")
			cygwin ("MS Gothic" . "Lucida Sans Unicode")
			windows-nt ("MS Gothic" . "Lucida Sans Unicode"))
		      system-type)))
	      ;; TODO: what if `system-type' is not listed above
	      (when fallback-font-names
		;; to be able to scale the fallback fonts with the default one
		;; (for zoom-in/out for instance)
		(let* ((fallback-props (>>=plist-remove props :size :height))
		       (fallback-spec (apply 'font-spec
					:name (car fallback-font-names)
					fallback-props))
		       (fallback-spec2 (apply 'font-spec
					 :name (cdr fallback-font-names)
					 fallback-props)))
		  ;; window numbers
		  (set-fontset-font "fontset-default"
		    '(#x2776 . #x2793) fallback-spec nil 'prepend)
		  ;; mode-line circled letters
		  (set-fontset-font "fontset-default"
		    '(#x24b6 . #x24fe) fallback-spec nil 'prepend)
		  ;; mode-line additional characters
		  (set-fontset-font "fontset-default"
		    '(#x2295 . #x22a1) fallback-spec nil 'prepend)
		  ;; new version lighter
		  (set-fontset-font "fontset-default"
		    '(#x2190 . #x2200) fallback-spec2 nil 'prepend)))))
	  (throw 'break t))))
    nil))


(provide 'xorns-display)
;;; xorns-display.el ends here
