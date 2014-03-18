;;; xorns-mail --- Merchise extensions for sending and receiving mail

;; Copyright (C) 2014 Merchise Autrement

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>
;; or type `C-h C-c' in Emacs.

;; Author: Medardo Rodriguez <med@merchise.org>
;; URL: http://dev.merchise.org/emacs/xorns-mail
;; Keywords: initialization, merchise, convenience
;; Version: 0.1.0

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;;; Commentary:

;; Extends standard Emacs modules `smtpmail.el' (simple SMTP protocol for
;; sending mail) and `pop3.el' (Post Office Protocol interface for receiving
;; mails).

;; Enjoy!


;;; Code:


;; Requires, auto-loads and declarations

(require 'smtpmail)
(require 'xorns-widgets)

(autoload
  'message-narrow-to-headers
  "message"
  "Narrow the buffer to the head of the message.")

(autoload
  'message-fetch-field
  "message"
    "The same as `mail-fetch-field', only remove all newlines.
The buffer is expected to be narrowed to just the header of the message;
see `message-narrow-to-headers-or-head'.")

(declare-function -first "dash.el"
  "Returns the first x in LIST where (PRED x) is non-nil, else nil.

To get the first item in the list no questions asked, use `car'.")



;; Local definitions



(defgroup xorns-email nil
   "Xorns Email"
   :prefix "xorns-email-"
   :group 'xorns)


(defcustom xorns-email-smtp-accounts nil
"Several SMTP accounts."
   :group 'xorns-email
   :risky t
   :type '(repeat xorns-smtp-account-line))


(defcustom xorns-email-debug nil
   "Set to t for adding debuging messages to SMTP."
   :group 'xorns-email
   :risky t
   :type 'boolean)


(defun xorns-get-from-address ()
   "Return the from address (without quoted name) in a message buffer."
   (save-excursion
      (mail-strip-quoted-names
	 (save-restriction
	    (message-narrow-to-headers)
	    (message-fetch-field "from")))))


(defun xorns-select-appropriate-smtp-server ()
  "Choose the SMTP account according to the current message's from line."
  (let* ((from (xorns-get-from-address))
	  (account
	    (-first
	      (lambda (account)
		(let ((address (car account)))
		  (string-match address from)))
	      xorns-email-smtp-accounts)))
    account))


(defun xorns-use-appropriate-smtp-server (&optional buffer)
   "Set the appropriate SMTP related variables in the BUFFER.
If BUFFER is not present, use the current buffer."
   (let ((account (xorns-select-appropriate-smtp-server))
	 (buffer (or buffer (current-buffer))))
      (with-current-buffer buffer
	 (when account
	    ;; TODO: (address login server mech) <- account
	    (let* ((address (car account))
		   (login (cadr account))
		   (server (string-utils-trim-whitespace (caddr account)))
		   (stream-type (cadddr account))
		   (message-from (split-string (xorns-get-from-address) "@"))
		   (email-login (car message-from))
		   (email-domain (cadr message-from))
		   (user
		      (cond
			 ((eq login 'full-email-address)
			    (concat email-login "@" email-domain))
			 ((eq login 'user-from-email)
			    email-login)
			 (t login))))
	       (when (equal "" server)
		  ;; TODO: find a function for this
		  (setq server
		     (concat "smtp." email-domain)))
	       (message "Setting SMTP. Server: '%s'. Login: '%s'. Type: '%s'"
		  server user stream-type)
	       (setq
		  smtpmail-smtp-server server
		  smtpmail-smtp-user user
		  smtpmail-stream-type stream-type)
	       (setq
		  ;; TODO: configure
		  smtpmail-smtp-service (if (eq stream-type 'ssl)
					   465
					   25))
	       (when xorns-email-debug
		  (setq
		     smtpmail-debug-info t
		     smtpmail-debug-verb t)))))
      (unless account
	 (error "No account matches message's from '%s'"
	    (xorns-get-from-address)))))


;; ---
;; TODO: Try to derive an alias-chain like rails'.
(defvar %xorns-super-smtpmail-via-smtp (symbol-function 'smtpmail-via-smtp))
(defun smtpmail-via-smtp (recipient smtpmail-text-buffer
			    &optional ask-for-password)
   "Wrap `smtpmail-via-smtp' to choose among `xorns-smtp-accounts'.

RECIPIENT, SMTPMAIL-TEXT-BUFFER and ASK-FOR-PASSWORD keep their original
meaning."
   (xorns-use-appropriate-smtp-server smtpmail-text-buffer)
   (funcall (symbol-value '%xorns-super-smtpmail-via-smtp)
      recipient smtpmail-text-buffer ask-for-password))



(provide 'xorns-mail)
;;; xorns-mail.el ends here
