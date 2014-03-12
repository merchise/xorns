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

(require 'smtpmail)
(require 'message)
(require 'widget)
(require 'dash)
(require 'xorns-widgets)


(define-widget 'xorns-smtp-account-line 'lazy
   "A custom SMTP address line.

A line has the email address, the login, the server and the type of
connection.  Other data such as password and port should be placed in the
~/.authinfo file."
   :tag "SMTP account"
   :type '(list
	     (string :tag "Email address"
		:validate --required)
	     (string :tag "Login")
	     (string
		:tag "Server address"
		:help-echo
		"The address of the SMTP server for this account.  If left
empty the address defaults to \"smtp.ADDRESS-DOMAIN\".")
	     (choice (const :tag "Possibly upgrade to STARTTLS" nil)
		 (const :tag "Always use STARTTLS" starttls)
		 (const :tag "Never use STARTTLS" plain)
		 (const :tag "Use TLS/SSL" ssl))
	     ))


(defcustom xorns-smtp-accounts nil
"Several SMTP accounts."
   :group 'xorns
   :risky t
   :type '(repeat xorns-smtp-account-line))


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
	      xorns-smtp-accounts)))
    account))


(defun xorns-use-appropriate-smtp-server (&optional buffer)
   "Set the appropriate SMTP related variables in the BUFFER.
If BUFFER is not present, use the current buffer."
   (let ((account (xorns-select-appropriate-smtp-server))
	 (buffer (or buffer (current-buffer))))
      (with-current-buffer buffer
	 (when account
	    ;; TODO: (address login server mech) <- account
	    (let ((address (car account))
		  (login (cadr account))
		  (server (string-utils-trim-whitespace (caddr account)))
		  (stream-type (cadddr account)))
	       (when (equal "" server)
		  ;; TODO: find a function for this
		  (let ((from (xorns-get-from-address)))
		     (-when-let (pos (string-match "@" from))
			(setq server
			   (concat "smtp." (substring from (1+ pos)))))))
	       (message "Setting SMTP account %s, with server '%s'"
		  account server)
	       (setq
		  smtpmail-smtp-server server
		  smtpmail-stream-type stream-type)
	       (when (string-match ".+" login)
		  smtpmail-smtp-user login))))
      (unless account
	 (error "No account matches message's from line"))))


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
