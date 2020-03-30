;;; xorns-mail.el --- Merchise extensions for sending and receiving mail

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;;; Commentary:

;; Extends standard Emacs modules `smtpmail.el' (simple SMTP protocol for
;; sending mail) and `pop3.el' (Post Office Protocol interface for receiving
;; mails).

;; Enjoy!


;;; Code:

;; Requires, auto-loads and declarations

(require 'cl-lib)

(require 'sendmail)
(require 'smtpmail)
(require 'message)

(require 'widget)
(require 'wid-edit)

(require 'xorns-tools)



;;; Widgets
(defun --required (widget)
  "Validate the WIDGET's value as required."
  (let ((value (widget-value widget)))
    (when (or (null value) (equal value ""))
      (widget-put widget :error "This field is required.")
      widget)))


(define-widget 'xorns-smtp-account-line 'lazy
  "A custom SMTP address line.

A line has the email address, the login, the server and the type of
connection.  Other data such as password and port should be placed in the
~/.authinfo file."
  :tag "SMTP account"
  :type '(list
           (string :tag "Email address"
             :validate --required)
           (choice :tag "Login"
             (const :tag "Use email address" full-email-address)
             (const :tag "User from email address" user-from-email)
             (string))
           (string
             :tag "Server address"
             :help-echo
             "The address of the SMTP server for this account.  If left
empty the address defaults to \"smtp.ADDRESS-DOMAIN\".")
           (choice
             (const :tag "Possibly upgrade to STARTTLS" nil)
             (const :tag "Always use STARTTLS" starttls)
             (const :tag "Never use STARTTLS" plain)
             (const :tag "Use TLS/SSL" ssl))
           ))



;; Local definitions

(setq-default
  mail-default-directory "~/mail/"
  message-directory "~/mail/"
  )

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
            (car
              ;; Chooses the first account that matches
              (loop
                for account in xorns-email-smtp-accounts
                for address = (car account)
                for match = (string-match address from)
                if match
                collect account
                until match))
            ))
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
                (server (string-trim (caddr account)))
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
                    (t login)))
                (envelop-address
                  (cond
                    ((eq login 'full-email-address) user)
                    (t nil))))
          (when (equal "" server)
            ;; TODO: find a function for this
            (setq server
              (concat "smtp." email-domain)))
          (message
            "xorns-email: Setting SMTP. Server: '%s'. Login: '%s'. Type: '%s'"
            server user stream-type)
          (setq
            smtpmail-smtp-server server
            smtpmail-smtp-user user
            smtpmail-stream-type stream-type)
          (when envelop-address
            (setq smtpmail-mail-address envelop-address))
          (setq
            smtpmail-smtp-service
            (case stream-type
              ('ssl 465)
              ('starttls 587)
              (otherwise 25)))
          (when xorns-email-debug
            (setq
              smtpmail-debug-info t
              smtpmail-debug-verb t)))))
    (unless account
      (error "No account matches message's from '%s'"
        (xorns-get-from-address)))))


(defun -xorns-use-appropriate-server (recipient smtpmail-text-buffer &optional ask-for-password)
  "Choose the SMTP account from `xorns-smtp-accounts'."
  (xorns-use-appropriate-smtp-server smtpmail-text-buffer))

(advice-add 'smtpmail-via-smtp :before #'-xorns-use-appropriate-server)



;;; Integration with Gnus reply

(require 'gnus nil)
(require 'gnus-sum nil)

(spam-initialize)


(defun -xorns-gnus-summary-reply (reply-func &rest args)
  "Change the From message header to one of the recipients of the message
that's being replied.

This function is prepared to advice the `gnus-summary-reply' function.  The
REPLY-FUNC is expected to behave as such.  The ARGS contain the arguments to
the original REPLY-FUNC."
  (let* ((article (gnus-summary-article-number))
          (header (gnus-summary-article-header article))
          (rcpt (assoc 'To (mail-header-extra header))))
    (apply reply-func args)
    (save-excursion
      (save-restriction
        (message-narrow-to-headers-or-head)
        (goto-char (point-min))
        ;; Remove the "From: " header
        (delete-matching-lines "^From: "))
      ;; And put it back using the To address... TODO: When the original email
      ;; was sent to several emails, how to get the From from it.
      (message-carefully-insert-headers
        (list (cons 'From (mail-decode-encoded-address-string (cdr rcpt))))))))

(advice-add 'gnus-summary-reply :around #'-xorns-gnus-summary-reply)


(provide 'xorns-mail)
;;; xorns-mail.el ends here
