;;; xorns-widgets.el --- Functions for creating and using widgets

;; Copyright (c) Merchise Autrement [~º/~]

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;;; Commentary:

;; Merchise extensions to creating and using widgets.

;; Enjoy!


;;; Code:

(require 'widget)
(require 'wid-edit)


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


(provide 'xorns-widgets)
;;; xorns-widgets.el ends here
