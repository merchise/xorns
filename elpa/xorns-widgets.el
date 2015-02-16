;;; xorns-widgets --- Functions for creating and using widgets

;; Copyright (C) 2014-2015 Merchise

;; Author: Medardo Rodriguez <med@merchise.org>
;; URL: http://dev.merchise.org/emacs/xorns-widgets
;; Keywords: initialization, merchise, convenience
;; Version: 0.1.0

;; This file is NOT part of GNU Emacs but I'd like it. ;)

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
	   (choice (const :tag "Possibly upgrade to STARTTLS" nil)
	     (const :tag "Always use STARTTLS" starttls)
	     (const :tag "Never use STARTTLS" plain)
	     (const :tag "Use TLS/SSL" ssl))
	   ))


(provide 'xorns-widgets)
;;; xorns-widgets.el ends here
