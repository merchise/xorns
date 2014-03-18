;;; xorns-init --- Execute all Merchise members common initialization

;; Copyright (C) 2014 Merchise Autrement

;; Author: Medardo Rodriguez <med@merchise.org>
;; URL: http://dev.merchise.org/emacs/xorns-init
;; Keywords: initialization, merchise, convenience
;; Version: 20140316.1200

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

;; Write your commentary here in only one line in this moment...

;; Enjoy!


;;; Code:

;; Start server
(require 'server)
(unless (server-running-p)
  (server-start))


(provide 'xorns-init)
;;; xorns-init.el ends here
