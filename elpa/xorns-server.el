;;; xorns-server.el --- Merchise Emacs server start

;; Copyright (c) 2014 Merchise Autrement

;; Author: Medardo Rodriguez <med@merchise.org>
;; Version: 0.1
;; TODO: ;; Package-Requires: ((flange "1.0"))
;; Keywords: merchise, extensions, setup
;; URL: http://dev.merchise.org/xorns/setup

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
;; or type `C-h C-c` in Emacs.

;; This file is NOT part of GNU Emacs but I'd like it. ;)

;;; Commentary:

;; This module activate Emacs server the first time it runs.

;; Start the server, so "emacsclient" could automatically communicate.  In
;; "xorns" project there are several scripts that use internally
;; `emacsclient'; for example: `emc' and `emacs-nw'.



;;; Code:


(require 'server)
(unless (server-running-p)
  (server-start))


(provide 'xorns-server)
;;; xorns-server.el ends here
