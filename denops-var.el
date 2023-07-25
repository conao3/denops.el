;;; denops-var.el --- Denops client                      -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a client library for Denops.

;;; Code:

(defcustom denops-server-host 'local
  "Denops server host."
  :type '(choice (const :tag "Local" local)
                 (string :tag "Host"))
  :group 'denops)

(defcustom denops-server-port 50635
  "Denops server port."
  :type 'integer
  :group 'denops)

(defcustom denops-load-path (list (locate-user-emacs-file "denops"))
  "Denops load path."
  :type '(repeat directory)
  :group 'denops)

(defvar denops--buffer nil)
(defvar denops--process nil)
(defvar denops--msgid 0)

(provide 'denops-var)
;;; denops-var.el ends here
