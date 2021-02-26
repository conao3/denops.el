;;; denops.el --- Write package in Deno  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Version: 0.0.1
;; Keywords: convenience
;; Package-Requires: ((emacs "26.1") (json-rpc-server "0.2"))
;; URL: https://github.com/conao3/denops.el

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Write package in Deno.


;;; Code:

(require 'json-rpc-server)

(defgroup denops nil
  "Write package in Deno."
  :group 'convenience
  :link '(url-link :tag "Github" "https://github.com/conao3/denops.el"))

(provide 'denops)

;;; denops.el ends here
