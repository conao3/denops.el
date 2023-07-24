;;; denops-json.el --- Denops client                      -*- lexical-binding: t; -*-

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

(require 'json)

(defun denops-json-encode--atom (obj)
  "Encode atom OBJ as JSON."
  (cond
   ((eq obj :json-false) "false")
   ((eq obj :json-empty-array) "[]")
   ((eq obj :json-empty-object) "{}")
   (t (json-encode obj))))

(defun denops-json-encode--list (obj)
  "Encode list OBJ as JSON."
  (if (keywordp (car obj))
      (let (pairs)
        (while obj
          (push (cons (json-encode (pop obj))
                      (denops-json-encode (pop obj)))
                pairs))
        (format "{%s}"
                (mapconcat
                 (lambda (elm) (format "%s:%s" (car elm) (cdr elm)))
                 (nreverse pairs)
                 ",")))
    (format "[%s]" (mapconcat 'denops-json-encode obj ","))))

(defun denops-json-encode (obj)
  "Encode OBJ to JSON string."
  (cond
   ((atom obj) (denops-json-encode--atom obj))
   ((listp obj) (denops-json-encode--list obj))))

(provide 'denops-json)
;;; denops-json.el ends here
