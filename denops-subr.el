;;; denops-subr.el --- Denops client                      -*- lexical-binding: t; -*-

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

(require 'denops-var)
(require 'denops-json)

(defun denops--send (sexp)
  "Send json encoded SEXP to denops server."
  (let ((json (denops-json-encode sexp)))
    (denops--logging (format "send(sexp): %S" sexp))
    (denops--logging (format "send      : %s" json))
    (process-send-string denops--process json)))

(defun denops--send-notify (type command args)
  "Send TYPE COMMAND with ARGS to denops server."
  (let* ((sexp `(,denops--msgid (,type ,command ,args))))
    (denops--send sexp))
  (prog1 denops--msgid
    (cl-incf denops--msgid)))

(defun denops--logging (msg)
  "Logging MSG."
  (with-current-buffer denops--buffer
    (save-excursion
      (goto-char (point-max))
      (insert (current-time-string) " " msg)
      (newline))))

(defun denops--gather-plugins ()
  "Gather plugins."
  (let (res)
    (dolist (dir denops-load-path)
      (dolist (script (when (file-directory-p dir)
                        (directory-files-recursively dir "main.ts")))
        (save-match-data
          (when (string-match "/denops/\\([^/]*\\)/main.ts$" script)
            (let ((plugin (match-string 1 script)))
              (push (cons plugin (file-truename script)) res))))))
    (nreverse res)))

(provide 'denops-subr)
;;; denops-subr.el ends here
