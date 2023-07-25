;;; denops-method.el --- Denops client                      -*- lexical-binding: t; -*-

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

(require 'denops-subr)

(defun denops-method--denops/api/cmd (args)
  "Call denops#api#cmd with ARGS."
  (let ((cmd (read (format "(%s)" (nth 0 args))))
        (_context (nth 1 args)))
    (pcase cmd
      (`(enew)
       (let ((buf (generate-new-buffer "*denops*")))
         (pop-to-buffer buf)
         (setq denops--current-buffer buf)))
      (_
       (denops--logging (format "Not implemented: denops/api/cmd %S" args)))))
  0)

(defun denops-method--setline (args)
  "Call setline with ARGS."
  (let ((lnum (nth 0 args))
        (line (nth 1 args)))
    (denops--logging (format "Current buffer: %s" (current-buffer)))
    (with-current-buffer denops--current-buffer
      (save-excursion
        (goto-char (point-min))
        (forward-line (1- lnum))
        (delete-region (point) (line-end-position))
        (dolist (elm (if (listp line) line (list line)))
          (insert elm)
          (newline)))))
  0)

(provide 'denops-method)
;;; denops-method.el ends here
