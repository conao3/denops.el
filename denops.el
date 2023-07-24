;;; denops.el --- Write package in Deno  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Version: 0.0.1
;; Keywords: convenience
;; Package-Requires: ((emacs "26.1"))
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

(defgroup denops nil
  "Write package in Deno."
  :group 'convenience
  :link '(url-link :tag "Github" "https://github.com/conao3/denops.el"))

(defcustom denops-server-host 'local
  "Denops server host."
  :type '(choice (const :tag "Local" local)
                 (string :tag "Host")))

(defcustom denops-server-port 50635
  "Denops server port."
  :type 'integer)

(defcustom denops-load-path (list (locate-user-emacs-file "denops"))
  "Denops load path."
  :type '(repeat directory))

(defvar denops--buffer nil)
(defvar denops--process nil)
(defvar denops--msgid 0)

(defun denops--gather-plugins ()
  "Gather plugins."
  (let (res)
    (dolist (dir denops-load-path)
      (dolist (file (when (file-directory-p dir)
                      (directory-files-recursively dir "main.ts")))
        (save-match-data
          (when (string-match "/denops/\\([^/]*\\)/main.ts$" file)
            (let ((plugin (match-string 1 file)))
              (push (cons plugin file) res))))))
    (nreverse res)))

(defun denops--logging (msg)
  "Logging MSG."
  (with-current-buffer denops--buffer
    (save-excursion
      (goto-char (point-max))
      (insert (current-time-string) " " msg)
      (newline))))

(defun denops--send-string (str)
  "Send STR to denops server."
  (process-send-string denops--process str))

(defun denops--process-filter (proc msg)
  "Process output MSG from PROC."
  (denops--logging msg))

(defun denops--process-sentinel (proc msg)
  "Process MSG from PROC."
  (denops--logging (format "sentinel: %s" msg)))

(defun denops-start-server ()
  "Start denops server."
  (interactive)
  (setq denops--buffer (get-buffer-create "*denops*"))
  (unless (process-live-p denops--process)
    (setq denops--process
          (make-network-process
           :name "denops"
           :buffer denops--buffer
           :host denops-server-host
           :service denops-server-port
           :family 'ipv4
           :filter #'denops--process-filter
           :sentinel #'denops--process-sentinel))
    (denops--logging "start denops server"))
  denops--buffer)

(provide 'denops)

;;; denops.el ends here
