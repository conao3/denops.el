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

(require 'denops-json)

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
      (dolist (script (when (file-directory-p dir)
                        (directory-files-recursively dir "main.ts")))
        (save-match-data
          (when (string-match "/denops/\\([^/]*\\)/main.ts$" script)
            (let ((plugin (match-string 1 script)))
              (push (cons plugin (file-truename script)) res))))))
    (nreverse res)))

(defun denops--register (plugin script)
  "Register PLUGIN with SCRIPT."
  (denops--send-notify
   "invoke"
   "register"
   `(,plugin
     ,script
     ( :platform "mac"
       :host "vim"
       :mode "debug"
       :version "9.0.1649")
     (:mode "skip")
     :json-false)))

(defun denops--logging (msg)
  "Logging MSG."
  (with-current-buffer denops--buffer
    (save-excursion
      (goto-char (point-max))
      (insert (current-time-string) " " msg)
      (newline))))

(defun denops--send-notify (type command args)
  "Send TYPE COMMAND with ARGS to denops server."
  (let* ((sexp `(,denops--msgid (,type ,command ,args))))
    (denops--send sexp))
  (prog1 denops--msgid
    (cl-incf denops--msgid)))


;;; Low level functions

(defun denops--send (sexp)
  "Send json encoded SEXP to denops server."
  (let* ((json (denops-json-encode sexp)))
    (denops--logging (format "send(sexp): %S" sexp))
    (denops--logging (format "send      : %s" json))
    (process-send-string denops--process json)))

(defun denops--process-filter (_proc msg)
  "Process output MSG from PROC."
  (denops--logging msg))

(defun denops--process-sentinel (_proc msg)
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
    (denops--logging "start denops server")
    (dolist (elm (denops--gather-plugins))
      (denops--register (car elm) (cdr elm))))
  denops--buffer)

(defun denops-stop-server ()
  "Stop denops server."
  (interactive)
  (when (process-live-p denops--process)
    (delete-process denops--process))
  (denops--logging "stop denops server"))

(provide 'denops)

;;; denops.el ends here
