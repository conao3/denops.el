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
(require 'denops-method)
(require 'denops-subr)

(defgroup denops nil
  "Write package in Deno."
  :group 'convenience
  :link '(url-link :tag "Github" "https://github.com/conao3/denops.el"))

(defun denops--response (sexp)
  "Process response SEXP."
  (let (fn res)
    (pcase sexp
      (`("call" "denops#api#vim#call" (,fn-name ,args) ,msgid)
       ;; # cannot use symbol without escape
       (setq fn-name (replace-regexp-in-string "#" "/" fn-name))
       (setq fn (symbol-function (intern (concat "denops-method--" fn-name))))
       (unless fn
         (error "No such method: denops-method--%s" fn-name))
       (setq res (funcall fn args))
       (let ((result (plist-get res :result))
             (error (plist-get res :error)))
         (denops--logging (format "response: %s" res))
         (when res
           (if error
               `(,msgid (,result ,error))
             `(,msgid (,result "")))))))))


;;; Low level functions

(defun denops--process-filter (_proc msg)
  "Process output MSG from PROC."
  (let ((json-object-type 'plist)
        (json-array-type 'list))
    (let ((sexp (json-read-from-string msg)))
      (denops--logging (format "recv      : %s" msg))
      (denops--logging (format "recv(sexp): %S" sexp))
      (denops--send (denops--response sexp)))))

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
      (denops--send-notify
       "invoke"
       "register"
       `(,(car elm)
         ,(cdr elm)
         ( :platform "mac"
           :host "vim"
           :mode "debug"
           :version "9.0.1649")
         (:mode "skip")
         :json-false))))
  denops--buffer)

(defun denops-stop-server ()
  "Stop denops server."
  (interactive)
  (when (process-live-p denops--process)
    (delete-process denops--process))
  (denops--logging "stop denops server"))

(provide 'denops)

;;; denops.el ends here
