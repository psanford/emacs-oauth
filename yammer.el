;;; yammer.el -- Simple tool for accessing yammer.com

;; Copyright (C) 2009 Peter Sanford

;; Author: Peter Sanford <peter AT petersdanceparty.com>
;; Version: 1.0

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Simple mode for browsing and posting to yammer.com.
;; This is intended to be a reference for using oauth.el

(require 'json)
(require 'oauth)

(defvar yammer-consumer-key "mykey")
(defvar yammer-consumer-secret "mysecret")
(defvar yammer-request-url "https://www.yammer.com/oauth/request_token")
(defvar yammer-access-url  "https://www.yammer.com/oauth/access_token") 
(defvar yammer-user-authorize "https://www.yammer.com/oauth/authorize")
(defvar yammer-list-url "https://yammer.com/api/v1/messages.json")
(defvar yammer-create-message-url "https://yammer.com/api/v1/messages")

(defvar yammer-access-token nil)

(defun yammer-authenticate (username)
  "Get authentication token"
  (if (file-exists-p "~/.yammer-tokens")
      (progn
        (save-excursion
          (find-file "~/.yammer-tokens")
          (let ((str (buffer-substring (point-min) (point-max))))
            (if (string-match (concat username ":\\([^:]*\\):\\(.*\\)")
                              (buffer-substring (point-min) (point-max)))
                (setq yammer-access-token
                      (make-oauth-access-token 
                       :consumer-key yammer-consumer-key
                       :consumer-secret yammer-consumer-secret
                       :auth-t (make-oauth-t
                                :token (match-string 1 str)
                                :token-secret (match-string 2 str))))))
          (save-buffer)
          (kill-this-buffer))))
  (unless yammer-access-token
    (setq yammer-access-token 
          (oauth-authorize-app yammer-consumer-key yammer-consumer-secret
                               yammer-request-url yammer-access-url
                               yammer-user-authorize))
    (save-excursion
      (find-file "~/.yammer-tokens")
      (end-of-buffer)
      (let ((token (oauth-access-token-auth-t yammer-access-token)))
        (insert (format "%s:%s:%s\n" username 
                      (oauth-t-token token)
                      (oauth-t-token-secret token))))
      (save-buffer)
      (kill-this-buffer)))
  yammer-access-token)
    
(defun yammer-post-message (message)
  "Posts to yammer"
  (interactive "sMessage: ")
  (set-buffer
   (oauth-post-url yammer-access-token yammer-create-message-url 
                   `(("body" . ,message))))
  (beginning-of-buffer)
  (let ((beg (point)) (end) (line))
    (end-of-line)
    (setq end (point))
    (setq line (buffer-substring beg end))
    (if (string-match "201 Created" line) (message "Message Created!")
      (error "Problem creating message: %s" line))))

(defun yammer-list-messages () 
  "List recent posts"
  (interactive)
  (set-buffer (oauth-fetch-url yammer-access-token yammer-list-url))
  (goto-char (point-min))
  (delete-region (point-min) (search-forward "\n\n"))
  (let ((references) (user-alist) (messages)
        (raw (json-read-from-string 
            (buffer-substring (point-min) (point-max)))))
    (setq references (assq 'references raw))
    (setq user-alist 
          (mapcar (lambda (user)
                    `(,(cdr (assq 'id user)) . ,(cdr (assq 'full_name user))))
                  (remove-if-not (lambda (ref)
                                   (equal (cdr (assq 'type ref)) "user"))
                  (cdr references))))
    (setq messages (assq 'messages raw))
    (switch-to-buffer (get-buffer-create "*yammer-messages*"))
    (setq buffer-read-only nil)
    (erase-buffer)
    (loop for yamm across (cdr messages) do
      (insert (format "%s (%s):\n\t%s\n\n"
                      (cdr (assq (cdr (assq 'sender_id yamm)) user-alist))
                      (yammer-pretty-date 
                       (yammer-parse-date (cdr (assq 'created_at yamm))))
                      (replace-regexp-in-string 
                       "\n"
                       "\n\t"
                       (cdr (assq 'plain (cdr (assq 'body yamm)))))))))
  (yammer-messages-mode)
  (beginning-of-buffer))

(defun yammer-parse-date (date-string)
  "Returns a emacs date for the given time string like what `encode-time' returns"
  (apply 'encode-time
         (mapcar (lambda (val)
                   (if (null val) 1 val))
                 (parse-time-string 
                  (replace-regexp-in-string "/" "-" date-string)))))

(defun yammer-pretty-date (date)
  "Pretty relative time"
  (let* ((now (float-time (current-time)))
        (post-time (float-time date))
        (time-diff (ftruncate (/ (- now post-time) 60))))
    (message "%s" time-diff)
    (cond 
     ((< time-diff 1) "1 minute ago")
     ((< time-diff 60) (format "%d minutes ago" time-diff))
     ((< time-diff 120) "1 hour ago")
     ((< time-diff (* 24 60)) (format "%d hours ago" (/ time-diff 60)))
     ((< time-diff (* 48 60)) "1 day ago")
     ((< time-diff (* 7 24 60)) (format "%d days ago" (/ time-diff 60 24)))
     (t (format-time-string "%D %I:%M" date)))))

(define-derived-mode yammer-messages-mode fundamental-mode
  "YammerMessages"
  "Viewing Yammer messages."
  (setq buffer-read-only t)
  (set (make-local-variable 'font-lock-defaults) 
       '(yammer-font-lock-keywords)))

(defvar yammer-font-lock-keywords-1
  (list
   '("^\s*.*(.*):" . 
     font-lock-comment-face)))

(defvar yammer-font-lock-keywords 
  yammer-font-lock-keywords-1
  "Default highlighting for yammer mode")

(yammer-authenticate "psanford")

(provide 'yammer)
