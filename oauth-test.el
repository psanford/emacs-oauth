;;; oauth-test.el --- Tests for oauth.el

;; Copyright (C) 2009 Peter Sanford

;; Author: Peter Sanford <peter AT petersdanceparty.com>

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
;; 
;; This file contains tests for oauth.el. Currently the tests cover
;; the generation of HMAC-SHA1 signatures.
;; 
;; The test data is taken from http://term.ie/oauth/example/

(require 'ert)
(require 'oauth)

(ert-deftest oauth-make-request ()
  (let ((req (oauth-make-request "Mercurochrome-knocked" 
                                 "behave-retaking"
                                 (make-oauth-t 
                                  :token "pearlier-Wallace\'s"
                                  :token-secret "Asunción-yuccas"))))
    (should (oauth-request-p req))
    (should (oauth-t-p (oauth-request-token req)))
    (let ((params (oauth-request-params req)))
      (loop for key in (list "oauth_consumer_key"
                             "oauth_timestamp"
                             "oauth_nonce"
                             "oauth_version") do
                             (should (assoc key params))))))

(ert-deftest oauth-request-to-header ()
  (let ((req (make-oauth-request
              :url "http://term.ie/oauth/example/request_token.php"
              :params '(("oauth_consumer_key" . "choleric-decolletes")
                        ("oauth_timestamp" . "1229298753")
                        ("oauth_nonce" . "0123456789")
                        ("oauth_signature_method" . "PLAINTEXT")
                        ("oauth_signature" . "dont_tell&")
                        ("oauth_version" . "1.0")
                        ("oauth_token" . "highballs-Croesus")))))
    (should (equal
     '(("Authorization" . "OAuth realm=\"\", oauth_consumer_key=\"choleric-decolletes\", oauth_nonce=\"0123456789\", oauth_signature=\"dont_tell%26\", oauth_signature_method=\"PLAINTEXT\", oauth_timestamp=\"1229298753\", oauth_token=\"highballs-Croesus\", oauth_version=\"1.0\""))
     (oauth-request-to-header req)))))

;; HMAC-SHA1 request
;; [oauth_version] => 1.0
;; [oauth_nonce] => 064f679764ac13d475e674672c106322
;; [oauth_timestamp] => 1231020688
;; [oauth_consumer_key] => key
;; [oauth_signature_method] => HMAC-SHA1
;; [http_method:private] => GET
;; [http_url:private] => http://term.ie/oauth/example/request_token.php
;; [base_string] => GET&http%3A%2F%2Fterm.ie%2Foauth%2Fexample%2Frequest_token.php&oauth_consumer_key%3Dkey%26oauth_nonce%3D064f679764ac13d475e674672c106322%26oauth_signature_method%3DHMAC-SHA1%26oauth_timestamp%3D1231020688%26oauth_version%3D1.0
;; results in signature:
;; [oauth_signature] => grs+SdPE0DTE2+KYdlBXVk4Z+jc=
  
(ert-deftest hmac-sha1-base-string ()
  (let ((req (make-oauth-request 
              :http-method "POST"
              :url "http://term.ie/oauth/example/request_token.php"
              :params '(("oauth_consumer_key" . "k&ey")
                        ("oauth_timestamp" . "1231020688")
                        ("oauth_nonce" . "064f679764ac13d475e674672c106322")
                        ("oauth_version" . "1.0")
                        ("oauth_signature_method" . "HMAC-SHA1")))))
    (should (equal
     "POST&http%3A%2F%2Fterm.ie%2Foauth%2Fexample%2Frequest_token.php&oauth_consumer_key%3Dk%2526ey%26oauth_nonce%3D064f679764ac13d475e674672c106322%26oauth_signature_method%3DHMAC-SHA1%26oauth_timestamp%3D1231020688%26oauth_version%3D1.0"
     (oauth-build-signature-basestring-hmac-sha1 req)))))

(ert-deftest hmac-sha1-base-string-with-url-params ()
  (let ((req (make-oauth-request 
              :http-method "POST"
              :url "http://term.ie/oauth/example/request_token.php?id=1234&other_param=true"
              :params '(("oauth_consumer_key" . "k&ey")
                        ("oauth_timestamp" . "1231020688")
                        ("oauth_nonce" . "064f679764ac13d475e674672c106322")
                        ("oauth_version" . "1.0")
                        ("oauth_signature_method" . "HMAC-SHA1")))))
    (should (equal
     "POST&http%3A%2F%2Fterm.ie%2Foauth%2Fexample%2Frequest_token.php&id%3D1234%26oauth_consumer_key%3Dk%2526ey%26oauth_nonce%3D064f679764ac13d475e674672c106322%26oauth_signature_method%3DHMAC-SHA1%26oauth_timestamp%3D1231020688%26oauth_version%3D1.0%26other_param%3Dtrue"
     (oauth-build-signature-basestring-hmac-sha1 req)))))

(ert-deftest hmac-sha1-request-signature ()
  (let ((req (make-oauth-request
              :url "http://term.ie/oauth/example/request_token.php"
              :params '(("oauth_consumer_key" . "key")
                        ("oauth_timestamp" . "1231020688")
                        ("oauth_nonce" . "064f679764ac13d475e674672c106322")
                        ("oauth_version" . "1.0")
                        ("oauth_signature_method" . "HMAC-SHA1"))))
        (secret "secret"))
    (should (equal
     "grs+SdPE0DTE2+KYdlBXVk4Z+jc="
     (oauth-build-signature-hmac-sha1 req secret)))))

(ert-deftest sign-request-hmac-sha1 ()
  (let ((req (make-oauth-request
              :url "http://term.ie/oauth/example/request_token.php"
              :params '(("oauth_consumer_key" . "key")
                        ("oauth_timestamp" . "1231020688")
                        ("oauth_nonce" . "064f679764ac13d475e674672c106322")
                        ("oauth_version" . "1.0"))))
        (secret "secret"))
    (oauth-sign-request-hmac-sha1 req secret)
    (should (equal
     "HMAC-SHA1"
     (cdr (assoc "oauth_signature_method" (oauth-request-params req)))))
    (should (equal
     "grs+SdPE0DTE2+KYdlBXVk4Z+jc="
     (cdr (assoc "oauth_signature" (oauth-request-params req)))))))


;;     [parameters:private] => Array
;;         (
;;             [oauth_version] => 1.0
;;             [oauth_nonce] => 25c105a828aaef0837213304376d95e6
;;             [oauth_timestamp] => 1231121320
;;             [oauth_consumer_key] => k&ey
;;             [oauth_token] => requestkey
;;             [oauth_signature_method] => HMAC-SHA1
;;             [oauth_signature] => TatnsMj+1RF5LzRS1/i3IeQQ21g=
;;         )
;;     [http_method:private] => GET
;;     [http_url:private] => http://term.ie/oauth/example/request_token.php
;;     [base_string] => GET&http%3A%2F%2Fterm.ie%2Foauth%2Fexample%2Frequest_token.php&oauth_consumer_key%3Dk%2526ey%26oauth_nonce%3D25c105a828aaef0837213304376d95e6%26oauth_signature_method%3DHMAC-SHA1%26oauth_timestamp%3D1231121320%26oauth_token%3Drequestkey%26oauth_version%3D1.0

(ert-deftest sign-request-with-token-hmac-sha1()
  (let ((req (make-oauth-request 
               :url "http://term.ie/oauth/example/request_token.php"
               :params '(("oauth_consumer_key" . "k&ey")
                         ("oauth_version" . "1.0")
                         ("oauth_nonce" . "25c105a828aaef0837213304376d95e6")
                         ("oauth_timestamp" . "1231121320"))
               :token (make-oauth-t :token "requestkey"
                                    :token-secret "requestsecret")))
         (secret "secret"))
    (oauth-sign-request-hmac-sha1 req secret)
    (should (equal
     "HMAC-SHA1"
     (cdr (assoc "oauth_signature_method" (oauth-request-params req)))))
    (should (equal
     "TatnsMj+1RF5LzRS1/i3IeQQ21g="
     (cdr (assoc "oauth_signature" (oauth-request-params req)))))))