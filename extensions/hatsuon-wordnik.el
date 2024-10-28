;;; hatsuon-wordnik.el --- Wordnik Plugin for Hatsuon  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2024 Taro Sato
;;
;;; License:
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This module provides an audio getter plugin for `hatsuon.el'.
;;
;;; Code:

(require 'request)

(defun hatsuon-wordnik--api-key ()
  "Get Wordnik API key."
  (plist-get (car (auth-source-search
                   :host "api.wordnik.com"
                   :requires '("api_key")))
             :api_key))

(defun hatsuon-wordnik-audio-url-getter (word)
  "Get audio URL for WORD from Wordnik."
  (let* ((api-key (hatsuon-wordnik--api-key))
         (page-url (format (concat "https://api.wordnik.com"
                                   "/v4/word.json/%s/audio"
                                   "?useCanonical=false&limit=50"
                                   "&api_key=%s")
                           word api-key))
         audio-url)
    (defun hatsuon--audio-url-getter-on-success (&key data &rest _)
      (let* ((items (json-parse-string data))
             item audio-urls)
        (dotimes (i (length items))
          (setq item (elt items i))
          (setq audio-urls (append audio-urls `(,(gethash "fileUrl" item)))))
        (setq audio-url (elt audio-urls (1- (length items))))))

    (request page-url
      :sync t
      :parser (lambda () (buffer-string))
      :status-code '((200 . hatsuon--audio-url-getter-on-success)))

    audio-url))

(provide 'hatsuon-wordnik)
;;; hatsuon-wordnik.el ends here
