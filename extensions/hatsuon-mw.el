;;; hatsuon-mw.el --- Merriam-Webster Plugin for Hatsuon  -*- lexical-binding: t -*-
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

(require 'hatsuon)
(require 'request)

(defun hatsuon-audio-url-getter-mw (word)
  "Get audio URL for WORD from Merriam-Webster."
  (let ((page-url (format "https://merriam-webster.com/dictionary/%s" word))
        (audio-file-regexp "\\.*\\(media.merriam-webster.com/audio/prons/en/us/mp3/[a-f]+/%s001.mp3\\)\\.*")
        (audio-url nil))
    (defun hatsuon--audio-url-getter-on-success (&key data &rest _)
      (let* ((_ (string-match (format audio-file-regexp word) data))
             (matched (match-string 1 data)))
        (if matched
            (setq audio-url (concat "https://" matched)))))

    (request page-url
      :sync t
      :parser (lambda () (buffer-string))
      :status-code '((200 . hatsuon--audio-url-getter-on-success)))

    audio-url))

(provide 'hatsuon-mw)
;;; hatsuon-mw.el ends here
