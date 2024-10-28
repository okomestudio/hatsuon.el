;;; hatsuon.el --- English pronunciation player  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2024 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (emms "18") (request "0.3.2") (s "1.13.0"))
;; Keywords: multimedia, language learning
;; URL: https://github.com/okomestudio/hatsuon.el
;;
;; This file is not part of GNU Emacs.
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; hatsuon.el adds the English pronunciation functionality to Emacs.
;;
;;;; Installation
;;
;;;;; MELPA
;;
;; This package is not on MELPA.
;;
;;;;; Manual
;;
;; Install the required packages:
;;
;; + emms
;; + request
;; + s
;;
;; Then place hatsuon.el in your load-path, and put the following line in your
;; init.el file:
;;
;; (require 'hatsuon)
;;
;; Or using use-package with straight:
;;
;; (use-package hatsuon
;;   :straight (:host github :repo "okomestudio/hatsuon.el"))
;;
;; By default, hatsuon.el pull audio from Wiktionary. To use audio
;; from other sites, load their audio URL getter extension:
;;
;; (use-package hatsuon
;;   :straight (:host github :repo "okomestudio/hatsuon.el"
;;                    :files (:defaults "extensions/*"))
;;   :custom (hatsuon-audio-url-getters '(hatsuon-mw-audio-url-getter))
;;   :config
;;   (require 'hatsuon-mw))
;;
;;;; Usage
;;
;; `hatsuon-play-audio': Play the audio for the word
;; `hatsuon-remove-cached-audio-file': Remove an audio file cached locally
;;
;;;; Tips
;;
;; + An audio URL getter is a simple function that takes in a word and construct
;;   an audio URL to download from; add it to `hatsuon-audio-url-getters' in order that
;;   you want it to be tried until audio for the word is found
;;
;;;; Credits
;;
;; The author thanks the following packages: emms[1], for making it easy to deal
;; with audio playback, request[2] for simplifying HTTP processing, s[3] for
;; string manipulation library that makes sense.
;;
;;   [1] https://github.com/emacsmirror/emms
;;   [2] https://github.com/tkf/emacs-request
;;   [3] https://github.com/magnars/s.el
;;
;;; Code:

;;;; Requrements

(require 'cl-lib)
(require 'emms)
(require 'request)
(require 's)

;;;; Customization

(defgroup hatsuon nil
  "Settings for `hatsuon'."
  :group 'extensions
  :group 'convenience
  :link '(url-link "https://github.com/okomestudio/hatsuon.el/hatsuon.el"))

(defcustom hatsuon-audio-url-getters '(hatsuon-audio-url-getter-wiktionary)
  "Audio URL getters."
  :type 'list)

(defcustom hatsuon-audio-cache-dir (expand-file-name
                                    (convert-standard-filename ".cache/hatsuon/")
                                    user-emacs-directory)
  "Cache directory for audio files."
  :type 'string)

(defcustom hatsuon-audio-file-extensions '("mp3" "ogg")
  "Supported audio file extensions."
  :type 'list)

;;;; Variables

;;;; Keymaps

;;;; Commands

;;;###autoload
(defun hatsuon-play-audio (&optional word)
  "Play the pronunciation audio for WORD.
This interactive function tries first to get WORD from active
region, at-point, and then user prompt."
  (interactive (list (hatsuon--string-general-get "Word: ")))
  (let* ((word (s-downcase word))
         (audio-files (hatsuon--audio-files-get-from-cache word))
         (audio-file (car audio-files)))
    (when (not audio-file)
      (hatsuon--try-audio-url-getter 0 word))
    (if (and audio-file (file-exists-p audio-file))
        (hatsuon--audio-play audio-file)
      (message "Pronunciation audio file not found for %s." word))))

;;;###autoload
(defun hatsuon-remove-cached-audio-file (&optional audio-file)
  "Remove AUDIO-FILE from the audio cache directory."
  (interactive
   (list (completing-read "Remove from audio file cache: "
                          (hatsuon--audio-files-get-from-cache)
                          nil t nil nil nil nil)))
  (delete-file audio-file))

;;;; Functions

;;;;; Public

(defun hatsuon-audio-url-getter-wiktionary (word)
  "Get audio URL for WORD from Wiktionary."
  (let ((page-url (format "https://en.wiktionary.org/wiki/File:En-us-%s.ogg" word))
        (audio-file-regexp "\\.*\\(upload.wikimedia.org/wikipedia/commons/[0-9a-f]+/[0-9a-f]+/En-us-%s.ogg\\)\\.*")
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

;;;;; Private

(defun hatsuon--audio-files-get-from-cache (&optional word)
  "Get the audio files from cache for WORD.
If WORD is not given, return all audio files."
  (let* ((pattern (format "%s\\.\\(%s\\)$"
                          (cond ((not word) ".+")
                                (t word))
                          (mapconcat (lambda (s) s) hatsuon-audio-file-extensions "\\|")))
         (audio-files (and (file-exists-p hatsuon-audio-cache-dir)
                           (directory-files hatsuon-audio-cache-dir t pattern))))
    audio-files))

(defun hatsuon--audio-play (audio-file)
  "Play AUDIO-FILE with the EMMS player."
  (emms-play-file audio-file))

(defun hatsuon--string-general-get (prompt &optional initial history default inherit)
  "Read string from region, at-point, or PROMPT.
If a region is active, get the string. If a region is not active,
try `thing-at-point`. If no word is found, then prompt the user
for word. See `read-string` for the meaning of INITIAL, HISTORY,
DEFAULT, and INHERIT."
  (if (region-active-p)
      (prog1
          (buffer-substring-no-properties (region-beginning) (region-end))
        (deactivate-mark)
        (message ""))
    (let ((word (thing-at-point 'word 'no-properties)))
      (if word
          word
        (read-string prompt initial history default inherit)))))

(defun hatsuon--try-audio-url-getter (index word &optional audio-url-getters)
  "Get and play an audio for WORD using a URL getter.
Use INDEX to specify a URL getter in AUDIO-URL-GETTERS."
  (let ((audio-url-getter (nth index (or audio-url-getters hatsuon-audio-url-getters))))
    (when audio-url-getter
      (let ((audio-url (funcall audio-url-getter word)))
        (defun hatsuon--try-audio-url-getter-on-success (&key data &rest _)
          (let* ((parsed-url (url-generic-parse-url audio-url))
                 (url-path-comps (split-string (url-filename parsed-url) "?" nil))
                 (url-path (car url-path-comps))
                 (audio-file (file-name-concat hatsuon-audio-cache-dir
                                               (format "%s.%s"
                                                       word
                                                       (file-name-extension url-path)))))
            (if (not (file-exists-p (file-name-parent-directory audio-file)))
                (make-directory (file-name-parent-directory audio-file) t))
            (with-temp-buffer
              (insert data)
              (setq-local buffer-file-coding-system 'raw-text)
              (write-file audio-file))
            (hatsuon--audio-play audio-file)))

        (defun hatsuon--try-audio-url-getter-on-error (&rest _)
          (message "Error fetching audio file for '%s' usind %s" word audio-url-getter)
          (hatsuon--try-audio-url-getter (+ index 1) word))

        (if audio-url
            (request audio-url
              :parser 'buffer-string
              :status-code
              '((200 . hatsuon--try-audio-url-getter-on-success)
                (404 . hatsuon--try-audio-url-getter-on-error))
              :error
              (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                             (message "Got error: %S" error-thrown)
                             (hatsuon--try-audio-url-getter (+ index 1) word))))
          (hatsuon--try-audio-url-getter-on-error))))))

;;;; Footer

(provide 'hatsuon)
;;; hatsuon.el ends here
