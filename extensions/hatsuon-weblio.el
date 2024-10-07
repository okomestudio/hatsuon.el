;;; hatsuon-weblio.el --- Weblio Plugin for Hatsuon  -*- lexical-binding: t -*-
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

(defun hatsuon-audio-url-getter-weblio (word)
  "Get audio URL for WORD from Weblio."
  (format "https://weblio.hs.llnwd.net/e8/audio/%s.mp3" word))

(provide 'hatsuon-weblio)
;;; hatsuon-weblio.el ends here
