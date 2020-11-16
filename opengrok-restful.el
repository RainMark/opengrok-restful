;;; opengrok-restful.el --- Emacs Source Code Search With OpenGrok RESTful API -*- lexical-binding: t; -*-

;; Copyright (C) 2020 RainMark

;; Author: RainMark <rain.by.zhou at gmail.com>
;; URL: https://github.com/RainMark/opengrok-restful.el
;; Package-Requires: ((emacs "24.4"))
;; Version: 0.0.1

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
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

;; opengrok restful interface for Emacs
;;
;; See documentation on https://github.com/RainMark/opengrok-restful.el

;;; Code:

(add-to-list 'load-path "~/.emacs.d/elpa/request-20200219.2257/")

(require 'json)
(require 'request)

(defconst opengrok-restful-search-url "http://127.0.0.1:8080/api/v1/search")
(defconst opengrok-restful-buffer "*opengrok-restful*")

(defmacro println (x)
  `(message (prin1-to-string ,x)))

(defun opengrok-restful-process-result (data)
  ;; (println data)
  ;; (setq rr (cdr r))
  ;; (println r)
  ;; (loop for (k . v) in r
  ;;          (println k)
  ;;          (println v)
  ;;          )

  (with-current-buffer (get-buffer-create opengrok-restful-buffer)
    (setq resp (cdr (assoc 'results data)))
    (mapcar (lambda (item)
              ;; (println item)
              (setq k (car item))
              (setq v (cdr item))
              (insert (prin1-to-string k))
              (insert (prin1-to-string v))

              ) resp)
    )
  )

(defun opengrok-restful-lookup-symbol (sym)
  ;; (println (type-of symbol))
  (request opengrok-restful-search-url
    :type "GET"
    :params '(("projects" . "anet") ("full" . "epoll"))
    :parser 'json-read
    :sync t
    :complete (cl-function (lambda (&key data &allow-other-keys)
                             (opengrok-restful-process-result data)
                             ))
    ))

(opengrok-restful-lookup-symbol "epoll")
;; (with-current-buffer (get-buffer-create opengrok-restful-buffer)
;;   (insert "ok"))


;;; opengrok-restful.el ends here
