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

(defmacro println (x)
  `(message (prin1-to-string ,x)))

(require 'json)
(require 'request)
;; (add-to-list 'load-path "~/.emacs.d/elpa/request-20200219.2257/")

(defconst opengrok-restful-url "http://127.0.0.1:8080/api/v1/search")
(defconst opengrok-restful-buffer "*opengrok-restful*")

(setq opengrok-restful-highlights
      '(("/.+:[0-9]+" . font-lock-constant-face)))

(define-derived-mode opengrok-restful-mode text-mode "opengrok-restful-mode"
  (setq font-lock-defaults '(opengrok-restful-highlights)))

(defun opengrok-restful-cleanup (text)
  (s-replace-all '(("<b>" . "") ("</b>" . "") ("&lt;" . "<") ("&gt;" . ">") ("&amp;" . "&")) text))

(defun opengrok-restful-current-line ()
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defvar opengrok-restful-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")
      (lambda ()
        (interactive "r")
        (message "%s" (opengrok-restful-current-line))))
    map))

(put-text-property (point-min) (point-max) 'keymap opengrok-restful-keymap)
;; (defun property-test (begin end)
;;   (interactive "r")
;;   (put-text-property begin end 'font-lock-face '(:foreground "red"))
;;   (put-text-property begin end 'keymap opengrok-restful-keymap)
;;   )

(defun opengrok-restful-parse-response (data)
  (with-current-buffer (get-buffer-create opengrok-restful-buffer)
    ;; (setq buffer-read-only nil)
    (erase-buffer)
    (opengrok-restful-mode)
    (mapcar (lambda (file)
              (let ((file-name (symbol-name (car file)))
                    (file-lines (cdr file)))
                (mapcar (lambda (line)
                          (let ((line-number (cdr (assoc 'lineNumber line)))
                                (line-content (cdr (assoc 'line line))))
                            (insert file-name ":" line-number ": ")
                            (insert (opengrok-restful-cleanup line-content) "\n")))
                        file-lines)))
            (cdr (assoc 'results data)))
    ;; (setq buffer-read-only t)
    ;; (put-text-property (point-min) (point-max) 'keymap opengrok-restful-keymap)
    ))

(defun opengrok-restful-project-lookup (project type value)
  (request opengrok-restful-url
    :type "GET"
    :params `(("projects" . ,project) (,type . ,value))
    :parser 'json-read
    :sync t
    :complete (cl-function (lambda (&key data &allow-other-keys)
                             (opengrok-restful-parse-response data)))))

(defmacro opengrok-restful-define-lookup (type)
  (let ((fun (intern (format "opengrok-restful-lookup-%s" type))))
    `(defun ,fun ()
       (interactive)
       (opengrok-restful-project-lookup (read-string "Project: ") ,(symbol-name type) (read-string "Symbol: ")))
    ))

(opengrok-restful-define-lookup full)
(opengrok-restful-define-lookup def)
(opengrok-restful-define-lookup symbol)
(opengrok-restful-define-lookup path)

(global-set-key (kbd "M-d") 'opengrok-restful-lookup-def)
(global-set-key (kbd "M-f") 'opengrok-restful-lookup-full)
(global-set-key (kbd "M-s") 'opengrok-restful-lookup-symbol)


(provide 'opengrok-restful)

(message "%s" (opengrok-restful-current-line))
(what-line)
(println (point-min))
(println (point-max))
;; (println (key-binding "M-n"))

;;; opengrok-restful.el ends here
