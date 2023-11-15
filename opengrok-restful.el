;;; opengrok-restful.el --- OpenGrok RESTful interface for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2020 RainMark

;; Author: RainMark <rain.by.zhou at gmail.com>
;; URL: https://github.com/RainMark/opengrok-restful.el
;; Package-Requires: ((emacs "24.4"))
;; Version: 0.0.2

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

;; (defmacro println (x)
;;   `(message (prin1-to-string ,x)))

(require 'json)
(require 'request)

(setq opengrok-restful-source-directory "")
(setq opengrok-restful-buffer "*opengrok-restful*")
(setq opengrok-restful-url "http://127.0.0.1:8080/api/v1/search")
(setq opengrok-restful-token "")

(setq opengrok-restful-highlights
      '(("/.+:[0-9]+" . font-lock-constant-face)))

(define-derived-mode opengrok-restful-mode text-mode "opengrok-restful-mode"
  (setq font-lock-defaults '(opengrok-restful-highlights)))

(defun opengrok-restful-cleanup (text)
  (s-replace-all '(("<b>" . "") ("</b>" . "") ("&lt;" . "<") ("&gt;" . ">") ("&amp;" . "&")) text))

(defun opengrok-restful-current-line ()
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun opengrok-restful-jump-to-target (line-content)
  (cl-multiple-value-bind (path linum)
      (save-match-data
        (string-match "/.+:[0-9]+" line-content)
        (split-string (match-string 0 line-content) ":"))
    (find-file (concat
                (file-name-as-directory opengrok-restful-source-directory)
                (substring path 1)))
    (goto-char (point-min))
    (forward-line (1- (string-to-number linum)))
    (kill-buffer opengrok-restful-buffer)
    ))

(setq opengrok-restful-keymap
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "RET")
          (lambda ()
            (interactive)
            (opengrok-restful-jump-to-target (opengrok-restful-current-line))))
        (define-key map (kbd "<escape>")
          (lambda ()
            (interactive)
            (kill-buffer (current-buffer))))
        map))

(defun opengrok-restful-parse-response (data)
  (with-current-buffer (get-buffer-create opengrok-restful-buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (opengrok-restful-mode)
    (mapcar (lambda (file)
              (let ((file-name (symbol-name (car file)))
                    (file-lines (cdr file)))
                (mapcar (lambda (line)
                          (let ((line-number (cdr (assoc 'lineNumber line)))
                                (line-content (cdr (assoc 'line line))))
                            (when (string= "" line-number) (setq line-number "1"))
                            (insert file-name ":" line-number ": ")
                            (insert (opengrok-restful-cleanup line-content) "\n")))
                        file-lines)))
            (cdr (assoc 'results data)))
    (put-text-property (point-min) (point-max) 'keymap opengrok-restful-keymap)
    (setq buffer-read-only t)
    (when (< 0 (buffer-size))
      (progn
        (switch-to-buffer-other-window opengrok-restful-buffer)
        (goto-char (point-min))))
    ))

(defun opengrok-restful-project-lookup (request-params)
  (request opengrok-restful-url
    :type "GET"
    :params request-params
    :headers (list (cons "Authorization" (format "Bearer %s" opengrok-restful-token)))
    :parser 'json-read
    :sync t
    :complete (cl-function (lambda (&key data &allow-other-keys)
                             (opengrok-restful-parse-response data)))))

(defun opengrok-restful-current-project-name ()
  (file-name-nondirectory (directory-file-name (projectile-project-root))))

(defun opengrok-restful-make-params (project type value)
  (let ((params `((,type ,value))))
    (if (not (string= "" project))
        (cons `("projects" . ,(if (string= "c" project)
                                  (opengrok-restful-current-project-name)
                                project))
              params)
      params)))

(defmacro opengrok-restful-define-lookup (type)
  (let ((fun (intern (format "opengrok-restful-lookup-%s" type))))
    `(defun ,fun ()
       (interactive)
       (let ((default-project )
             (default-symbol (thing-at-point 'symbol)))
         (opengrok-restful-project-lookup
          (opengrok-restful-make-params
           (read-string "Project> ")
           ,(symbol-name type)
           (read-string (format "Symbol (%s)> " default-symbol) nil nil default-symbol)))
         ))))

(opengrok-restful-define-lookup full)
(opengrok-restful-define-lookup def)
(opengrok-restful-define-lookup symbol)
(opengrok-restful-define-lookup path)

(provide 'opengrok-restful)

;;; opengrok-restful.el ends here
