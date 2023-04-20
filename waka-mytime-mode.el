;;; wakame-mode.el --- wakatime mode in elisp -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Stanislav Ochotnický
;;
;; Author: Stanislav Ochotnický <stanislav@ochotnicky.com>
;; Maintainer: Stanislav Ochotnický <stanislav@ochotnicky.com>
;; Created: April 17, 2023
;; Modified: April 17, 2023
;; Version: 0.0.1
;; Keywords: convenience data docs files languages lisp  tools
;; Homepage: https://github.com/sochotnicky/wakame-mode
;; Package-Requires: ((emacs "28.2"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Enable collection of activity for WakaTime using native Emacs functionality.
;;  Unlike official wakatime-mode this way we can provide support for more
;;  built-in functionality:
;;  - Tramp
;;  - Project.el and projectile project detection
;;  - Non-file buffers (dired, proced etc.)
;;
;;
;;  Description
;;
;;; Code:

(require 'magit-git)
(require 'project)
(require 'json)

(defconst wakame-mode-version "0.0.1")
(defconst wakame-mode-user-agent "emacs-wakatime")


(defgroup wakame-mode nil
  "Customizations for WakaTime native mode."
  :group 'convenience
  :prefix "wakame-mode-")

(defcustom wakame-mode-api-url "https://wakapi.dev/api"
  "API URL for WakaTime."
  :type 'string
  :group 'wakame-mode)


(defcustom wakame-mode-cache-file "~/.cache/wakame-mode.cache"
  "File path used for caching WakaTime heartbeats when offline."
  :type 'string
  :group 'wakame-mode)

(defvar wakame-mode-api-key
  "API Key populated from Auth-Source")

(defvar wakame-mode--heartbeats '()
  "List of heartbeats yet to be sent to wakatime.")

(defun wakame-mode--pretty-mode()
  "Return nice human-readable type of current file/buffer."
  (if (buffer-file-name)
      (if (listp mode-name)
          (car mode-name)
        mode-name)))

(defun wakame-mode--current-type()
  "Return whether current buffer is file backed or considered an app."
  (if (buffer-file-name)
      "file"
    "app"))

(defun wakame-mode--current-entity()
  "Return path of current entity or app name - i.e. major mode."
  (or (buffer-file-name)
      (symbol-name major-mode)))

(defun wakame-mode--create-heartbeat(savep)
  "Return a new heartbeat.

If SAVEP is non-nil record writing heartbeat"
  `(
    (type . ,(wakame-mode--current-type))
    (time . ,(float-time))
    (branch . ,(magit-get-current-branch))
    (project . ,(if (project-current) (project-name (project-current)) ""))
    (language . ,(wakame-mode--pretty-mode))
    (is_write . ,savep)
    ;; See ParseUserAgent at https://github.com/muety/wakapi/blob/master/utils/http.go
    (user_agent . ,(format "wakatime/unset (linux-unset) Emacs emacs-wakatime/1.0"))
    (entity . ,(wakame-mode--current-entity))))

(defun wakame-mode--send-heartbeat(heartbeat)
  "Sends heartbeat to configured API server."
  (let ((url-request-method "POST")
        (url-request-data (json-encode heartbeat))
        (url-request-extra-headers
         `(("Authorization" . ,(format "Basic %s" (base64-encode-string wakame-mode-api-key)))
           ("Content-Type" . "application/json")
           ("X-Machine-Name" . ,(system-name)))))
    (url-retrieve
     (format "%s/heartbeat" wakame-mode-api-url)
     (lambda(status)
       (if (plist-get status 'error)
           (message "Error posting heartbeat: %s" (plist-get status 'error))
         (delete heartbeat wakame-mode--heartbeats)))
     nil
     t
     t)))

(defun wakame-mode--save-heartbeat(heartbeat)
  (if (file-exists-p wakame-mode-cache-file)
      (with-temp-buffer
        (insert-file-contents wakame-mode-cache-file)
        (setq-local cache-data (read (current-buffer)))
        (add-to-list 'cache-data heartbeat)
        (delete-region (point-min) (point-max))
        (insert (prin1-to-string cache-data))
        (write-region (point-min) (point-max) wakame-mode-cache-file nil 'quiet))
    (with-temp-buffer
      (insert (prin1-to-string (cons heartbeat '())))
      (write-region (point-min) (point-max) wakame-mode-cache-file nil 'quiet))))

(defun wakame-mode--buffer-change(frame)
  (add-to-list 'wakame-mode--heartbeats (wakame-mode--create-heartbeat nil)))

(defun wakame-mode--buffer-change(frame)
  (add-to-list 'wakame-mode--heartbeats (wakame-mode--create-heartbeat nil)))

(add-to-list 'window-selection-change-functions 'wakame-mode--buffer-change)
(add-hook 'after-save-hook
          (lambda()
            (add-to-list 'wakame-mode--heartbeats (wakame-mode--create-heartbeat t)))
          nil t)

(provide 'wakame-mode)
;;; wakame-mode.el ends here
