;;; waka-mytime-mode.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Stanislav Ochotnický
;;
;; Author: Stanislav Ochotnický <stanislav@ochotnicky.com>
;; Maintainer: Stanislav Ochotnický <stanislav@ochotnicky.com>
;; Created: April 17, 2023
;; Modified: April 17, 2023
;; Version: 0.0.1
;; Keywords: convenience data docs files languages lisp  tools
;; Homepage: https://github.com/sochotnicky/waka-mytime-mode
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

(defconst waka-mytime-version "0.0.1")
(defconst waka-mytime-user-agent "emacs-wakatime")


(defgroup waka-mytime nil
  "Customizations for WakaTime native mode."
  :group 'convenience
  :prefix "waka-mytime-")

(defcustom waka-mytime-api-url "https://wakapi.dev/api"
  "API URL for WakaTime."
  :type 'string
  :group 'waka-mytime)


(defcustom waka-mytime-cache-file "~/.cache/waka-mytime.cache"
  "File path used for caching WakaTime heartbeats when offline."
  :type 'string
  :group 'waka-mytime)

(defvar waka-mytime-api-key
  "API Key populated from Auth-Source")

(defvar waka-mytime--heartbeats '()
  "List of heartbeats yet to be sent to wakatime.")

(defun waka-mytime--pretty-mode()
  "Return nice human-readable type of current file/buffer."
  (if (buffer-file-name)
      (if (listp mode-name)
          (car mode-name)
        mode-name)))

(defun waka-mytime--current-type()
  "Return whether current buffer is file backed or considered an app."
  (if (buffer-file-name)
      "file"
    "app"))

(defun waka-mytime--current-entity()
  "Return path of current entity or app name - i.e. major mode."
  (or (buffer-file-name)
      (symbol-name major-mode)))

(defun waka-mytime--create-heartbeat(savep)
  "Return a new heartbeat.

If SAVEP is non-nil record writing heartbeat"
  `(
    (type . ,(waka-mytime--current-type))
    (time . ,(float-time))
    (branch . ,(magit-get-current-branch))
    (project . ,(if (project-current) (project-name (project-current)) ""))
    (language . ,(waka-mytime--pretty-mode))
    (is_write . ,savep)
    ;; See ParseUserAgent at https://github.com/muety/wakapi/blob/master/utils/http.go
    (user_agent . ,(format "wakatime/unset (linux-unset) Emacs emacs-wakatime/1.0"))
    (entity . ,(waka-mytime--current-entity))))

(defun waka-mytime--send-heartbeat(heartbeat)
  "Sends heartbeat to configured API server."
  (let ((url-request-method "POST")
        (url-request-data (json-encode heartbeat))
        (url-request-extra-headers
         `(("Authorization" . ,(format "Basic %s" (base64-encode-string waka-mytime-api-key)))
           ("Content-Type" . "application/json")
           ("X-Machine-Name" . ,(system-name)))))
    (url-retrieve
     (format "%s/heartbeat" waka-mytime-api-url)
     (lambda(status)
       (if (plist-get status 'error)
           (message "Error posting heartbeat: %s" (plist-get status 'error))
         (delete heartbeat waka-mytime--heartbeats)))
     nil
     t
     t)))

(defun waka-mytime--save-heartbeat(heartbeat)
  (if (file-exists-p waka-mytime-cache-file)
      (with-temp-buffer
        (insert-file-contents waka-mytime-cache-file)
        (setq-local cache-data (read (current-buffer)))
        (add-to-list 'cache-data heartbeat)
        (delete-region (point-min) (point-max))
        (insert (prin1-to-string cache-data))
        (write-region (point-min) (point-max) waka-mytime-cache-file nil 'quiet))
    (with-temp-buffer
      (insert (prin1-to-string (cons heartbeat '())))
      (write-region (point-min) (point-max) waka-mytime-cache-file nil 'quiet))))

(defun waka-mytime--buffer-change(frame)
  (add-to-list 'waka-mytime--heartbeats (waka-mytime--create-heartbeat nil)))

(defun waka-mytime--buffer-change(frame)
  (add-to-list 'waka-mytime--heartbeats (waka-mytime--create-heartbeat nil)))

(add-to-list 'window-selection-change-functions 'waka-mytime--buffer-change)
(add-hook 'after-save-hook
          (lambda()
            (add-to-list 'waka-mytime--heartbeats (waka-mytime--create-heartbeat t)))
          nil t)

(provide 'waka-mytime-mode)
;;; waka-mytime-mode.el ends here
