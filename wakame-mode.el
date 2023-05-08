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
(require 'auth-source)
(require 'url-parse)

(defconst wakame-mode-version "0.0.1")
(defconst wakame-mode-user-agent "emacs-wakatime")


(defgroup wakame nil
  "Customizations for WakaTime native mode."
  :group 'convenience
  :prefix "wakame")

(defcustom wakame-api-url "https://wakapi.dev/api"
  "API URL for WakaTime."
  :type 'string
  :group 'wakame)

(defcustom wakame-cache-file (file-name-concat (xdg-cache-home) "wakame-mode.cache")
  "File path used for caching WakaTime heartbeats when offline."
  :type 'string
  :group 'wakame)

(defvar wakame--heartbeats '()
  "List of heartbeats yet to be sent to wakatime.")

(defvar wakame--idle-timer nil
  "Idle timer for posting heartbeats")

(defun wakame--pretty-mode()
  "Return nice human-readable type of current file/buffer."
  (if (buffer-file-name)
      (if (listp mode-name)
          (car mode-name)
        mode-name)))

(defvar wakame--last-heartbeat-entity nil
  "Value of last entity that was added to the heartbeats.")

(defvar wakame--last-heartbeat-time nil
  "Time of of last heartbeat was added.")

(defun wakame--current-type()
  "Return whether current buffer is file backed or considered an app."
  (if (buffer-file-name)
      "file"
    "app"))

(defun wakame--current-entity()
  "Return path of current entity or app name - i.e. major mode."
  (or (buffer-file-name)
      (symbol-name major-mode)))

(defun wakame--create-heartbeat(savep)
  "Return a new heartbeat.

If SAVEP is non-nil record writing heartbeat"
  `(
    (type . ,(wakame--current-type))
    (time . ,(float-time))
    (branch . ,(magit-get-current-branch))
    (project . ,(if (project-current) (project-name (project-current)) ""))
    (language . ,(wakame--pretty-mode))
    (is_write . ,savep)
    ;; See ParseUserAgent at https://github.com/muety/wakapi/blob/master/utils/http.go
    (user_agent . ,(format "wakatime/unset (linux-unset) Emacs emacs-wakatime/1.0"))
    (entity . ,(wakame--current-entity))))

(defun wakame--send-all-heartbeats ()
  "Sends next batch of 25 heartbeats using bulk API endpoint."
  (if-let* ((url-request-method "POST")
            (current-heartbeats (last wakame--heartbeats 25))
            (url-request-data (json-encode current-heartbeats))
            (secret (car (auth-source-search :host (url-host (url-generic-parse-url wakame-api-url))
                                             :user "wakame" :max 1)))
            (url-request-extra-headers
             `(("Authorization" . ,(format "Basic %s" (base64-encode-string (funcall (plist-get secret :secret)))))
               ("Content-Type" . "application/json")
               ("X-Machine-Name" . ,(system-name)))))
      (progn
        (url-retrieve
         (format "%s/heartbeats" wakame-api-url)
         (lambda(status cbargs)
           (if (plist-member status :error)
               (error "Error posting heartbeats: %s" (plist-member status :error))
             (progn
               (setq wakame--heartbeats (butlast wakame--heartbeats 25))
               ;; TODO: Fix ugly recursive call
               (run-with-timer 1 nil #'wakame--send-all-heartbeats))))
         (list url-request-data)
         t
         t))))

(defun wakame--add-heartbeat(heartbeat)
  "Add HEARTBEAT to heartbeats and track last addition.

Only adds heartbeat if same entity (file/app) has not been added
to the heartbeat last in past 2 minutes"
  (let ((entity (alist-get 'entity heartbeat))
        (timediff (- (float-time) (or wakame--last-heartbeat-time 120))))
    (if (or (not (equal wakame--last-heartbeat-entity entity))
            (> timediff 120))
        (progn
          (setq wakame--last-heartbeat-time (alist-get 'time heartbeat))
          (setq wakame--last-heartbeat-entity (alist-get 'entity heartbeat))
          (add-to-list 'wakame--heartbeats heartbeat)))))

(defun wakame--handle-save()
  "Handler for buffer save."
  (wakame--add-heartbeat (wakame--create-heartbeat t)))

(defun wakame--buffer-change(&optional frame)
  "Handler for buffer focus change."
  (wakame--add-heartbeat (wakame--create-heartbeat nil)))

(defun wakame-mode--enable()
  "Add hooks to enable wakame tracking."
  (setq wakame--idle-timer (run-with-idle-timer 10 t #'wakame--send-all-heartbeats))
  (add-to-list 'window-selection-change-functions #'wakame--buffer-change)
  (add-hook 'first-change-hook 'wakame--buffer-change nil t)
  (add-hook 'after-save-hook #'wakame--handle-save nil t))

(defun wakame-mode--disable()
  "Remove hooks and disable wakame tracking."
  (if (timerp wakame--idle-timer) (cancel-timer wakame--idle-timer))
  (setq window-selection-change-functions (delete #'wakame--buffer-change window-selection-change-functions))
  (remove-hook 'after-save-hook #'wakame--handle-save t)
  (remove-hook 'first-change-hook #'wakame--buffer-change t))

;;;###autoload
(define-minor-mode wakame-mode
  "Toggle WakaMeMode (WakaTime Native mode)."
  :init-value nil
  :global     t
  :group      'wakame
  (cond
    (noninteractive (setq wakame-mode nil))
    (wakame-mode (wakame-mode--enable))
    (t (wakame-mode--disable))))

(provide 'wakame-mode)
;;; wakame-mode.el ends here
