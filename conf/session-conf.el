;; -*- lexical-binding: t -*-
;;

(require 'session)
(setq session-save-file (concat my-cache-dir "session"))
(add-to-list 'session-globals-exclude 'org-mark-ring)
(add-hook 'after-init-hook 'session-initialize)
