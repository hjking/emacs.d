;; -*- lexical-binding: t -*-
;;

(defun locale-is-utf8-p ()
  "Return t if the \"locale\" command or environment variables prefer UTF-8."
  (cl-flet ((is-utf8 (v) (and v (string-match "UTF-8" v))))
    (or (is-utf8 (and (executable-find "locale") (shell-command-to-string "locale")))
        (is-utf8 (getenv "LC_ALL"))
        (is-utf8 (getenv "LC_CTYPE"))
        (is-utf8 (getenv "LANG")))))

(setq utf-translate-cjk-mode nil) ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
(set-clipboard-coding-system   'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-language-environment      'utf-8)
(set-keyboard-coding-system    'utf-8)
(set-default-coding-systems    'utf-8)
(set-terminal-coding-system    'utf-8)
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))
; (modify-coding-system-alist 'process "*" 'utf-8)

; (setq default-process-coding-system '(euc-cn . euc-cn))
(setq default-buffer-file-coding-system 'utf-8)
(setq-default pathname-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq file-name-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(prefer-coding-system 'utf-8)

(provide 'encoding-conf)