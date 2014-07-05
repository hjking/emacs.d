
;;;; Personal Setting

;; Set user name
(setq user-full-name "Hong Jin")
;; Set email address
(setq user-mail-address "hon9jin@gmail.com")
;;; Used in ChangeLog entries
(setq add-log-mailing-address "hon9jin@gmail.com")

; (setq user-mail-address (when (file-exists-p "~/.email")
;                      (with-temp-buffer
;                        (insert-file-contents "~/.email")
;                        (goto-char (point-min))
;                        (buffer-substring-no-properties
;                         (point) (point-at-eol)))))