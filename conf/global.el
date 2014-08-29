
;; don't truncate the message log buffer when it becomes large
(setq message-log-max t)

;; use decimal for `C-q'
(setq read-quoted-char-radix 10)

(setq minibuffer-max-depth nil)        ; max depth of mini-buffer no limit.

;; System locale for time
(setq system-time-locale "C")

;; make Gnus fast
(setq gc-cons-threshold 3500000)
;; don't display messages at start and end of garbage collection (as it hides
;; too many interesting messages)
(setq garbage-collection-messages nil)

;; display what I'm typing *immediately*
(setq echo-keystrokes 0.1)

;; visually indicate buffer boundaries and scrolling
(setq indicate-buffer-boundaries t)

;; visually indicate empty lines after the buffer end
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; Shut off warning messages when using system shell
(setq w32-allow-system-shell t)

;;;; Alarm
;; no visible bell ring
(setq visible-bell nil)

;; no ring and screen flash
(setq ring-bell-function 'ignore)

;; no beep or flash anymore, when hit ‘C-g’ in the minibuffer or during an ‘isearch’
;; (setq ring-bell-function
;;   (lambda ()
;;     (unless (memq this-command
;;         '(isearch-abort abort-recursive-edit exit-minibuffer keyboard-quit))
;;         (ding))))


;; yes/no ==> y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Remove useless whitespace before saving a file
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; highlight trailing whitespaces in all modes
(setq show-trailing-whitespace t)

;; after 1 second typed M-x CMD, display CMD binding key
(setq suggest-key-bindings 1)

;; confirm before quit emacs
(setq confirm-kill-emacs 'yes-or-no-p)

;; Indent before TAB
(setq tab-always-indent 'complete)

(setq backup-by-copying-when-linked t) ; When making backups of link files.

(setq confirm-nonexistent-file-or-buffer t)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(global-hi-lock-mode 1)

;; Echo commands I haven’t finished quicker than the default of 1 second
(setq echo-keystrokes 0.1)

;; warn me if the file is larger than 25Mb
(setq large-file-warning-threshold (* 25 1024 1024))

;; Hide the mouse while typing
(setq make-pointer-invisible t)

;; It’s okay to refer to a file by a symlink
(setq-default find-file-visit-truename nil)

;; Single space still ends a sentence
(setq sentence-end-double-space nil)

;; Single dash starts a paragraph
(setq paragraph-start "- \\|\f\\|[ \t]*$"
      paragraph-separate "[\f\t ]*$")

;; activate whitespace-mode to view all whitespace characters
(whitespace-mode t)

;; highlight current line
(require 'hl-line)
(global-hl-line-mode)

(setq comment-style 'extra-line
      comment-auto-fill-only-comments t)