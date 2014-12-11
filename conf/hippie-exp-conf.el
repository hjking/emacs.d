
;; Filename: hippie-exp-conf.el
;; Description: Setting for hippie-exp.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2013-12-31 16:39:48
;;

;; [ hippie-expand ]------------------------------------------------------------
;; expand text trying various ways to find its expansion
;; Hippie Expand
(when (require 'hippie-exp nil t)
  (message "%d: >>>>> Loading [ hippie-exp ] Customizations ...." step_no)
  (setq step_no (1+ step_no))
  ;; I recommend you split the key binding of those two command.
  ;; I binding TAB yas/expand, and binding M-/ hippie-expand.
  ;; So yas/expand don't conflict with hippie/expand.

  ;; skipping space
  (setq hippie-expand-dabbrev-skip-space t)

  (autoload 'senator-try-expand-semantic "senator")

  ;; list of expansion functions tried (in order) by `hippie-expand'

  (setq hippie-expand-try-functions-list
      '(senator-try-expand-semantic
        try-complete-file-name-partially
        try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-list-all-buffers
        try-expand-line-all-buffers
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        try-expand-whole-kill
        ))

;; Full-line completion is *annoying*
(setq hippie-expand-try-functions-list
      (delq 'try-expand-line
      hippie-expand-try-functions-list))

  ;; expand-function
  (defun my-hippie-expand (arg)
    ;; called with a positive prefix `P', it jumps directly to the `P'-th
    ;; `try-function'
    (interactive "P")
    ;; `hippie-expand' does not have a customization-feature (like
    ;; `dabbrev-expand') to search case-sensitive for completions. So we
    ;; must set `case-fold-search' temporarily to nil!
    (let ((old-case-fold-search case-fold-search))
      (setq case-fold-search nil)
      (hippie-expand arg)
      (setq case-fold-search old-case-fold-search)))

)
;; [ hippie-expand ]----------------------------------------------------[ End ]--

(provide 'hippie-exp-conf)

;; EOF