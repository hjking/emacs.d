
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

  (add-to-list 'hippie-expand-try-functions-list 'senator-try-expand-semantic)
  (add-to-list 'hippie-expand-try-functions-list 'try-expand-dabbrev)   ; from current buffer
  (add-to-list 'hippie-expand-try-functions-list 'try-expand-dabbrev-visible)   ; from visible parts of all windows
  (add-to-list 'hippie-expand-try-functions-list 'try-expand-dabbrev-all-buffers)   ; from all other buffers
  (add-to-list 'hippie-expand-try-functions-list 'try-expand-dabbrev-from-kill)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-file-name-partially)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-file-name)
  (add-to-list 'hippie-expand-try-functions-list 'try-expand-all-abbrevs)
  (add-to-list 'hippie-expand-try-functions-list 'try-expand-list)
  (add-to-list 'hippie-expand-try-functions-list 'try-expand-list-all-buffers)
  (add-to-list 'hippie-expand-try-functions-list 'try-expand-line)
  (add-to-list 'hippie-expand-try-functions-list 'try-expand-line-all-buffers)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol-partially)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol)
  (add-to-list 'hippie-expand-try-functions-list 'try-expand-whole-kill)

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

;; EOF
;; [ hippie-expand ]----------------------------------------------------[ End ]--
