
;; Filename: hippie-exp-conf.el
;; Description: Setting for hippie-exp.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2013-12-31 16:39:48
;;

;; [ hippie-expand ]------------------------------------------------------------
;; expand text trying various ways to find its expansion
;; Hippie Expand
(use-package hippie-exp
  ;; I recommend you split the key binding of those two command.
  ;; I binding TAB yas/expand, and binding M-/ hippie-expand.
  ;; So yas/expand don't conflict with hippie/expand.

  :commands (hippie-expand)
  :bind ("M-;" . hippie-expand)
  :init (progn
      (setq hippie-expand-dabbrev-skip-space t)

      ;; list of expansion functions tried (in order) by `hippie-expand'
      (setq hippie-expand-try-functions-list
          '(try-complete-file-name-partially
            try-expand-dabbrev
            try-expand-dabbrev-visible
            try-expand-dabbrev-all-buffers
            try-expand-dabbrev-from-kill
            try-complete-file-name-partially
            try-complete-file-name
            try-expand-all-abbrevs
            try-expand-list
            try-expand-line
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
  )
)
;; [ hippie-expand ]----------------------------------------------------[ End ]--

(provide 'hippie-exp-conf)

;; EOF