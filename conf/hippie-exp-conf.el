
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
  :defer t
  :ensure nil ; built-in package
  :commands (hippie-expand)
  :bind ("M-/" . hippie-expand)
  :init (progn
      (setq hippie-expand-dabbrev-skip-space t)
      ;; list of expansion functions tried (in order) by `hippie-expand'
      (setq hippie-expand-try-functions-list
          '(
            ;; Try to expand word "dynamically", searching the current buffer.
            try-expand-dabbrev
            try-expand-dabbrev-visible
            ;; Try to expand word "dynamically", searching all other buffers.
            try-expand-dabbrev-all-buffers
            ;; Try to expand word "dynamically", searching the kill ring.
            try-expand-dabbrev-from-kill
            ;; Try to complete text as a file name, as many characters as unique.
            try-complete-file-name-partially
            ;; Try to complete text as a file name.
            try-complete-file-name
            ;; Try to expand word before point according to all abbrev tables.
            try-expand-all-abbrevs
            ;; Try to complete the current line to an entire line in the buffer.
            try-expand-list
            ;; Try to complete the current line to an entire line in the buffer.
            try-expand-line
            ; try-expand-list-all-buffers
            ; try-expand-line-all-buffers
            ;; Try to complete as an Emacs Lisp symbol, as many characters as unique.
            try-complete-lisp-symbol-partially
            ;; Try to complete word as an Emacs Lisp symbol.
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