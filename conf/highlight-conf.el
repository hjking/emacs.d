
(message "%d: >>>>> Loading [ highlight ] Customization ...." step_no)
(setq step_no (1+ step_no))

;; Highlight Symbol at point/cursor
; (add-site-lisp-load-path "highlight-symbol")
; (eval-after-load "highlight-symbol"
;     '(progn
;       (highlight-symbol-mode 1)
;       (global-set-key [(control f3)] 'highlight-symbol-at-point)
;       (global-set-key [f3]           'highlight-symbol-next)
;       (global-set-key [(shift f3)]   'highlight-symbol-prev)
;       (global-set-key [(meta f3)]    'highlight-symbol-query-replace)
;       ;;(global-set-key [(shift f3)]    'highlight-symbol-prev)
;       ))


;; Highlight Global
;; Source: https://github.com/glen-dai/highlight-global
(add-site-lisp-load-path "highlight-global")
(require 'highlight-global)
