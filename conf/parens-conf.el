;;
(message "%d: >>>>> Loading [ Parentheses ] Customizations ...." step_no)
(setq step_no (1+ step_no))

;; auto-insert/close bracket pairs
(electric-pair-mode 1)

;; smartparens
;;
(require 'smartparens-config)
(smartparens-global-mode t)

;; highlights matching pairs
(show-smartparens-global-mode t)

(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)

(sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

;;; markdown-mode
(sp-with-modes '(markdown-mode gfm-mode rst-mode)
  (sp-local-pair "*" "*" :bind "C-*")
  (sp-local-tag "2" "**" "**")
  (sp-local-tag "s" "```scheme" "```")
  (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

;;; lisp modes
(sp-with-modes sp--lisp-modes
  (sp-local-pair "(" nil :bind "C-("))

;;
;; rainbow-delimiters
;;
;; highlights parens, brackets, and braces according to their depth
(add-site-lisp-load-path "rainbow-delimiters/")
(require 'rainbow-delimiters)
;; enable in all programming-related modes (Emacs 24+)
;; (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;; use Emacs-wide
(global-rainbow-delimiters-mode)

;; show paren
; (show-paren-mode 1) ; turn on paren match highlighting
; (setq show-paren-style 'parentheses)
; (setq show-paren-style 'expression) ; highlight entire bracket expression
; (setq show-paren-delay 0)        ; show matching paren immediately



; ;; Check parens
; ; turn on check-parens on a save
; ; it will not let you save until you correct the error
; (add-hook 'emacs-lisp-mode-hook
;   (function (lambda () (add-hook 'local-write-file-hooks 'check-parens))))
; (add-hook 'c-mode-hook
;   (function (lambda () (add-hook 'local-write-file-hooks 'check-parens))))
; (add-hook 'verilog-mode-hook
;   (function (lambda () (add-hook 'local-write-file-hooks 'check-parens))))


; ;; SkeletonMode
; (setq skeleton-pair t)
; (setq skeleton-autowrap nil)
; ;; (setq skeleton-pair-alist  '(
; ;;     (?\(? _ ")")
; ;;     (?\[? _ "]")
; ;;     (?\{? _ "}")))
; ;; Auto complete parentheses
; (global-set-key (kbd "(")   'skeleton-pair-insert-maybe)
; (global-set-key (kbd "{")   'skeleton-pair-insert-maybe)
; (global-set-key (kbd "\"")  'skeleton-pair-insert-maybe)
; (global-set-key (kbd "[")   'skeleton-pair-insert-maybe)
; (global-set-key (kbd "<")   'skeleton-pair-insert-maybe)


;; [ highlight-parentheses ]----------------------------------------------------
;;  (message "%d: >>>>> Loading [ highlight-parentheses ] Customizations ...." step_no)
;;  (setq step_no (1+ step_no))
;;  (require 'highlight-parentheses)
;;  (setq hl-paren-colors '("red" "yellow" "cyan" "magenta" "green" "red"))
;;  (dolist (hook (list 'hs-hide-hook
;;                      'man-mode-hook
;;                      'gdb-mode-hook
;;                      'info-mode-hook
;;                      'find-file-hook
;;                      'help-mode-hook
;;                      'dired-mode-hook
;;                      'custom-mode-hook
;;                      'apropos-mode-hook
;;                      'log-view-mode-hook
;;                      'compilation-mode-hook
;;                      'svn-log-edit-mode-hook
;;                      'package-menu-mode-hook
;;                      'inferior-ruby-mode-hook
;;                      'completion-list-mode-hook
;;                      'lisp-interaction-mode-hook
;;                      'browse-kill-ring-mode-hook
;;                )
;;          )
;;    (add-hook hook
;;              (lambda()
;;                (highlight-parentheses-mode t)) t)
;;  (highlight-parentheses-mode 1)
;;  )
;; [ highlight-parentheses ]-------------------------------------------[ End ]--
