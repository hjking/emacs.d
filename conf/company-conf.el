
;; Use M-n, M-p, <tab> and <tab> to complete.
;; Search through the completions with C-s, C-r and C-o.

;; Once installed, enable company-mode with M-x company-mode.
;; Use M-n, M-p, <tab> and <return> to complete.
;; Search through the completions with C-s, C-r and C-o.
;; Type `M-x company-complete` to initiate completion manually.
;; Bind this command to a key combination of your choice.
;; When the completion candidates are shown, press <f1> to display the documentation for the selected candidate, or C-w to see its source. Not all back-ends support this.
;; To use company-mode in all buffers, add the following line to your init file:
;; (add-hook 'after-init-hook 'global-company-mode)
;;
;; To see or change the list of enabled back-ends, type M-x customize-variable RET company-backends. Also see its description for information on writing a back-end.
;; For information on specific back-ends, also check out the comments inside the respective files.
;; For more information, type M-x describe-function RET company-mode.
;; To customize other aspects of its behavior, type M-x customize-group RET company.

; (require 'company)

(add-hook 'prog-mode-hook 'global-company-mode)
;; (add-hook 'after-init-hook 'global-company-mode)
(global-set-key "\M-]" 'company-complete-common)

(setq company-require-match nil)

(eval-after-load 'company
  '(progn
     (add-to-list 'company-backends 'company-dabbrev t)
     (add-to-list 'company-backends 'company-ispell t)
     (add-to-list 'company-backends 'company-files t)
     (add-to-list 'company-backends 'company-semantic t)
     ; (add-to-list 'company-backends 'company-cider)
     ;; company-c-headers
     (add-to-list 'company-backends 'company-c-headers)
     ;; can't work with TRAMP
     (delete 'company-ropemacs company-backends)
     (delete 'company-capf company-backends)
     (delete 'company-clang company-backends)
     (delete 'company-semantic company-backends)
     ;; I don't like the downcase word in company-dabbrev
     ;; for languages use camel case naming convention
     (setq company-dabbrev-downcase nil)
     (setq company-show-numbers t)
     (setq company-begin-commands '(self-insert-command))
     (setq company-idle-delay 0.2)
     ; (setq company-idle-delay t)
     ; (setq company-clang-insert-arguments nil)

     ; (setq company-idle-delay 0.3)
     (setq company-tooltip-limit 20)
     (setq company-minimum-prefix-length 2)
     (setq company-echo-delay 0)
     (setq company-auto-complete nil)
     ))

(provide 'company-conf)