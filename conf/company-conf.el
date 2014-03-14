
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

(require 'company)

;; (add-hook 'after-init-hook 'global-company-mode)

(setq company-require-match nil)
(setq company-begin-commands '(self-insert-command))
; (setq company-idle-delay 0.3)
(setq company-idle-delay t)
(setq company-tooltip-limit 20)
(setq company-minimum-prefix-length 2)
(setq company-echo-delay 0)
(setq company-auto-complete nil)
(global-company-mode 1)

(add-to-list 'company-backends 'company-cmake)
(add-to-list 'company-backends 'company-dabbrev t)
(add-to-list 'company-backends 'company-ispell t)
(add-to-list 'company-backends 'company-files t)
(add-to-list 'company-backends 'company-cider)

(global-set-key "\M-]" 'company-complete-common)