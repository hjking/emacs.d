
(require 'helm-config)
;; (helm-mode 1)
(setq helm-input-idle-delay 0.1) ;; I want it now!
(setq helm-candidate-number-limit 10)
(helm-mode 1)

(global-set-key (kbd "<print>") 'helm-mini)
(define-key global-map [remap find-file] 'helm-find-files)
(define-key global-map [remap occur] 'helm-occur)
;; (define-key global-map [remap list-buffers] 'helm-buffers-list)
(define-key lisp-interaction-mode-map [remap indent-for-tab-command] 'helm-lisp-completion-at-point-or-indent)
(define-key emacs-lisp-mode-map       [remap indent-for-tab-command] 'helm-lisp-completion-at-point-or-indent)
;; (global-set-key (kbd "C-c h") 'helm-mini)
