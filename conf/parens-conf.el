;; -*- lexical-binding: t -*-
;;

(setq show-paren-delay 0) ; show matching paren immediately
(show-paren-mode 1) ; turn on paren match highlighting

;; Minor mode for Emacs that deals with parens
;; pairs and tries to be smart about it
;; https://github.com/Fuco1/smartparens
(use-package smartparens
  ; :disabled t
  :ensure t
  :diminish smartparens-mode
  :diminish smartparens-strict-mode
  :config
   (require 'smartparens-config)
   ; (smartparens-global-mode)
   ; (smartparens-global-strict-mode)
   ; (show-smartparens-global-mode)
   (add-hook 'prog-mode-hook #'smartparens-strict-mode)
)

;;
;; Emacs rainbow delimiters mode
;; https://github.com/Fanael/rainbow-delimiters
;;
;; highlights parens, brackets, and braces according to their depth
;; enable in all programming-related modes (Emacs 24+)
(use-package rainbow-delimiters
  :ensure t
  :commands (rainbow-delimiters-mode)
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))
;; use Emacs-wide
;; (global-rainbow-delimiters-mode)

;; Check parens
; turn on check-parens on a save
; it will not let you save until you correct the error
(add-hook 'emacs-lisp-mode-hook
  (function (lambda () (add-hook 'local-write-file-hooks 'check-parens))))
(add-hook 'c-mode-hook
  (function (lambda () (add-hook 'local-write-file-hooks 'check-parens))))
(add-hook 'verilog-mode-hook
  (function (lambda () (add-hook 'local-write-file-hooks 'check-parens))))

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

(provide 'parens-conf)