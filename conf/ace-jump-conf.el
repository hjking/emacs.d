;;
;; Filename: ace-jump-conf.el
;; Description: Setting for ace-jump-mode.el
;; Author: Hong Jin
;; Created: 2012-04-13 10:00
;; Last Updated: 2012-04-13 10:24:00
;;

(require 'ace-jump-mode)
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode)
;; echo before jump
(add-hook 'ace-jump-mode-before-jump-hook (lambda ()
                                            (message "I am jumping")))
; (eval-after-load "ace-jump-mode"
;   '(ace-jump-mode-enable-mark-sync))

;; use package
;;(use-package ace-jump-mode
;;  :bind ("C-x SPC" . ace-jump-mode))

;;If you also use viper mode :
;; (define-key viper-vi-global-user-map (kbd "SPC") 'ace-jump-mode)
;;If you use evil
;; (define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)

;; "C-c SPC" ==> ace-jump-word-mode
;; enter first character of a word, select the highlighted key to move to it.
;;
;; "C-u C-c SPC" ==> ace-jump-char-mode
;; enter a character for query, select the highlighted key to move to it.
;;
;; "C-u C-u C-c SPC" ==> ace-jump-line-mode
;; each non-empty line will be marked, select the highlighted key to move to it.
