;;
;; Filename: ace-jump-conf.el
;; Description: Setting for ace-jump-mode.el
;; Author: Hong Jin
;; Created: 2012-04-13 10:00
;; Last Updated: 2012-04-13 10:24:00
;;

(use-package ace-jump-mode
  :commands ace-jump-mode
  :init
  (bind-key "C-x SPC" 'ace-jump-mode))

(provide 'ace-jump-conf)

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
