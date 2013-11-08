;;
;; Filename: multi-term-conf.el
;; Description: Setting for multi-term.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2011-12-07 14:27:28

;; multi-term setting
;; available for Emacs 23
;;
(message "%d: >>>>> Loading [ mutil-term ] Customizations ...." step_no)
(setq step_no (1+ step_no))
(require 'multi-term)
;; (setq multi-term-program "/bin/csh")
(setq multi-term-switch-after-close nil)
(defun term-mode-settings ()
  "Settings for term-mode"
  ; (make-local-variable 'scroll-margin)
  (setq-default scroll-margin 0)
)
(add-hook 'term-mode-hook 'term-mode-settings)
(global-set-key "\M-t"              'multi-term)
; (global-set-key (kbd "C-x C-m") 'multi-term)
; (global-set-key (kbd "C-x m") 'multi-term-next)

;; Likewise, yasnippet breaks the tab key.
(add-hook 'term-mode-hook (lambda() (yas-minor-mode -1)))

;; Don't try to enable autopair in term-mode, it remaps the return key!
(add-hook 'term-mode-hook (lambda () (autopair-mode 0)))
