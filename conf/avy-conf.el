;;
;; Filename: avy-conf.el
;; Description: avy allows us to effectively navigate to visible things
;; Author: Hong Jin
;; Last Updated: 2015-07-23 10:00
;;

(use-package avy
  ; :bind ("M-SPC" . avy-goto-char)
  :config (progn
            (setq avy-background t)
            (setq avy-style 'at-full)
            (setq avy-all-windows nil)))

(provide 'avy-conf)