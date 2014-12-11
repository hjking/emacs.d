;;
;; Filename: column-marker-conf.el
;; Description: Setting for column-marker.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2010-12-22 14:34:13

(message "%d: >>>>> Loading [ column-marker ] Customizations ...." step_no)
(setq step_no (1+ step_no))

(require 'column-marker)
(dolist (hook '(emacs-lisp-mode-hook
                perl-mode-hook
                python-mode-hok
                shell-mode-hook
                text-mode-hook
                change-log-mode-hook
                makefile-mode-hook
                message-mode-hook
                verilog-mode-hook
                vlog-mode-hook
                texinfo-mode-hook))
  (add-hook hook (lambda ()
                   (interactive)
                   (column-marker-1 80)
                   (column-marker-2 90)
                   (column-marker-3 100))))

;; use `C-c m' interactively to highlight with `column-marker-1-face'
;;  (global-set-key (kbd "C-c m") 'column-marker-1)

(provide 'column-marker-conf)

;; EOF
