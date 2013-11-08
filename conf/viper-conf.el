
;; Filename: viper-conf.el
;; Description: Setting for viper.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2010-12-14 14:27:07
;;
(message "%d: >>>>> Loading [ vi mode ] Customizations ...." step_no)
(setq step_no (1+ step_no))

;; Enable Viper
(setq viper-mode t)
(require 'viper)
;; (add-hook 'emacs-startup-hook 'viper-mode)
;; (setq viper-toggle-key (kbd "M-a"))
