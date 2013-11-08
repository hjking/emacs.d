
;; Filename: auto-install-conf.el
;; Description: Setting for auto-install.el
;; Author: Hong Jin
;; Created: 2011-1-12 10:00
;; Last Updated: 2011-12-07 14:20:21
;;
(message "%d: >>>>> Loading [ auto-install ] Customizations ...." step_no)
(setq step_no (1+ step_no))

(require 'auto-install)
; (setq auto-install-directory "~/.emacs.d/auto-install/")
(setq auto-install-directory (concat my-emacs-dir "auto-install/"))

;; update the list of package names from Emacswiki when Emacs starts up
;;(auto-install-update-emacswiki-package-name t)

