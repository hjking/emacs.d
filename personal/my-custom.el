;;; my-custom.el ---
;; Copyright © Hong Jin
;;
;; Filename: my-custom.el
;; Description:
;; Author: Hong Jin
;;           By: Hong Jin
;; Created: Mon Feb 13 16:28:43 2012 (+0800)
;; Last-Updated: Wed Jun 22 12:05:35 2016 (+0800)
;; Version:
;;     Update #: 35
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; my-custom.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(org-agenda-files
   (quote
    ("~/org/personal.org" "~/org/work/ccs.org" "~/org/work/fabric.org" "~/org/work/fic.org" "~/org/work/fic_uml.org" "~/org/work/ref_pdf.org" "~/org/work/schedule.org" "~/org/work/uml_training.org")))
 '(paradox-github-token t)
 '(projectile-enable-idle-timer t)
 '(session-use-package t nil (session))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((((background dark)) :foreground "cyan") (t :foreground "gray")))
 '(org-done ((t (:foreground "PaleGreen" :weight normal :strike-through t))))
 '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon" :strike-through t)))))
