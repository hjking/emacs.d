;;; my-custom.el ---
;; Copyright © Hong Jin
;;
;; Filename: my-custom.el
;; Description:
;; Author: Hong Jin
;;           By: Hong Jin
;; Created: Mon Feb 13 16:28:43 2012 (+0800)
;; Last-Updated: Tue Sep 27 09:04:03 2016 (+0800)
;; Version:
;;     Update #: 38
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
 '(package-selected-packages
   (quote
    (gotham-theme monokai-theme calfw ztree use-package stripe-buffer smooth-scrolling ranger popup persp-projectile paredit paradox org-trello org-toc names miniedit info+ indent-guide guide-key-tip gnuplot-mode general f expand-region evil-nerd-commenter esup ecb discover-my-major dired-sort-menu+ dired-details+ dired+ csv-mode counsel company-c-headers beacon async aggressive-indent ace-window)))
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
