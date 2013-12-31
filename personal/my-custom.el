;;; my-custom.el ---
;; Copyright © Hong Jin
;;
;; Filename: my-custom.el
;; Description:
;; Author: Hong Jin
;;           By: Jin hong
;; Created: Mon Feb 13 16:28:43 2012 (+0800)
;; Last-Updated: Tue Dec 31 15:34:05 2013 (+0800)
;; Version:
;;     Update #: 19
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
 '(org-agenda-files (quote ("~/org/gtd.org" "~/org/mygtd.org" "~/org/todo.org" "~/org/habit.org" "~/org/personal.org" "~/org/work.org" "~/org/meeting.org" "~/org/books.org" "~/org/call.org" "~/org/misc.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(completions-first-difference ((((class color) (background dark)) (:foreground "red"))))
 '(linum ((((background dark)) :foreground "cyan") (t :foreground "gray")))
 '(org-done ((t (:foreground "PaleGreen" :weight normal :strike-through t))))
 '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon" :strike-through t)))))
