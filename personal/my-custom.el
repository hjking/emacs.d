;;; my-custom.el ---
;; Copyright © Hong Jin
;;
;; Filename: my-custom.el
;; Description:
;; Author: Hong Jin
;;           By: Hong Jin
;; Created: Mon Feb 13 16:28:43 2012 (+0800)
;; Last-Updated: Mon Aug 28 15:58:38 2017 (+0800)
;; Version:
;;     Update #: 93
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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((((background dark)) :foreground "cyan") (t :foreground "gray")))
 '(org-done ((t (:foreground "PaleGreen" :weight normal :strike-through t)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" default)))
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(org-agenda-files
   (quote
    ("~/org/personal.org" "~/org/work/ccs.org" "~/org/work/fabric.org" "~/org/work/fic.org" "~/org/work/fic_uml.org" "~/org/work/gpon.org" "~/org/work/hgu.org" "~/org/work/hgu_sim.org" "~/org/work/ref_pdf.org" "~/org/work/schedule.org" "~/org/work/uml_training.org" "~/org/topics/call.org" "~/org/topics/diary.org" "~/org/topics/habit.org" "~/org/topics/journal.org" "~/org/topics/listen-read-watch.org" "~/org/topics/notes.org")))
 '(package-selected-packages
   (quote
    (sublimity smart-hungry-delete files+ faces+ smart-compile smartparens multi-term buffer-move hideshowvis session w3m clippy header2 tabbar molokai-theme dracula-theme drag-stuff flx-ido zenburn-theme color-theme-solarized google-c-style ido-hacks ido-ubiquitous ido-vertical-mode find-file-in-project fiplr markdown-mode mmm-mode anzu rainbow-delimiters magit visual-regexp visual-regexp-steroids org-bullets popwin undo-tree multiple-cursors highlight-symbol volatile-highlights hl-anything which-key yasnippet browse-kill-ring+ mpg123 bongo graphviz-dot-mode ztree wttrin use-package stripe-buffer spaceline smooth-scrolling ranger popup persp-projectile paredit paradox origami org-trello org-toc neotree names monokai-theme miniedit ivy-hydra info+ indent-guide goto-last-change gotham-theme gnuplot-mode general f expand-region evil-nerd-commenter esup ecb discover-my-major dired-sort-menu+ dired-details+ dired+ deft csv-mode crux counsel company-c-headers color-theme-sanityinc-tomorrow calfw beacon async aggressive-indent ace-window)))
 '(paradox-github-token t t)
 '(session-use-package t nil (session))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#cc6666")
     (40 . "#de935f")
     (60 . "#f0c674")
     (80 . "#b5bd68")
     (100 . "#8abeb7")
     (120 . "#81a2be")
     (140 . "#b294bb")
     (160 . "#cc6666")
     (180 . "#de935f")
     (200 . "#f0c674")
     (220 . "#b5bd68")
     (240 . "#8abeb7")
     (260 . "#81a2be")
     (280 . "#b294bb")
     (300 . "#cc6666")
     (320 . "#de935f")
     (340 . "#f0c674")
     (360 . "#b5bd68"))))
 '(vc-annotate-very-old-color nil))
