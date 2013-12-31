;;; my-custom.el ---
;; Copyright © Hong Jin
;;
;; Filename: my-custom.el
;; Description:
;; Author: Hong Jin
;;           By: Jin hong
;; Created: Mon Feb 13 16:28:43 2012 (+0800)
;; Last-Updated: Tue Dec 31 08:47:48 2013 (+0800)
;; Version:
;;     Update #: 17
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
 '(cscope-database-file "cscope.out")
 '(cscope-filename-chars "-.,/A-Za-z0-9_~!@#$%&+=\\\\")
 '(cscope-index-file "cscope.files")
 '(cscope-indexing-script "cscope-indexer")
 '(cscope-name-line-width -30)
 '(cscope-overlay-arrow-string "=>")
 '(cscope-program "cscope")
 '(cscope-suppress-empty-matches t)
 '(cscope-symbol-chars "A-Za-z0-9_")
 '(custom-safe-themes (quote ("bf9d5728e674bde6a112979bd830cc90327850aaaf2e6f3cc4654f077146b406" default)))
 '(display-time-mode t)
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ido-buffer-history t t)
 '(ido-case-fold t)
 '(ido-confirm-unique-completion nil)
 '(ido-enable-last-directory-history t)
 '(ido-enable-tramp-completion nil)
 '(ido-max-prospects 7)
 '(ido-max-work-directory-list 100)
 '(ido-max-work-file-list 50)
 '(ido-use-filename-at-point nil)
 '(ido-use-url-at-point nil)
 '(semanticdb-default-file-name "semantic.cache")
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#002b36" :foreground "#839496" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 105 :width normal :foundry "outline" :family "Consolas"))))
 '(completions-first-difference ((((class color) (background dark)) (:foreground "red"))))
 '(linum ((((background dark)) :foreground "cyan") (t :foreground "gray")))
 '(org-done ((t (:foreground "PaleGreen" :weight normal :strike-through t))))
 '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon" :strike-through t)))))
