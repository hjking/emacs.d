;;
;; Filename: cperl-conf.el
;; Description: Setting for cperl.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2010-12-16 14:39:27

(use-package cperl-mode
  :mode (("\\.\\([pP][Llm]\\|al\\)\\'"    . cperl-mode)
         ("\\.pm$"                        . cperl-mode)
         ("\\.t$"                         . cperl-mode)
         ("\\.psgi$"                      . cperl-mode)
         ("\\.comp$"                      . cperl-mode)
         ("\\.perl\\'"                    . cperl-mode))
  :interpreter (("perl"     . cperl-mode)
                ("perl5"    . cperl-mode)
                ("miniperl" . cperl-mode))
  :init (progn
          (defalias 'perl-mode 'cperl-mode)
          (setq cperl-autoindent-on-semi t
                cperl-auto-newline t
                cperl-clobber-lisp-bindings t
                cperl-close-paren-offset -2
                cperl-continued-statement-offset 2
                cperl-electric-keywords t
                cperl-electric-lbrace-space nil
                cperl-electric-linefeed t
                cperl-electric-parens nil
                cperl-font-lock t
                cperl-highlight-variables-indiscriminately t
                cperl-indent-level 4
                cperl-indent-parens-as-block t
                cperl-merge-trailing-else nil
                cperl-extra-newline-before-brace t
                cperl-indent-region-fix-constructs nil
                cperl-info-on-command-no-prompt t
                cperl-invalid-face nil
                cperl-lazy-help-time 5
                cperl-continued-brace-offset -2
                cperl-tab-always-indent t)))


; (add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
; (add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
; (add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

(provide 'perl-conf)