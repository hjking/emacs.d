;;
;; Filename: cperl-conf.el
;; Description: Setting for cperl.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2010-12-16 14:39:27

(message "%d: >>>>> Loading [ Perl Mode ] Customizations ...." step_no)
(setq step_no (1+ step_no))

; (defun my-perl-startup ()
;   "Setup perl."
;   (interactive)
;   (local-set-key '[pause] 'perldb)
;   (setq gud-perldb-command-name "perl -w ") ; For warnings
;   (setq tab-width 4)
;   (setq indent-tabs-mode nil)  ; Autoconvert tabs to spaces
;   (setq cperl-indent-level 4)
;   (setq cperl-tab-always-indent nil) ; Indent if at left margin, else tab
;   (setq cperl-continued-statement-offset 2)
;   (setq cperl-continued-brace-offset -2)
;   (set-face-background 'cperl-array-face "wheat")
;   (set-face-background 'cperl-hash-face "wheat")
; )
; (add-hook 'perl-mode-hook 'my-perl-startup)

(eval-after-load "cperl-mode"
  '(progn
    (setq
     cperl-merge-trailing-else nil
     cperl-continued-statement-offset 0
     cperl-extra-newline-before-brace t)
      (local-set-key '[pause] 'perldb)
    (setq gud-perldb-command-name "perl -w ") ; For warnings
    (setq tab-width 4)
    (setq indent-tabs-mode nil)  ; Autoconvert tabs to spaces
    (setq cperl-indent-level 4)
    (setq cperl-tab-always-indent nil) ; Indent if at left margin, else tab
    (setq cperl-continued-statement-offset 2)
    (setq cperl-continued-brace-offset -2)
    (set-face-background 'cperl-array-face "wheat")
    (set-face-background 'cperl-hash-face "wheat")

    (defun installed-perl-version ()
      (interactive)
      (let ((perl (executable-find "perl")))
        (if perl
            (shell-command-to-string (concatenate 'string perl " -e '($v = $]) =~ s/(?<!\\.)(?=(\\d{3})+$)/./g; print $v;'")))))
    (defun use-installed-perl-version ()
      (interactive)
      (let ((perl-version (installed-perl-version)))
        (if perl-version
            (save-excursion
              (beginning-of-buffer)
              (let ((case-fold-search nil))
                (re-search-forward "^use [a-z]" (point-max) t)
                (beginning-of-line)
                (open-line 1)
                (insert (concatenate 'string "use v" perl-version ";"))))
            (message "Couldn't determine perl version"))))
    (add-hook 'cperl-mode-hook
      (lambda ()  (local-set-key (kbd "C-h f") 'cperl-perldoc)))
    (define-key 'help-command "P" 'cperl-perldoc)
    ))

(provide 'perl-conf)