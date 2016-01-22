
;; Filename: template-conf.el
;; Description: Setting for template.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2011-12-08 15:43:54
;;

(use-package template
  :init (progn
    (setq template-default-directories (concat my-emacs-dir "templates/"))
    )
  :config (progn
    (template-initialize)
    ; (setq template-default-directories  "~/emacs.d/templates/")

    (define-auto-insert 'elisp-mode     "elisp.tpl")
    (define-auto-insert 'verilog-mode   "verilog.tpl")
    (define-auto-insert 'python-mode    "python.tpl")
    (define-auto-insert 'perl-mode      "perl.tpl")
    (define-auto-insert 'sh-mode        '(nil "#!/bin/bash\n\n"))

    (add-hook 'find-file-hooks 'template-expand-template)
    )
  )