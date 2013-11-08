
;; Filename: template-conf.el
;; Description: Setting for template.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2011-12-08 15:43:54
;;

(message "%d: >>>>> Loading [ template ] Customizations ...." step_no)
(setq step_no (1+ step_no))
(require 'template nil t)
(when (featurep 'template)
  (template-initialize)
  ; (setq template-default-directories  "~/emacs.d/templates/")
  (setq template-default-directories (concat my-emacs-dir "templates/"))
  (define-auto-insert 'elisp-mode     "elisp.tpl")
  (define-auto-insert 'verilog-mode   "verilog.tpl")
  (define-auto-insert 'python-mode    "python.tpl")
  (define-auto-insert 'perl-mode      "perl.tpl")
  (define-auto-insert 'sh-mode        '(nil "#!/bin/bash\n\n"))

  (add-hook 'find-file-hooks 'template-expand-template)
)
