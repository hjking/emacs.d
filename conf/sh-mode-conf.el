
;;; Shell Script mode
;; Major mode for editing Unix and GNU/Linux shell script code.
(use-package sh-mode
  :mode (("/etc/profile"                  . sh-mode)
         ("/etc/bash_completion"          . sh-mode)
         ("\\.SH"                         . sh-mode)
         ("\\.sh\\'"                      . sh-mode)
         ("\\.csh\\'"                     . sh-mode)
         ("\\.zsh\\'"                     . sh-mode)
         ("\\rc\\'"                       . sh-mode)
         ("\\.[ck]?sh\\'\\|\\.shar\\'\\|/\\.z?profile\\'" . sh-mode)
         ("\\(/\\|\\`\\)\\.\\(bash_profile\\|z?login\\|bash_login\\|z?logout\\)\\'" . sh-mode)
         ("\\.\\(sh\\|csh\\|bash\\|zsh\\|SH\\)\\'" . sh-mode)
         ("\\(/\\|\\`\\)\\.\\(bash_logout\\|[kz]shrc\\|bashrc\\|t?cshrc\\|esrc\\)\\'" . sh-mode)
         ("\\(/\\|\\`\\)\\.\\([kz]shenv\\|xinitrc\\|startxrc\\|xsession\\)\\'" . sh-mode)
         ("\\.bash_history\\'"            . sh-mode)
         ("\\.bash\\'"                    . sh-mode)
         ("\\.bashrc.local\\'"            . sh-mode))
  :init
   (progn
    (setq sh-basic-offset '8)
    (setq sh-indentation '8)
    (setq sh-indent-comment t)
    (setq indent-tabs-mode t)
    (setq sh-indent-supported '((sh . sh)
                                (rc . rc)
                                (csh . csh)
                                (tcsh . csh)))
    (setq sh-indent-for-case-label '0)
    (setq sh-indent-for-case-alt '+)
    ))
