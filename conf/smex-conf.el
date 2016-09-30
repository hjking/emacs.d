
(use-package smex
  :disabled t
  :load-path :load-path (lambda () (concat my-site-lisp-dir "smex/"))
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command))
  :init
   (setq smex-save-file (concat my-cache-dir ".smex-items"))
  :config (smex-initialize))

(provide 'smex-conf)