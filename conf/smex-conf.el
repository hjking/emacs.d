
(use-package smex
  :disabled t
  :load-path my-smex-path
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command))
  :init
   (setq smex-save-file (concat my-cache-dir ".smex-items")))

(provide 'smex-conf)