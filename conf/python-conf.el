
;; Filename: python-conf.el
;; Description: Setting for python-mode.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2010-12-14 14:33:45

;; The package is "python" but the mode is "python-mode":
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (defun my-python-startup ()
    "Setup Python style."
    (interactive)
    (local-set-key '[f4] 'pdb)
    (setq tab-width 4)
    (setq indent-tabs-mode nil)  ; Autoconvert tabs to spaces
    (setq python-indent 2)
    (setq python-continuation-offset 2)
    (setq py-smart-indentation nil)
    )
  (add-hook 'python-mode-hook 'my-python-startup)
  (add-hook 'python-mode-hook
    (lambda ()
      (set
        (make-variable-buffer-local 'beginning-of-defun-function) 'py-beginning-of-def-or-class)
      (setq outline-regexp "def\\|class ")
    )
  )
  )


(provide 'python-conf)