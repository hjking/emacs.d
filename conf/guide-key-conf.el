
(use-package guide-key
  :diminish ""
  :init
  (progn
    ;; for org mode
    (defun guide-key/my-hook-function-for-org-mode ()
      (guide-key/add-local-guide-key-sequence "C-c")
      (guide-key/add-local-guide-key-sequence "C-c C-x")
      (guide-key/add-local-highlight-command-regexp "org-"))
    (add-hook 'org-mode-hook 'guide-key/my-hook-function-for-org-mode)
    (setq guide-key/guide-key-sequence '("C-x r" ;; rectangle, registers
                                         "C-x 4" ;; commands that operate in other window (buffer)
                                         "C-x 5" ;; commands that operate in other frame (window)
                                         "C-x"
                                         "C-c"
                                         (outline-minor-mode "C-c @")))
    (setq guide-key/recursive-key-sequence-flag t)

    ;; highlight only rectangle family commands when press "C-x r"
    ;; (setq guide-key/highlight-command-regexp "rectangle")
    ;; highlight both rectangle family and register family when press "C-x r"
    (setq guide-key/highlight-command-regexp "rectangle\\|register")

    (guide-key-mode 1))
  :config
  (progn

    (use-package guide-key-tip
      :init
      (setq guide-key-tip/enabled t))
  ))

(provide 'guide-key-conf)