
(use-package icicles
  :init
  (progn
    (message "%d: >>>>> Loading [ icicles ] Customization ...." step_no)
    (setq step_no (1+ step_no))
    (icy-mode 1)
    ))

(provide 'icicles-conf)