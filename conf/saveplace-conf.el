
(use-package saveplace
  :init
  (setq-default save-place t)
  (setq save-place-file (concat my-cache-dir "emacs.places"))
  ;; do not make backups of master save-place file
  (setq save-place-version-control "never")
  (define-key ctl-x-map "p" 'toggle-save-place-globally))

(provide 'saveplace-conf)