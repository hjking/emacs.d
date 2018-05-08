
(use-package saveplace
  :ensure nil
  :init
    ;; Emacs 25 has a proper mode for `save-place'
    (if (fboundp 'save-place-mode)
        (add-hook 'after-init-hook #'save-place-mode)
        (setq save-place t)
        (setq save-place-file (concat my-cache-dir "emacs.places"))
        ;; do not make backups of master save-place file
        (setq save-place-version-control "never")
        (define-key ctl-x-map "p" 'toggle-save-place-globally)))

(provide 'saveplace-conf)