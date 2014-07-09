
(when (require 'saveplace nil t)
  (message "%d: >>>>> Loading [ saveplace ] Customization ...." step_no)
  (setq step_no (1+ step_no))
  ;; automatically save place in each file
  (setq-default save-place t)
  ;; name of the file that records `save-place-alist' value
  (setq save-place-file (concat my-cache-dir "emacs.places"))
  ;; do not make backups of master save-place file
  (setq save-place-version-control "never")
  (define-key ctl-x-map "p" 'toggle-save-place-globally))