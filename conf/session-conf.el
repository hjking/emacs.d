
(message "%d: >>>>> Loading [ session ] Customizations ...." step_no)
(setq step_no (1+ step_no))

(require 'session)
(setq session-save-file (concat my-cache-dir "session"))
(add-to-list 'session-globals-exclude 'org-mark-ring)
(add-hook 'after-init-hook 'session-initialize)

