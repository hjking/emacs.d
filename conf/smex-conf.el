
; (require 'smex)
; (smex-initialize)
(autoload 'smex "smex" nil t)
(setq smex-save-file (concat my-cache-dir ".smex-items"))
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)