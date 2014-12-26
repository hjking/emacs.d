

(autoload 'smex "smex" nil t)
; (smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                    ; when Smex is auto-initialized on its first run.
(eval-after-load "smex"
  (setq smex-save-file (concat my-cache-dir ".smex-items")))
;; Replaced with helm-M-x
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(provide 'smex-conf)