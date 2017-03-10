
(use-package deft
  ; :bind ("<f8>" . deft)
  :commands (deft)
  :config (setq deft-directory "~/org"
                deft-extensions '("txt" "tex" "md" "org")
                deft-recursive t))

(provide 'deft-conf)