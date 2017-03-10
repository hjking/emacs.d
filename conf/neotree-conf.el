;; https://www.emacswiki.org/emacs/NeoTree

;; A Emacs tree plugin like NerdTree for Vim.

(use-package neotree
  :commands (neotree neotree-toggle)
  :init
   (setq neo-smart-open t)
  )

(provide 'neotree-conf)