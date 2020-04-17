;; https://www.emacswiki.org/emacs/NeoTree

;; A Emacs tree plugin like NerdTree for Vim.

(use-package neotree
  :ensure t
  :disabled t
  :commands (neotree neotree-toggle)
  :bind ("C-x t t" . neotree-toggle)
  :init
  (setq neo-smart-open t)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-window-fixed-size nil)
  )

(provide 'neotree-conf)