
;; Save a minibuffer input history

(use-package savehist
  :init (progn
         (setq savehist-file (concat my-cache-dir ".emacs-history"))
         (setq history-length t)
         (setq history-delete-duplicates t)
         (setq savehist-save-minibuffer-history 1)
         (setq savehist-autosave-interval 180)
         (setq savehist-additional-variables
               '(kill-ring
                 search-ring
                 regexp-search-ring)))
  :config (savehist-mode t)
)

(provide 'savehist-conf)