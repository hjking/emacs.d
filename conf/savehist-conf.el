
;; Save a minibuffer input history

(use-package savehist
  :ensure nil
  :init
  (setq enable-recursive-minibuffers t)
  (setq savehist-file (concat my-cache-dir ".emacs-history"))
  (setq history-length 1000)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history 1)
  (setq savehist-autosave-interval 180)
  (setq savehist-additional-variables '(kill-ring
                                        search-ring
                                        regexp-search-ring))
  (add-hook 'after-init-hook #'savehist-mode)
)

(provide 'savehist-conf)