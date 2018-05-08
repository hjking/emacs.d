
;; browse-kill-ring

(use-package browse-kill-ring
  :commands browse-kill-ring
  :init (progn
    ; (global-set-key (kbd "s-y") 'browse-kill-ring)
    (setq browse-kill-ring-separator "\n--separator------------------------------")
    ;; temporarily highlight the inserted `kill-ring' entry
    (setq browse-kill-ring-highlight-inserted-item t)
    ;; face in which to highlight the `browse-kill-ring-separator'
    (defface separator-face '((t (:foreground "Blueviolet" :weight bold))) nil)
    (setq browse-kill-ring-separator-face 'separator-face)
    (setq browse-kill-ring-quit-action 'save-and-restore)
    (add-hook 'after-init-hook #'browse-kill-ring-default-keybindings)
    )
  )

(use-package browse-kill-ring+
  :after browse-kill-ring)

(provide 'kill-ring-conf)