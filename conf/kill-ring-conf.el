
(message "%d: >>>>> Loading [ browse-kill-ring ] Customizations ...." step_no)
(setq step_no (1+ step_no))

(use-package browse-kill-ring
  :commands browse-kill-ring
  :config
  (progn
    ; (global-set-key (kbd "s-y") 'browse-kill-ring)
    (browse-kill-ring-default-keybindings) ;; M-y
    (setq browse-kill-ring-separator "\n--separator------------------------------")
    ;; temporarily highlight the inserted `kill-ring' entry
    (setq browse-kill-ring-highlight-inserted-item t)
    ;; face in which to highlight the `browse-kill-ring-separator'
    (defface separator-face '((t (:foreground "Blueviolet" :weight bold))) nil)
    (setq browse-kill-ring-separator-face 'separator-face)
    (setq browse-kill-ring-quit-action 'save-and-restore)
    )
  )

(provide 'kill-ring-conf)