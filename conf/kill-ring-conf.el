
;; browse-kill-ring

(use-package browse-kill-ring
  :load-path (lambda () (concat my-site-lisp-dir "browse-kill-ring/"))
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
    )
  :config (progn
    (browse-kill-ring-default-keybindings) ;; M-y
    )
  )

(provide 'kill-ring-conf)