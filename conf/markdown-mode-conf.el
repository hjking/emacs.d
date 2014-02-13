
(autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)

(add-hook 'markdown-mode-hook
      (lambda ()
        (define-key markdown-mode-map (kbd "<tab>") 'markdown-insert-pre)
        (define-key markdown-mode-map (kbd "M-<left>") nil)
        (define-key markdown-mode-map (kbd "M-<right>") nil)
        (visual-line-mode t)
        ))

;; Markdown file handling
(dolist (pattern '("\\.md$" "\\.markdown$" "\\.mkdn$"))
  (add-to-list 'auto-mode-alist (cons pattern 'markdown-mode)))

