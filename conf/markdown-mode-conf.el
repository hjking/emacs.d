
;; Markdown file handling

(use-package markdown-mode
  :mode ("\\.\\(m\\(ark\\)?down\\|md\\)$" . markdown-mode)
  :config (progn
            (when win32p
              (add-to-list 'exec-path "D:/DEV/Python27/Scripts")
              (defun markdown-custom ()
                    "markdown-mode-hook"
                    (setq markdown-command "markdown | D:/DEV/Python27/Scripts/markdown_py.bat"))
                  (add-hook 'markdown-mode-hook '(lambda() (markdown-custom))))

            (add-hook 'markdown-mode-hook
                  (lambda ()
                    (define-key markdown-mode-map (kbd "<tab>") 'markdown-insert-pre)
                    (define-key markdown-mode-map (kbd "M-<left>") nil)
                    (define-key markdown-mode-map (kbd "M-<right>") nil)
                    (define-key markdown-mode-map (kbd "C-c C-x") 'fc/markdown-code-block)
                    (define-key markdown-mode-map (kbd "C-c C-n") 'fc/markdown-next-header)
                    (define-key markdown-mode-map (kbd "C-c C-p") 'fc/markdown-previous-header)
                    (visual-line-mode t)
                    ))

            (defun fc/markdown-code-block (beg end)
              "Wrap the current region into a code block."
              (interactive "r")
              (save-excursion
                (goto-char end)
                (when (not (bolp))
                  (insert "\n"))
                (insert "```\n")
                (goto-char beg)
                (forward-line 0)
                (insert "```\n")))


            (defun fc/markdown-next-header ()
              "Go to the next header in the file."
              (interactive)
              (let ((next-header (save-excursion
                                   (forward-line 1)
                                   (re-search-forward "^#" nil t))))
                (if (not next-header)
                    (error "No next header")
                  (goto-char next-header)
                  (goto-char (point-at-bol)))))


            (defun fc/markdown-previous-header ()
              "Go to the previous header in the file."
              (interactive)
              (let ((previous-header (save-excursion
                                       (forward-line -1)
                                       (re-search-backward "^#" nil t))))
                (if (not previous-header)
                    (error "No previous header")
                  (goto-char previous-header)
                  (goto-char (point-at-bol)))))
    ))

(provide 'markdown-mode-conf)