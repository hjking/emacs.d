;;
;; Filename: yasnippet-conf.el
;; Description: Setting for yasnippet.el
;; Author: Hong Jin
;; Created: 2011-8-31 10:00
;; Last Updated: 2014-02-12 10:47:47

(message "%d: >>>>> Loading [ yasnippet ] Customizations ...." step_no)
(setq step_no (1+ step_no))
;; (require 'yasnippet) ;; not yasnippet-bundle

(with-eval-after-load 'yasnippet
  ;; load as a global minor mode
  ;; (yas-global-mode 1)

  ;; use YASnippet as a non-global minor mode
  (yas-reload-all)

  ; (setq my-snippet-dir (concat my-site-lisp-dir "snippets/"))
  ; (yas/load-directory my-snippet-dir)
  (setq my-snippet-dir (concat my-site-lisp-dir "snippets/"))
  (if (and  (file-exists-p my-snippet-dir) (not (member my-snippet-dir yas/snippet-dirs)))
      (add-to-list 'yas/snippet-dirs my-snippet-dir))
  ; (setq yas-snippet-dirs
  ;   (list"/home/eric/.emacs.d/lisp/yasnippet/snippets"
  ;        "/home/eric/Projects/DotEmacs/snippets"))
  (setq yas/key-syntaxes '("w_" "w_." "^ "))

  ;; hook for automatic reloading of changed snippets
  (defun my-update-yasnippets-on-save ()
    (when (string-match "snippets" buffer-file-name)
      (yas/load-directory my-snippet-dir)))
  (add-hook 'after-save-hook 'my-update-yasnippets-on-save)

  ;; disable TAB trigger
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "M-[") 'yas-expand)

  (setq yas-new-snippet-default "# -*- mode: snippet -*-
  # contributor: Kaushal Modi
  # name: $1
  # key: ${2:${1:$(yas--key-from-desc yas-text)}}${3:
  # binding: ${4:direct-keybinding}}${5:
  # expand-env: ((yas-indent-line 'auto) (yas-also-auto-indent-first-line t) (yas-wrap-around-region t))}
  # --
  $0"
        )

  ;; enable yasnippet mode in program mode
  (add-hook 'prog-mode-hook
            '(lambda ()
                 (yas-minor-mode)))
  (add-hook 'markdown-mode 'yas-minor-mode)
  (add-hook 'org-mode-hook
            '(lambda ()
                 (yas-minor-mode)
                 (org-set-local 'yas/trigger-key [tab])
                 (define-key yas/keymap [tab] 'yas/next-field-or-maybe-expand))))


(provide 'yasnippet-conf)