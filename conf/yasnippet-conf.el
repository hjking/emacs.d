;;
;; Filename: yasnippet-conf.el
;; Description: Setting for yasnippet.el
;; Author: Hong Jin
;; Created: 2011-8-31 10:00
;; Last Updated: 2014-01-24 10:31:44

(message "%d: >>>>> Loading [ yasnippet ] Customizations ...." step_no)
(setq step_no (1+ step_no))
(require 'yasnippet) ;; not yasnippet-bundle

;; load as a global minor mode
;; (yas-global-mode 1)

;; use YASnippet as a non-global minor mode
(yas-reload-all)
(add-hook 'prog-mode-hook
          '(lambda ()
               (yas-minor-mode)))

(setq my-snippet-dir (concat my-site-lisp-dir "snippets/"))
(yas/load-directory my-snippet-dir)
;; (setq yas-snippet-dirs
;;           '(,my-snippet-dir                 ;; personal snippets
;;            ))
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
