;;
;; Filename: yasnippet-conf.el
;; Description: Setting for yasnippet.el
;; Author: Hong Jin
;; Created: 2011-8-31 10:00
;; Last Updated: 2013-09-03 11:07:14

(message "%d: >>>>> Loading [ yasnippet ] Customizations ...." step_no)
(setq step_no (1+ step_no))
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
; (setq my-yasnippet-dir "~/.emacs.d/plugin/yasnippet/snippets")
(setq my-yasnippet-dir (concat my-site-lisp-dir "yasnippet/snippets/"))
(yas/load-directory my-yasnippet-dir)
(setq yas/key-syntaxes '("w_" "w_." "^ "))

;; hook for automatic reloading of changed snippets
(defun my-update-yasnippets-on-save ()
  (when (string-match "/yasnippet/snippets" buffer-file-name)
    (yas/load-directory my-yasnippet-dir)))
(add-hook 'after-save-hook 'my-update-yasnippets-on-save)

;; disable TAB trigger
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
