;; -*- lexical-binding: t -*-
;;
;; Filename: yasnippet-conf.el
;; Description: Setting for yasnippet.el
;; Author: Hong Jin
;; Created: 2011-8-31 10:00
;; Last Updated: 2014-02-12 10:47:47

(use-package yasnippet
  :defer 5
  :ensure t
  :commands (yas-insert-snippet
             yas-new-snippet
             yas-hippie-try-expand)
  :diminish yas-minor-mode
  :init
   (setq yas-verbosity 1)
   (setq my-snippet-dir (concat my-site-lisp-dir "snippets/"))
   (if (and  (file-exists-p my-snippet-dir) (not (member my-snippet-dir yas-snippet-dirs)))
      (add-to-list 'yas-snippet-dirs my-snippet-dir))

   (setq yas-triggers-in-field t); Enable nested triggering of snippets
   (setq yas-prompt-functions '(yas-completing-prompt))
   (setq yas-wrap-around-region t)
   (push 'yas-hippie-try-expand hippie-expand-try-functions-list)
   ; (add-hook 'after-init-hook #'yas-global-mode)
  :config
   (add-hook 'snippet-mode-hook '(lambda () (setq-local require-final-newline nil)))
   (defun sk/force-yasnippet-off ()
     (yas-minor-mode -1)
     (setq yas-dont-activate t))
   (add-hook 'term-mode-hook 'sk/force-yasnippet-off)
   (add-hook 'shell-mode-hook 'sk/force-yasnippet-off))

(provide 'yasnippet-conf)