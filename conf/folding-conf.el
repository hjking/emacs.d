
(use-package origami
  :commands (origami-toggle-node))

(use-package vimish-fold
  :commands (vimish-fold-toggle
             vimish-fold-delete
             vimish-fold))

(use-package hideshow
  :disabled t
  :init (progn
         (add-hook 'prog-mode '(progn
                                (hs-minor-mode 1))))
  :config (progn
           (setq hs-isearch-open 'code) ; default 'code, options: 'comment, t, nil
           (setq hs-special-modes-alist
                  '((c-mode      "{" "}" "/[*/]" nil nil)
                    (c++-mode    "{" "}" "/[*/]" nil nil)))

           ;; hideshowvis
           (use-package hideshowvis
             :commands (hideshowvis-enable hideshowvis-minor-mode)
             :init (progn
                ; (dolist (hook (list 'emacs-lisp-mode-hook
                ;                     'c++-mode-hook
                ;                     'lisp-mode-hook
                ;                     'ruby-mode-hook
                ;                     'perl-mode-hook
                ;                     'php-mode-hook
                ;                     'python-mode-hook
                ;                     'lua-mode-hook
                ;                     'c-mode-hook
                ;                     'java-mode-hook
                ;                     'js-mode-hook
                ;                     'verilog-mode-hook
                ;                     'css-mode-hook))
                ;   (add-hook hook 'hideshowvis-enable)))
                (add-hook 'prog-mode '(progn
                                       (hideshowvis-minor-mode 1)))
                )
           )
  ))

(provide 'folding-conf)