
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

;;
;; folding (hiding) parts of the text
;; autoload when turn on `folding-mode'
; (message "%d: >>>>> Loading [ Folding ] Customization ...." step_no)
; (setq step_no (1+ step_no))
; (autoload 'folding-mode          "folding" "Folding mode" t)
; (autoload 'turn-off-folding-mode "folding" "Folding mode" t)
; (autoload 'turn-on-folding-mode  "folding" "Folding mode" t)
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
;   (add-hook hook 'turn-on-folding-mode))

; (eval-after-load 'folding
;   '((folding-add-to-marks-list 'ruby-mode "#{{{" "#}}}" nil t)
;     (folding-add-to-marks-list 'php-mode    "//{"  "//}"  nil t)
;     (folding-add-to-marks-list 'html-mode   "<!-- {{{ " "<!-- }}} -->" " -->" nil t)
;     (folding-add-to-marks-list 'verilog-mode "// {"  "// }"  nil t)
;     (folding-add-to-marks-list 'sh-mode "#{{{" "#}}}" nil t)
;     (folding-add-to-marks-list 'emacs-lisp-mode ";;{"  ";;}"  nil t)
;     (folding-add-to-marks-list 'c-mode "/* {{{ " "/* }}} */" " */" t)))

(provide 'folding-conf)