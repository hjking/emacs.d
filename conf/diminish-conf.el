
(require 'diminish)

;; Major modes abbrev --------------------------------------------------------
(add-hook 'emacs-lisp-mode-hook
          (lambda () (setq mode-name "Elisp")))

(add-hook 'erlang-mode-hook
          (lambda () (setq mode-name "Erlang")))

(add-hook 'python-mode-hook
          (lambda () (setq mode-name "Python")))


;; Minor modes abbrev --------------------------------------------------------
(when (display-graphic-p)
  ; (eval-after-load "auto-complete"
  ;   '(diminish 'auto-complete-mode " Ⓐ"))
  (eval-after-load "flymake"
    '(diminish 'flymake-mode " Ⓕ"))
  (eval-after-load "projectile"
    '(diminish 'projectile-mode " Ⓟ"))
  (eval-after-load "flyspell"
    '(diminish 'flyspell-mode " Ⓢ"))
  (eval-after-load "paredit"
    '(diminish 'paredit-mode " (Ⓟ)"))
  (eval-after-load "tagedit"
    '(diminish 'tagedit-mode " Ⓣ"))
  (eval-after-load "yasnippet"
    '(diminish 'yas-minor-mode " Ⓨ"))
  )


;; Minor Mode (hidden) -------------------------------------------------------
(eval-after-load "smartparens"
  '(diminish 'smartparens-mode))

(eval-after-load "hi-lock"
  '(diminish 'hi-lock-mode))

(eval-after-load "page-break-lines"
  '(diminish 'page-break-lines-mode))

(eval-after-load "rainbow-mode"
  '(diminish 'rainbow-mode))

(eval-after-load "undo-tree"
  '(diminish 'undo-tree-mode))

(eval-after-load "helm-mode"
  '(diminish 'helm-mode))

(eval-after-load "git-gutter"
  '(diminish 'git-gutter-mode))

(eval-after-load "abbrev"
  '(diminish 'abbrev-mode))
; (diminish 'abbrev-mode "Abv")

(eval-after-load "volatile-highlights"
  '(diminish 'volatile-highlights-mode))

(eval-after-load "outline"
  '(diminish 'outline-minor-mode))

(eval-after-load "eldoc"
  '(diminish 'eldoc-mode))

(eval-after-load "guru-mode"
  '(diminish 'guru-mode "G"))

(eval-after-load "auto-revert"
  '(diminish 'auto-revert-mode))

(eval-after-load "workgroups2"
  '(diminish 'workgroups-mode))

(eval-after-load "auto-fill"
  '(diminish 'auto-fill-mode))

(eval-after-load "magit"
  '(diminish 'magit-auto-revert-mode))

(eval-after-load "whitespace"
  '(diminish 'whitespace-mode))

(eval-after-load "guide-key"
  '(diminish 'guide-key-mode))

; (diminish 'dired-view-minor-mode)

(diminish 'drag-stuff-mode)

;;  (diminish 'wrap-region-mode)
;;  (diminish 'yas/minor-mode)

(diminish 'visual-line-mode)
;; (diminish 'global-visual-line-mode)

(eval-after-load 'highlight-symbol
  '(diminish 'highlight-symbol-mode))

(eval-after-load 'aggressive-indent
  '(diminish 'aggressive-indent-mode))