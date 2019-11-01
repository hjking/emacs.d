
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
  (with-eval-after-load "flymake"
    (diminish 'flymake-mode " Ⓕ"))
  (with-eval-after-load "projectile"
    (diminish 'projectile-mode " Ⓟ"))
  (with-eval-after-load "flyspell"
    (diminish 'flyspell-mode " Ⓢ"))
  (with-eval-after-load "paredit"
    (diminish 'paredit-mode " (Ⓟ)"))
  (with-eval-after-load "tagedit"
    (diminish 'tagedit-mode " Ⓣ"))
  (with-eval-after-load "yasnippet"
    (diminish 'yas-minor-mode " Ⓨ"))
  )


;; Minor Mode (hidden) -------------------------------------------------------
(with-eval-after-load "smartparens"
  (diminish 'smartparens-mode))

(with-eval-after-load "hi-lock"
  (diminish 'hi-lock-mode))

(with-eval-after-load "page-break-lines"
  (diminish 'page-break-lines-mode))

(with-eval-after-load "rainbow-mode"
  (diminish 'rainbow-mode))

(with-eval-after-load "undo-tree"
  (diminish 'undo-tree-mode))

(with-eval-after-load "helm-mode"
  (diminish 'helm-mode))

(with-eval-after-load "git-gutter"
  (diminish 'git-gutter-mode))

; (with-eval-after-load "abbrev"
;   (diminish 'abbrev-mode))

(with-eval-after-load "volatile-highlights"
  (diminish 'volatile-highlights-mode))

(with-eval-after-load "outline"
  (diminish 'outline-minor-mode))

(with-eval-after-load "eldoc"
  (diminish 'eldoc-mode))

(with-eval-after-load "guru-mode"
  (diminish 'guru-mode "G"))

(with-eval-after-load "auto-revert"
  (diminish 'auto-revert-mode))

(with-eval-after-load "workgroups2"
  (diminish 'workgroups-mode))

(with-eval-after-load "auto-fill"
  (diminish 'auto-fill-mode))

(with-eval-after-load "magit"
  (diminish 'magit-auto-revert-mode))

(with-eval-after-load "whitespace"
  (diminish 'whitespace-mode))

(with-eval-after-load "guide-key"
  (diminish 'guide-key-mode))

; (diminish 'dired-view-minor-mode)

; (diminish 'drag-stuff-mode)

;;  (diminish 'wrap-region-mode)
;;  (diminish 'yas/minor-mode)

(diminish 'visual-line-mode)
;; (diminish 'global-visual-line-mode)

(with-eval-after-load 'highlight-symbol
  (diminish 'highlight-symbol-mode))

(with-eval-after-load 'indent-guide
  (diminish 'indent-guide-mode))

(diminish 'isearch-mode)

; (use-package diminish
;   :ensure t
;   :config
;   (diminish 'abbrev-mode)
;   (diminish 'auto-fill-function)
;   (diminish 'auto-revert-mode)
;   (diminish 'eldoc-mode)
;   (diminish 'whitespace-mode))