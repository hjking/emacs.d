; Ivy, a generic completion mechanism for Emacs.

; Counsel, a collection of Ivy-enhanced versions of common Emacs commands.

; Swiper, an Ivy-enhanced alternative to isearch.

;; Ivy / Counsel replace a lot of ido or helms completion functionality
(use-package ivy
  :diminish ivy-mode ; does not display ivy in the modeline
  :init (progn
          (ivy-mode 1)        ; enable ivy globally at startup
          ;; show recently killed buffers when calling `ivy-switch-buffer'
          ;; extend searching to bookmarks and …
          (setq ivy-use-virtual-buffers t)
          (setq ivy-initial-inputs-alist nil) ; don't need start with ^
          (setq ivy-virtual-abbreviate 'full) ; Show the full virtual file paths
          (setq ivy-count-format "(%d/%d) ") ; count format, from the ivy help page
          ; (setq ivy-display-style 'fancy)
          ; (setq ivy-re-builders-alist
          ;  '((ivy-switch-buffer . ivy--regex-plus)
          ;    (t . ivy--regex-fuzzy))) ; (t . ivy--regex-plus)
          ; (setq ivy-initial-inputs-alist nil)  ; if fuzzy with flx, no need the initial ^
          ;; Always ignore buffers set in `ivy-ignore-buffers'
          (setq ivy-use-ignore-default 'always)
          ;; Ignore some buffers in `ivy-switch-buffer'
          (setq ivy-ignore-buffers '("company-statistics-cache.el"))
        )
  :bind (("C-c C-r"  . ivy-resume)
         ("C-x b"    . ivy-switch-buffer))
  ;        (:map ivy-mode-map  ("C-'" . ivy-avy))) ; bind in the ivy buffer C-' to ivy-avy
  :config (progn
           ;; Disable ido
           (with-eval-after-load 'ido
             (ido-mode -1))
           (ivy-mode 1))
 )

;; Swiper gives us a really efficient incremental search with regular expressions
(use-package swiper
  :after ivy
  :commands (swiper
             swiper-all
             ivy-mode
             ivy-read
             ivy-completing-read
             ivy-resume
             ivy-switch-buffer
             ivy-switch-buffer-other-window)
  :bind (("C-s" . swiper)
         ("C-r" . swiper))
  )

;; Replace smex
(use-package counsel
  :after ivy
  :diminish ivy-mode counsel-mode
  :bind (("M-x"     . counsel-M-x)       ; M-x use counsel
         ("M-y"     . counsel-yank-pop)
         ("C-o"     . counsel-find-file)
         ("C-x C-f" . counsel-find-file) ; C-x C-f use counsel-find-file
         ("C-x C-r" . counsel-recentf)   ; search recently edited files
         ("C-c g"   . counsel-git)       ; search for files in git repo
         ("C-c j"   . counsel-git-grep)  ; search for regexp in git repo
         ("C-c k"   . counsel-ag)        ; search for regexp in git repo using ag
         ("C-c l"   . counsel-locate)    ; search for files or else using locate
         ("C-h f"   . counsel-describe-function)
         ("C-h v"   . counsel-describe-variable)
         ("C-h l"   . counsel-load-library)
         ("C-h i"   . counsel-info-lookup-symbol)
         ("C-h u"   . counsel-unicode-char))
  :config (progn
    (setq counsel-prompt-function #'counsel-prompt-function-dir)

    ;; counsel-find-file
    (setq counsel-find-file-at-point t)
    (setq counsel-find-file-ignore-regexp
          (concat
           ;; file names beginning with # or .
           "\\(?:\\`[#.]\\)"
           ;; file names ending with # or ~
           "\\|\\(?:[#~]\\'\\)"))

    (with-eval-after-load 'org
        (bind-key "C-c C-q" #'counsel-org-tag org-mode-map))
    (with-eval-after-load 'org-agenda
        (bind-key "C-c C-q" #'counsel-org-tag-agenda org-agenda-mode-map)))
  )

(use-package ivy-rich
  :defer 6
  :after ivy
  :config
  (ivy-rich-mode 1))

(use-package flx
  :load-path (lambda () (concat my-site-lisp-dir "flx-ido/")))

(provide 'ivy-conf)