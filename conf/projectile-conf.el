;; Projectile

(use-package projectile
  :diminish projectile-mode
  :bind (("C-c p f" . projectile-find-file)
         ("C-c p d" . projectile-find-dir)
         ("C-c p g" . projectile-grep)
         ("C-c p p" . projectile-find-file))
  :commands (projectile-global-mode
             projectile-ack
             projectile-ag
             projectile-compile-project
             projectile-dired
             projectile-grep
             projectile-find-dir
             projectile-find-file
             projectile-find-tag
             projectile-find-test-file
             projectile-invalidate-cache
             projectile-kill-buffers
             projectile-multi-occur
             projectile-project-root
             projectile-recentf
             projectile-regenerate-tags
             projectile-replace
             projectile-run-async-shell-command-in-root
             projectile-run-shell-command-in-root
             projectile-switch-project
             projectile-switch-to-buffer
             projectile-vc)
  :init (progn
    ; (projectile-global-mode 1)
    (setq projectile-keymap-prefix (kbd "C-c p"))
    ; (setq projectile-completion-system 'default) ; ido/grizzl/ido-flx
    ;; with helm
    ; (setq projectile-completion-system 'helm)
    (with-eval-after-load 'ivy
        (setq projectile-completion-system 'ivy))
    (setq projectile-enable-caching t)
    (when win32p
        ;; enable external indexing
        (setq projectile-indexing-method 'alien))
    (setq projectile-cache-file (concat my-cache-dir "projectile.cache"))
    (setq projectile-known-projects-file (concat my-cache-dir "projectile-bookmarks.eld"))
    ;; restore the recent window configuration of the target project
    (setq projectile-remember-window-configs t)
    ;; Action after running projectile-switch-project (C-c p s)
    ;; default: projectile-find-file
    ;; once selected the project, the top-level directory of the project is opened in a dired buffer
    (setq projectile-switch-project-action 'projectile-dired)
    ;; same as projectile-find-file but with more features
    ; (setq projectile-switch-project-action 'helm-projectile-find-file)
    (setq projectile-switch-project-action 'helm-projectile)
    ;; projectile-find-dir : remain in Projectile's completion system to select a sub-directory of your project,
    ;; and then that sub-directory is opened for you in a dired buffer
    ;; probably also want to set
    ;; (setq projectile-find-dir-includes-top-level t)
    ; (defconst projectile-mode-line-lighter " P")
    (setq projectile-enable-idle-timer t)
    )
  :config (progn
    ;; Don't consider my home dir as a project
    (add-to-list 'projectile-ignored-projects `,(concat (getenv "HOME") "/"))
    (dolist (item '(".SOS" "nobackup"))
      (add-to-list 'projectile-globally-ignored-directories item))
    (dolist (item '("GTAGS" "GRTAGS" "GPATH"))
      (add-to-list 'projectile-globally-ignored-files item))
    (projectile-global-mode)
    ;; integrate perspective with projectile
    (use-package perspective
      :config
       (persp-mode))

    (use-package persp-projectile
      :config
       (define-key projectile-mode-map (kbd "s-s") 'projectile-persp-switch-project))

    (with-eval-after-load 'helm
      (use-package helm-projectile
        :disabled t
        :commands (helm-projectile-switch-to-buffer
                   helm-projectile-find-dir
                   helm-projectile-dired-find-dir
                   helm-projectile-recentf
                   helm-projectile-find-file
                   helm-projectile-grep
                   helm-projectile
                   helm-projectile-switch-project)
        :init (progn
          (setq projectile-switch-project-action 'helm-projectile))
        :config (progn
          (helm-projectile-on))))
  ))

(provide 'projectile-conf)