;; Projectile

(use-package projectile
  :ensure t
  :disabled t
  :diminish projectile-mode
  :commands (projectile-ack
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
  :init
  (progn
    (message "%d: >>>>> Loading [ projectile ] Customization ...." step_no)
    (setq step_no (1+ step_no))
    ; (setq projectile-completion-system 'default)
    ;; with helm
    (setq projectile-completion-system 'helm)
    (helm-projectile-on)
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
    (projectile-global-mode)
    )
  :config
  (progn
    ;; integrate perspective with projectile
    (use-package perspective
      :config
      (persp-mode))

    (use-package persp-projectile
      :config
      (define-key projectile-mode-map (kbd "s-s") 'projectile-persp-switch-project))


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
      :init
      (progn
        (setq projectile-switch-project-action 'helm-projectile)))
  ))

(provide 'projectile-conf)