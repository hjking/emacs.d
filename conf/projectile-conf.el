;; Projectile
; (eval-after-load "projectile"
;   '(progn
    (require 'projectile)
    (projectile-global-mode)
    (setq projectile-enable-caching t)  ;; enable caching
    ;; restore the recent window configuration of the target project
    (setq projectile-remember-window-configs t)
    ;; Action after running projectile-switch-project (C-c p s)
    ;; default: projectile-find-file
    ;; once selected the project, the top-level directory of the project is opened in a dired buffer
    (setq projectile-switch-project-action 'projectile-dired)
    ;; projectile-find-dir : remain in Projectile's completion system to select a sub-directory of your project,
    ;; and then that sub-directory is opened for you in a dired buffer
    ;; probably also want to set
    ;; (setq projectile-find-dir-includes-top-level t)
    ; (defconst projectile-mode-line-lighter " P")
    ;; (diminish 'projectile-mode)

    ;; integrate perspective with projectile
    (require 'perspective)
    (persp-mode)
    (require 'persp-projectile)
    ; ))