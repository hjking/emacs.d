
(use-package workgroups2
  :load-path (lambda () (concat my-site-lisp-dir "workgroups2/src/"))
  :commands (wg-create-workgroup
             wg-kill-workgroup
             wg-find-session-file
             wg-read-workgroup-name
             wg-switch-to-workgroup
             wg-switch-to-workgroup-at-index
             wg-save-session
             wg-switch-to-buffer
             wg-switch-to-workgroup-left
             wg-switch-to-workgroup-right
             wg-undo-wconfig-change
             wg-redo-wconfig-change
             wg-save-wconfig)
  :init
    ;; Change prefix key (before activating WG)
    (setq wg-prefix-key (kbd "C-c z"))

    ;; Change workgroups session file
    (setq wg-default-session-file (concat my-cache-dir ".emacs_workgroups"))
    ;; autoload/autosave:
    ;; if you start Emacs as "emacs --daemon" - turn off autoloading of workgroups:
    (setq wg-use-default-session-file t)
    (setq wg-mode-line-display-on nil)
  :config
    (workgroups-mode 1)   ; put this one at the bottom of .emacs
)

(provide 'workgroups-conf)