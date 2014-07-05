
(require 'workgroups2)

;; Change prefix key (before activating WG)
(setq wg-prefix-key (kbd "C-c z"))

;; Change workgroups session file
(setq wg-default-session-file (concat my-cache-dir ".emacs_workgroups"))
;; autoload/autosave:
;; if you start Emacs as "emacs --daemon" - turn off autoloading of workgroups:
(setq wg-use-default-session-file t)
(setq wg-mode-line-display-on nil)
;; Set your own keyboard shortcuts to reload/save/switch WG:
(global-set-key (kbd "<pause>")     'wg-reload-session)
(global-set-key (kbd "C-S-<pause>") 'wg-save-session)
(global-set-key (kbd "s-z")         'wg-switch-to-workgroup)
(global-set-key (kbd "s-/")         'wg-switch-to-previous-workgroup)

(autoload 'wg-create-workgroup "workgroups2" nil t)
(autoload 'wg-kill-workgroup "workgroups2" nil t)
(autoload 'wg-find-session-file "workgroups2" nil t)
(autoload 'wg-read-workgroup-name "workgroups2" nil t)
(autoload 'wg-switch-to-workgroup "workgroups2" nil t)
(autoload 'wg-switch-to-workgroup-at-index "workgroups2" nil t)
(autoload 'wg-save-session "workgroups2" nil t)
(autoload 'wg-switch-to-buffer "workgroups2" nil t)
(autoload 'wg-switch-to-workgroup-left "workgroups2" nil t)
(autoload 'wg-switch-to-workgroup-right "workgroups2" nil t)
(autoload 'wg-undo-wconfig-change "workgroups2" nil t)
(autoload 'wg-redo-wconfig-change "workgroups2" nil t)
(autoload 'wg-save-wconfig "workgroups2" nil t)

(workgroups-mode 1)   ; put this one at the bottom of .emacs