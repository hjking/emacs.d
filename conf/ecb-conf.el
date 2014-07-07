
;; load the ECB first after starting it by ecb-activate
(require 'ecb-autoloads)
(when (require 'ecb nil 'noerror)
  (setq ecb-version-check nil)  ; to prevent ecb failing to start up
  (setq stack-trace-on-error t)
  (setq ecb-tip-of-the-day nil)

  ;; Change layout
  ;; (ecb-layout-define "my-cscope-layout" left nil
  ;;     (ecb-set-methods-buffer)
  ;;     (ecb-split-ver 0.5 t)
  ;;     (other-window 1)
  ;;     (ecb-set-history-buffer)
  ;;     (ecb-split-ver 0.25 t)
  ;;     (other-window 1)
  ;;     (ecb-set-cscope-buffer))
  ;;
  ;; (defecb-window-dedicator ecb-set-cscope-buffer " *ECB cscope-buf*"
  ;;             (switch-to-buffer "*cscope*"))
  ;;
  ;; (setq ecb-layout-name "my-cscope-layout")

  ;; Disable buckets so that history buffer can display more entries
  (setq ecb-history-make-buckets 'never)
  (setq ecb-auto-compatibility-check nil)
  (setq ecb-version-check nil)

  ;;; my favorite layout
  (setq ecb-windows-width 0.12)
  (setq ecb-layout-name "leftright2"
      ecb-layout-window-sizes
      '(("leftright2"
       (ecb-directories-buffer-name 0.12 . 0.6428571428571429)
       (ecb-sources-buffer-name 0.12 . 0.3392857142857143)
       (ecb-methods-buffer-name 0.12 . 0.6428571428571429)
       (ecb-history-buffer-name 0.12 . 0.3392857142857143))))

  ;;; compilation window
  (setq ecb-compile-window-height 12)
  (setq ecb-enlarged-compilation-window-max-height 0.5)
  (setq ecb-compile-window-temporally-enlarge 'after-selection)
  ;;; show sources in directories buffer
  (setq ecb-show-sources-in-directories-buffer 'always)

  ;;; replacement for built-in ecb-deactive, ecb-hide-ecb-windows and
  ;;; ecb-show-ecb-windows functions
  ;;; since they hide/deactive ecb but not restore the old windows for me
  (defun tmtxt/ecb-deactivate ()
    "deactive ecb and then split emacs into 2 windows that contain 2 most recent buffers"
    (interactive)
    (ecb-deactivate)
    (split-window-right)
    (switch-to-next-buffer)
    (other-window 1))
  (defun tmtxt/ecb-hide-ecb-windows ()
    "hide ecb and then split emacs into 2 windows that contain 2 most recent buffers"
    (interactive)
    (ecb-hide-ecb-windows)
    (split-window-right)
    (switch-to-next-buffer)
    (other-window 1))
  (defun tmtxt/ecb-show-ecb-windows ()
    "show ecb windows and then delete all other windows except the current one"
    (interactive)
    (ecb-show-ecb-windows)
    (delete-other-windows))

    (add-to-list 'ecb-source-path '("~/.emacs.d/" ".emacs.d"))
    (add-to-list 'ecb-source-path '("~/dotfiles" "dotfiles"))
  )

;; C-c . g d  : switch directory window
;; C-c . g m  : switch method/function window
;; C-c . g s  : switch source window
;; C-c . g h  : switch history window
;; C-c . g l  : switch last edit window
;; C-c . h    : help
