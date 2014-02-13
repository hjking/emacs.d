


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
 )

;; C-c . g d  : switch directory window
;; C-c . g m  : switch method/function window
;; C-c . g s  : switch source window
;; C-c . g h  : switch history window
;; C-c . g l  : switch last edit window
;; C-c . h    : help
