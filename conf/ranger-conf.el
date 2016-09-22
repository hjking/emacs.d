
;; https://github.com/ralesi/ranger.el

;; This is a minor mode that runs within dired emulating
;; many of the features of ranger. This minor mode shows
;; a stack of the parent directories and updates the
;; parent buffers while navigating the file system.

(use-package ranger
  :commands (ranger)
  :bind (("C-x d" . deer))
  :config
   (setq ranger-cleanup-eagerly t)
  )

(provide 'ranger-conf)