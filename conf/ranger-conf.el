;; https://github.com/ralesi/ranger.el

;; This is a minor mode that runs within dired emulating
;; many of the features of ranger. This minor mode shows
;; a stack of the parent directories and updates the
;; parent buffers while navigating the file system.

(use-package ranger
  :commands (ranger)
  :bind (("C-x d" . deer))
  :init (progn
          ;; show dotfiles at ranger startup
          (setq ranger-show-dotfiles t)
          ;; kill the buffer just after you move to
          ;; another entry in the dired buffer
          (setq ranger-cleanup-eagerly t)
          ;; NOT open certain files like videos when previewing
          (setq ranger-excluded-extensions '("mkv" "iso" "mp4"))
          ;; set the max files size (MB)
          (setq ranger-max-preview-size 10)
          ;; NOT preview if the file selected is a binary file
          (setq ranger-dont-show-binary t))
  :config (progn
            ;; (ranger-override-dired-mode t)
            )
  )

(provide 'ranger-conf)
