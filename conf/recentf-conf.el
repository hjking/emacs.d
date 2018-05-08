
(use-package recentf
  :ensure nil
  :defer 1
  :init
  (setq recentf-max-saved-items 200)
  (defun ido-recentf-open ()
    "Use `ido-completing-read' to \\[find-file] a recent file"
    (interactive)
    (if (find-file (ido-completing-read "Find recent file: " recentf-save-file))
        (message "Opening file...")
      (message "Aborting")))
  ;; file to save the recent list into
  (setq recentf-save-file (concat my-cache-dir "recentf"))
  ;; maximum number of items in the recentf menu
  (setq recentf-max-menu-items 30)
  ;; to protect from TRAMP -- FIXME not correctly supported (yet) under Win32
  (setq recentf-auto-cleanup 'never)
  ;; save file names relative to my current home directory
  (setq recentf-filename-handlers '(abbreviate-file-name))
  (setq recentf-keep '(file-remote-p file-readable-p))
  (setq recentf-exclude '("/tmp/"
                          "/\\.git/.*\\'" ; Git contents
                          "/elpa/.*\\'" ; Package files
                          "/ssh:"
                          "/sudo:"
                          "/home/[a-z]\+/\\."
                          ))
  ;; lazy load recentf
  ;; (add-hook 'after-init-hook #'recentf-mode)
  (add-hook 'find-file-hook (lambda () (unless recentf-mode
                                         (recentf-mode)
                                         (recentf-track-opened-file))))
  :config (recentf-mode t)
  :bind ("C-x C-r" . ido-recentf-open))

(provide 'recentf-conf)