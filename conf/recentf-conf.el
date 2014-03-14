
(message "%d: >>>>> Loading [ recentf ] Customization ...." step_no)
(setq step_no (1+ step_no))
;; recentf is a minor mode that builds a list of recently opened files
;; keep a list of recently opened files
;; this list is automatically saved across Emacs sessions
;; open recently opened files under menubar
(require 'recentf)
;; toggle `recentf' mode
(recentf-mode 1)
;; file to save the recent list into
(setq recentf-save-file (concat my-cache-dir "recentf"))
(setq recentf-max-saved-items 500)
;; maximum number of items in the recentf menu
(setq recentf-max-menu-items 30)
;; to protect from TRAMP -- FIXME not correctly supported (yet) under Win32
(setq recentf-auto-cleanup 'never)
;; save file names relative to my current home directory
(setq recentf-filename-handlers '(abbreviate-file-name))
(setq recentf-keep '(file-remote-p file-readable-p))
(setq recentf-exclude '("/tmp/"
                        "/ssh:"
                        "/sudo:"
                        "/home/[a-z]\+/\\."
                        ))
;; add key binding
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
