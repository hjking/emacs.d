

;;; Cygwin

  	(add-to-list 'exec-path my-cygwin-bin-dir)
  	(setq woman-manpath '(
  		  (concat my-cygwin-usr-path "man")
          (concat my-cygwin-usr-path "share/man")
          (concat my-cygwin-usr-path "local/man")))
    (setq shell-file-name "bash"
          explicit-shell-file-name "bash")
    (setenv "SHELL" shell-file-name)
    (setenv "CYGWIN" "nodosfilewarning")

    (when (require 'cygwin-mount nil t)
      (cygwin-mount-activate)
      (setq w32shell-cygwin-bin my-cygwin-dir))


;;      (setenv "PATH" (concat "d:/cygwin/bin" (getenv "PATH")))
;;      (require 'cygwin-mount)
;;      (cygwin-mount-activate)
;;      (add-hook 'comint-output-filter-functions
;;          'shell-strip-ctrl-m nil t)
;;      (add-hook 'comint-output-filter-functions
;;          'comint-watch-for-password-prompt nil t)
;;      (setq explicit-shell-file-name "bash.exe")
;;      ;; For subprocesses invoked via the shell
;;      ;; (e.g., "shell -c command")
;;      (setq shell-file-name explicit-shell-file-name)
