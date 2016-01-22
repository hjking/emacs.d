
(message "%d: >>>>> Loading [ Shell Mode ] Customizations ...." step_no)
(setq step_no (1+ step_no))
;; (setq popup-terminal-command '("/bin/bash"))

;; close shell buffer when "exit"
(setq comint-use-prompt-regexp-instead-of-fields nil)
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
;; close when exit shell
(defun my-shell-mode-hook-func  ()
  (set-process-sentinel (get-buffer-process (current-buffer))
                            #'my-shell-mode-kill-buffer-on-exit)
)
(defun my-shell-mode-kill-buffer-on-exit (process state)
  (message "%s" state)
  (if (or
        (string-match "exited abnormally with code.*" state)
        (string-match "finished" state))
        (kill-buffer (current-buffer)
      )
  )
)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on t)

;; highlight some text based on regexp, like OK
(add-hook 'shell-mode-hook (lambda () (highlight-regexp "\\[OK\\]" "hi-green-b")))

;; Every line representing a path to a file will be colorized
;; and made clickable, so that you can jump to that file and
;; that line, like in compilation-mode
(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)

;; Make URLs clickable
(add-hook 'shell-mode-hook (lambda () (goto-address-mode )))

;; (add-hook 'shell-mode-hook 'my-shell-mode-hook-func)
;; (add-hook 'term-mode-hook 'my-shell-mode-hook-func)

;; C-d to kill buffer if process is dead.

(defun comint-delchar-or-eof-or-kill-buffer (arg)
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)))

(provide 'shell-mode-conf)