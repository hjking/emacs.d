;; -*- lexical-binding: t -*-
;;

;;; Shell Mode
;; using shell interactively

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


;; Multi term
(use-package multi-term
  :commands (multi-term)
  ; :bind ("\M-t" . multi-term)
  :config
    (setq multi-term-switch-after-close nil)
    (defun term-mode-settings ()
      "Settings for term-mode"
      ; (make-local-variable 'scroll-margin)
      (setq-default scroll-margin 0)
    )

    ;; Don't try to enable autopair in term-mode, it remaps the return key!
    (add-hook 'term-mode-hook
              (lambda ()
                (setq show-trailing-whitespace nil)
                (autopair-mode 0)))

    ;; yanking / pasting
    (add-hook 'term-mode-hook
              (lambda ()
                (define-key term-raw-map (kbd "C-y") 'term-paste)))

    (add-hook 'term-mode-hook
              (lambda ()
                (setq term-buffer-maximum-size 50000)))

    (add-hook 'term-mode-hook 'term-mode-settings)
  )

(provide 'shell-mode-conf)