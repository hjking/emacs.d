;;
;; Filename: eshell-conf.el
;; Description: Setting for eshell.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2010-12-22 09:40:50

(message "%d: >>>>> Loading [ shell-mode ] Customizations ...." step_no)
(setq step_no (1+ step_no))

;; Eshell, the Emacs Shell
(use-package eshell
  :commands (eshell)
  ; :bind* (("M-m SPC s" . sk/eshell-vertical)
  ;         ("M-m SPC S" . sk/eshell-horizontal))
  :init
  (setq eshell-glob-case-insensitive t
        eshell-scroll-to-bottom-on-input 'this
        eshell-buffer-shorthand t
        eshell-history-size 1024
        eshell-cmpl-ignore-case t
        eshell-aliases-file (concat user-emacs-directory ".eshell-aliases")
        eshell-last-dir-ring-size 512
        eshell-cd-on-directory nil
        eshell-save-history-on-exit t
        eshell-hist-ignoredups      nil
        eshell-default-target-is-dot t
        eshell-pushd-tohome          t
        eshell-scroll-show-maximum-output nil
        eshell-prompt-function 'ted-eshell-prompt
        eshell-prompt-regexp "^[^#:\n]*[#:] "
        eshell-aliases-file "~/.alias"
        eshell-cmpl-cycle-completions nil)

  (defun ted-eshell-prompt ()
    (let ((user (or (getenv "USER") (user-login-name) "ted"))
          (host (car (split-string
                      (or (getenv "HOST") (system-name) "unknown")
                      "\\.")))
          (char (if (= (user-uid) 0) "#" ":")))
      (format "\n%s@%s%s " user host char)))

  :config
   (add-hook 'shell-mode-hook 'goto-address-mode)
   (add-hook 'eshell-mode-hook
            (lambda ()
              (local-set-key (kbd "C-a") 'eshell-bol)
              (local-set-key (kbd "<up>") 'previous-line)
              (local-set-key (kbd "<down>") 'next-line)))
   )

(provide 'eshell-conf)