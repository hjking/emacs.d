;; -*- lexical-binding: t -*-
;;

;;;;;;;;;;;;;;
;; electric.el
;;  indent automatically (from 24.4)
(electric-indent-mode 1)
;; set default tab char's display width to 4 spaces
(setq-default tab-width 4)
;; set current buffer's tab char's display width to 4 spaces
(setq tab-width 4)
;; Use spaces, not tabs
(setq-default indent-tabs-mode nil)

(setq backward-delete-char-untabify nil)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;;; indent-guide
;; `C-M-\' runs the command `indent-region' (which does the job of
;; the imaginary command `unsuck-html-layout' in `html-mode')

(use-package indent-guide
  :init
  (add-hook 'prog-mode-hook (lambda () (indent-guide-mode 1))))


;;; aggressive-indent-mode
;; https://github.com/Malabarba/aggressive-indent-mode
;; aggressive-indent-mode is a minor mode that keeps your code always indented.
;; It reindents after every change, making it more reliable than electric-indent-mode.
(use-package aggressive-indent
  ; :disabled t
  :defer 2
  :diminish aggressive-indent-mode
  :config
    (unbind-key "C-c C-q" aggressive-indent-mode-map)
    (defvar hjking/aggressive-indent-mode-hooks '(emacs-lisp-mode-hook)
        "List of hooks of major modes in which aggressive-indent-mode should be enabled.")

    (defun hjking/turn-on-aggressive-indent-mode ()
      "Turn on aggressive-indent-mode only for specific modes."
      (interactive)
      (dolist (hook hjking/aggressive-indent-mode-hooks)
        (add-hook hook #'aggressive-indent-mode)))

    (defun hjking/turn-off-aggressive-indent-mode ()
      "Turn off aggressive-indent-mode only for specific modes."
      (interactive)
      (dolist (hook hjking/aggressive-indent-mode-hooks)
        (remove-hook hook #'aggressive-indent-mode)))

    (hjking/turn-on-aggressive-indent-mode)
  )

(provide 'indent-conf)