;; ----------------------------------------------------------------------------
;; Paredit
;; ----------------------------------------------------------------------------
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; Use paredit in the minibuffer
(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)

(defvar paredit-minibuffer-commands '(eval-expression
                                      pp-eval-expression
                                      eval-expression-with-eldoc
                                      ibuffer-do-eval
                                      ibuffer-do-view-and-eval)
  "Interactive commands for which paredit should be enabled in the minibuffer.")

(defun conditionally-enable-paredit-mode ()
  "Enable paredit during lisp-related minibuffer commands."
  (if (memq this-command paredit-minibuffer-commands)
      (enable-paredit-mode)))


(defun maybe-map-paredit-newline ()
  (unless (or (eq major-mode 'inferior-emacs-lisp-mode) (minibufferp))
    (local-set-key (kbd "RET") 'paredit-newline)))

(add-hook 'paredit-mode-hook 'maybe-map-paredit-newline)