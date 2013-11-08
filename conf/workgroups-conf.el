
(require 'workgroups)

;; Set your prefix key
(setq wg-prefix-key (kbd "C-x w")
     wg-no-confirm t
     wg-file (concat my-emacs-dir "workgroups")
     wg-use-faces nil
     wg-switch-on-load nil)
(fset 'wg-mode-line-add-display (lambda () nil))
(fset 'wg-mode-line-remove-display (lambda () nil))
(workgroups-mode 1)

(defun wg-load-default ()
  "Run `wg-load' on `wg-file'."
  (interactive)
  (wg-load wg-file))

(defun wg-save-default ()
  "Run `wg-save' on `wg-file'."
  (interactive)
  (when wg-list
    (with-temp-message ""
      (wg-save wg-file))))

(define-key wg-map (kbd "g") 'wg-switch-to-workgroup)
(define-key wg-map (kbd "C-l") 'wg-load-default)
(define-key wg-map (kbd "C-s") 'wg-save-default)
(define-key wg-map (kbd "<backspace>") 'wg-switch-left)
(add-hook 'auto-save-hook 'wg-save-default)
(add-hook 'kill-emacs-hook 'wg-save-default)

;; See the customization section
;; M-x customize-group RET workgroups RET
