
;; My minor mode
;; Main use is to have my key bindings have the highest priority

(defvar hjking-special-keymap-prefix (kbd "C-x h")
  "`hjking-mode' keymap prefix.
Overrides the default binding for `compose-mail'.")

(defvar hjking-mode-special-map (make-sparse-keymap)
  "Special keymap for `hjking-mode' whose bindings begin with
`hjking-special-keymap-prefix'.")
(fset 'hjking-mode-special-map hjking-mode-special-map)

(defvar hjking-mode-map (let ((map (make-sparse-keymap)))
                        (define-key map hjking-special-keymap-prefix 'hjking-mode-special-map)
                        map)
  "Keymap for `hjking-mode'.")

;;;###autoload
(define-minor-mode hjking-mode
  "A minor mode so that my key settings override annoying major modes."
  ;; If init-value is not set to t, this mode does not get enabled in
  ;; `fundamental-mode' buffers even after doing \"(global-hjking-mode 1)\".
  ;; More info: http://emacs.stackexchange.com/q/16693/115
  :init-value t
  :lighter    " HJ"
  :keymap     hjking-mode-map)

;;;###autoload
(defun turn-on-hjking-mode ()
  "Turn on hjking-mode."
  (interactive)
  (hjking-mode 1))

(defun turn-off-hjking-mode ()
  "Turn off hjking-mode."
  (interactive)
  (hjking-mode -1))

;; Turn off the minor mode in the minibuffer
(add-hook 'minibuffer-setup-hook #'turn-off-hjking-mode)


(provide 'hjking-mode)