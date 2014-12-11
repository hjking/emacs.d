
(message "%d: >>>>> Loading [ browse-kill-ring ] Customizations ...." step_no)
(setq step_no (1+ step_no))

; (require 'browse-kill-ring)
; ;; (require 'browse-kill-ring+)
; ;; use `M-y' to invoke `browse-kill-ring'
; (browse-kill-ring-default-keybindings)

; ;;
; (require 'browse-kill-ring+)

; (defun browse-kill-ring-settings ()
;   "Settings for `browse-kill-ring'."
;   ;; string separating entries in the `separated' style
;   (setq browse-kill-ring-separator "\n--separator------------------------------")
;   ;; temporarily highlight the inserted `kill-ring' entry
;   (setq browse-kill-ring-highlight-inserted-item t)
;   ;; face in which to highlight the `browse-kill-ring-separator'
;   (defface separator-face '((t (:foreground "Blueviolet" :weight bold))) nil)
;   (setq browse-kill-ring-separator-face 'separator-face)
;   (setq browse-kill-ring-quit-action 'save-and-restore)
;   ;; map `browse-kill-ring' to another key combination
;   ;; (global-set-key (kbd "C-c y") 'browse-kill-ring)
;   ;; (global-set-key "\C-cy" 'browse-kill-ring)

;   (add-hook 'browse-kill-ring-hook 'browse-kill-ring-my-keys)

;   (defun browse-kill-ring-my-keys ()
;   (let ((map browse-kill-ring-mode-map))
;     (define-key-list
;      map
;      `(("RET" browse-kill-ring-insert-and-quit)
;        ("<"   beginning-of-buffer)
;        (">"   end-of-buffer)
;        ("j"   next-line)
;        ("k"   previous-line)
;        ("h"   backward-char)
;        ("l"   forward-char)
;        ("n"   browse-kill-ring-forward-without-linum-mode)
;        ("p"   browse-kill-ring-previous-without-linum-mode)
;        ("SPC" scroll-up)
;        ("U"   scroll-down)
;        ("u"   View-scroll-half-page-backward)
;        ("o"   other-window))))))

; (eval-after-load "browse-kill-ring"
;   `(browse-kill-ring-settings))


(use-package browse-kill-ring
  :init
  (progn
    (browse-kill-ring-default-keybindings) ;; M-y
    (setq browse-kill-ring-separator "\n--separator------------------------------")
    ;; temporarily highlight the inserted `kill-ring' entry
    (setq browse-kill-ring-highlight-inserted-item t)
    ;; face in which to highlight the `browse-kill-ring-separator'
    (defface separator-face '((t (:foreground "Blueviolet" :weight bold))) nil)
    (setq browse-kill-ring-separator-face 'separator-face)
    (setq browse-kill-ring-quit-action 'save-and-restore))
  )

(provide 'kill-ring-conf)