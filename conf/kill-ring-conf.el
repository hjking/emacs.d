
(message "%d: >>>>> Loading [ browse-kill-ring ] Customizations ...." step_no)
(setq step_no (1+ step_no))

(require 'browse-kill-ring)
;; (require 'browse-kill-ring+)
;; string separating entries in the `separated' style
(setq browse-kill-ring-separator
      "\n--separator------------------------------")
;; temporarily highlight the inserted `kill-ring' entry
(setq browse-kill-ring-highlight-inserted-item t)
;; face in which to highlight the `browse-kill-ring-separator'
(defface separator-face '((t (:foreground "Blueviolet" :weight bold))) nil)
(setq browse-kill-ring-separator-face 'separator-face)
(setq browse-kill-ring-quit-action 'save-and-restore)
;; use `M-y' to invoke `browse-kill-ring'
(browse-kill-ring-default-keybindings)
;; map `browse-kill-ring' to another key combination
;; (global-set-key (kbd "C-c y") 'browse-kill-ring)
;; (global-set-key "\C-cy" 'browse-kill-ring)

;;
(require 'browse-kill-ring+)
