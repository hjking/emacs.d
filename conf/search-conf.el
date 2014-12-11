
(message "%d: >>>>> Loading [ Search and Replace Customization ] ...." step_no)
(setq step_no (1+ step_no))
;; highlight during searching
(setq query-replace-highlight t)
;; highlight incremental search
(setq search-highlight t)
;; Non-nil if searches should ignore case
(setq case-fold-search t)
;; always exit searches at the beginning of the expression found
(add-hook 'isearch-mode-end-hook 'custom-goto-match-beginning)
(defun custom-goto-match-beginning ()
  "Use with isearch hook to end search at first char of match."
  (when isearch-forward (goto-char isearch-other-end)))

(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

;; Search back/forth for the symbol at point
;; See http://www.emacswiki.org/emacs/SearchAtPoint
(defun isearch-yank-symbol ()
  "*Put symbol at current point into search string."
  (interactive)
  (let ((sym (symbol-at-point)))
    (if sym
        (progn
          (setq isearch-regexp t
                isearch-string (concat "\\_<" (regexp-quote (symbol-name sym)) "\\_>")
                isearch-message (mapconcat 'isearch-text-char-description isearch-string "")
                isearch-yank-flag t))
      (ding)))
  (isearch-search-and-update))

(define-key isearch-mode-map "\C-\M-w" 'isearch-yank-symbol)


;; `M-x flush-lines' deletes each line that contains a match for REGEXP

;; *** Grep search
;; ignore case distinctions in the default grep command
(setq grep-command "grep -n -i -e ")

;; I-search with initial contents.
;; original source: http://platypope.org/blog/2007/8/5/a-compendium-of-awesomeness
(defvar isearch-initial-string nil)

(defun isearch-set-initial-string ()
  (remove-hook 'isearch-mode-hook 'isearch-set-initial-string)
  (setq isearch-string isearch-initial-string)
  (isearch-search-and-update))

(defun isearch-forward-at-point (&optional regexp-p no-recursive-edit)
  "Interactive search forward for the symbol at point."
  (interactive "P\np")
  (if regexp-p (isearch-forward regexp-p no-recursive-edit)
    (let* ((end (progn (skip-syntax-forward "w_") (point)))
           (begin (progn (skip-syntax-backward "w_") (point))))
      (if (eq begin end)
          (isearch-forward regexp-p no-recursive-edit)
          (setq isearch-initial-string (buffer-substring begin end))
          (add-hook 'isearch-mode-hook 'isearch-set-initial-string)
          (isearch-forward regexp-p no-recursive-edit)))))

;;; anzu
(require 'anzu)
(global-anzu-mode +1)
(diminish 'anzu-mode)
; (global-set-key (kbd "M-%") 'anzu-query-replace)
; (global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

;; visual-regexp
(require 'visual-regexp)
;; (define-key global-map (kbd "C-c r") 'vr/replace)
;; (define-key global-map (kbd "C-c q") 'vr/query-replace)
;; ;; if you use multiple-cursors, this is for you:
;; (define-key global-map (kbd "C-c m") 'vr/mc-mark)

(require 'isearch-extension)

(provide 'search-conf)