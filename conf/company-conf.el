
;; Use M-n, M-p, <tab> and <tab> to complete.
;; Search through the completions with C-s, C-r and C-o.

;; Once installed, enable company-mode with M-x company-mode.
;; Use M-n, M-p, <tab> and <return> to complete.
;; Search through the completions with C-s, C-r and C-o.
;; Type `M-x company-complete` to initiate completion manually.
;; Bind this command to a key combination of your choice.
;; When the completion candidates are shown, press <f1> to display the documentation for the selected candidate, or C-w to see its source. Not all back-ends support this.
;; To use company-mode in all buffers, add the following line to your init file:
;; (add-hook 'after-init-hook 'global-company-mode)
;;
;; To see or change the list of enabled back-ends, type M-x customize-variable RET company-backends. Also see its description for information on writing a back-end.
;; For information on specific back-ends, also check out the comments inside the respective files.
;; For more information, type M-x describe-function RET company-mode.
;; To customize other aspects of its behavior, type M-x customize-group RET company.

(use-package company
  :defer 2
  :ensure t
  :diminish company-mode
  :bind (("\M-]" . company-complete)
         :map company-active-map
          ("C-p" . company-select-previous)
          ("C-n" . company-select-next)
          ("C-d" . company-show-doc-buffer)
          ("<tab>" . company-complete)
         ;; ("<tab>" . company-complete-selection)
         :map company-search-map
          ("C-p" . company-select-previous)
          ("C-n" . company-select-next)
        )
  :commands (company-mode
             company-complete
             company-complete-common
             company-complete-common-or-cycle
             company-files
             company-dabbrev
             company-ispell
             company-c-headers
             company-jedi
             company-tern
             company-web-html
             company-auctex)
  :config
  (progn
   (setq company-idle-delay 0.1
         ;; min prefix of 1 chars
         company-minimum-prefix-length 2
         company-require-match 0
         company-selection-wrap-around t
         company-show-numbers t
         company-dabbrev-downcase nil
         company-transformers '(company-sort-by-occurrence)
         company-tooltip-limit 10
         company-minimum-prefix-length 2
         company-echo-delay 0
         company-auto-complete nil
         company-begin-commands '(self-insert-command)
         ;; aligns annotation to the right hand side
         company-tooltip-align-annotations t)
   (add-hook 'prog-mode-hook 'company-mode)
   ; (add-hook 'after-init-hook 'global-company-mode)

   ;; Default colors are awful - borrowed these from gocode (thanks!):
   ;; https://github.com/nsf/gocode/tree/master/emacs-company#color-customization
   (set-face-attribute
    'company-preview nil :foreground "black" :underline t)
   (set-face-attribute
    'company-preview-common nil :inherit 'company-preview)
   (set-face-attribute
    'company-tooltip nil :background "lightgray" :foreground "black")
   (set-face-attribute
    'company-tooltip-selection nil :background "steelblue" :foreground "white")
   (set-face-attribute
    'company-tooltip-common nil :foreground "darkgreen" :weight 'bold)
   (set-face-attribute
    'company-tooltip-common-selection nil :foreground "black" :weight 'bold)

   (add-to-list 'company-backends 'company-dabbrev t)
   (add-to-list 'company-backends 'company-ispell t)
   (add-to-list 'company-backends 'company-files t)
   (add-to-list 'company-backends 'company-semantic t)
   (add-to-list 'company-backends 'company-c-headers)

   (delete 'company-ropemacs company-backends)
   (delete 'company-capf company-backends)
   (delete 'company-clang company-backends)
   (delete 'company-semantic company-backends)))

;; Auto-completion for C/C++ headers using Company
;; https://github.com/randomphrase/company-c-headers
(use-package company-c-headers
  :defer t
  :init
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode)
                  (add-to-list 'company-backends 'company-c-headers)))))

(provide 'company-conf)