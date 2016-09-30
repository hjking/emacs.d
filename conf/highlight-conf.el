
(message "%d: >>>>> Loading [ Highlight ] Customization ...." step_no)
(setq step_no (1+ step_no))

;; syntax highlight everywhere
;; (global-font-lock-mode t)
(if (fboundp 'global-font-lock-mode)
     (global-font-lock-mode 1)          ; turn on syntax coloring
     (setq font-lock-auto-fontify t))   ; XEmacs'))
(setq font-lock-maximum-decoration t)
;;(setq font-lock-global-modes '(not text-mode))
;;(setq font-lock-verbose t)
;;(setq font-lock-maximum-size '((t . 1048576) (vm-mode . 5250000)))

;; Get more highlight
(require 'generic-x nil t)

;; Highlight tabulations
(setq-default highlight-tabs t)

(use-package hi-lock
  :init
   ;; Don't scan the file beyond 1000 characters to look for the Hi-Lock patterns.
   (setq hi-lock-file-patterns-range 1000)
  :config
   (global-hi-lock-mode 1))

;; Highlight Anything
(use-package hl-anything
  :load-path (lambda () (concat my-site-lisp-dir "hl-anything/"))
  :commands (hl-global-highlight-on
             hl-highlight-thingatpt-global
             hl-highlight-thingatpt-local
             hl-find-next-thing
             hl-find-prev-thing
             hl-save-highlights
             hl-restore-highlights
             hl-global-highlight-on/off)
  :init (progn
         (bind-key  "h"  #'hjking-hydra-hl-anything/body hjking-mode-map)

         (defun my/hl-anything (&optional arg)
             "Wrapper function to call functions to highlight the thing at point either
         globally or locally (when called with prefix `C-u')."
             (interactive "p")
             (if (eq arg 4)
                 (hl-highlight-thingatpt-local)
               (hl-highlight-thingatpt-global)))

         (defun my/unhl-anything (&optional arg)
             "Wrapper function to call functions to unhighlight all either
         globally or locally (when called with prefix `C-u')."
             (interactive "p")
             (if (eq arg 4)
                 (hl-unhighlight-all-local)
               (hl-unhighlight-all-global)))

         (defhydra hjking-hydra-hl-anything (:color red :hint nil)
           "
            hl-anything
        ^Highlight^          ^UnHighlight^        ^Next/Prev^        ^Misc
        ^^^^^^^^-----------------------------------------------------------------
        _H_: global          _U_: unhl-global     _n_: next          _s_: save
        _h_: local           _h_: unhl-local      _p_: prev          _r_: restore
        "
           ("H" my/hl-anything             )
           ("h" (my/hl-anything 4)         )
           ("U" my/unhl-anything           )
           ("u" (my/unhl-anything 4)       )
           ("n" hl-find-next-thing         )
           ("p" hl-find-prev-thing         )
           ("s" hl-save-highlights         )
           ("r" hl-restore-highlights      )
           ("t" hl-global-highlight-on/off "on/off" :color blue)
           ("q" nil                        "cancel" :color blue)
           )
         )
  :config (progn
           (hl-highlight-mode +1)
           (hl-paren-mode +1)
           )
  )

;; Highlight search pattern
;; https://github.com/nschum/highlight-symbol.el
;; Highlight Symbol at point/cursor
(use-package highlight-symbol
  :load-path (lambda () (concat my-site-lisp-dir "highlight-symbol/"))
  :commands (highlight-symbol-at-point
             highlight-symbol-next
             highlight-symbol-prev
             highlight-symbol-query-replace)
  :init (progn
         ; (bind-key "a" 'highlight-symbol-at-point hjking-mode-map) ;; C-x m a
         ; (bind-key "n" 'highlight-symbol-next hjking-mode-map) ;; C-x m n
         ; (bind-key "p" 'highlight-symbol-prev hjking-mode-map) ;; C-x m n
         ; (bind-key "r" 'highlight-symbol-query-replace hjking-mode-map) ;; C-x m r
         ; (bind-key  "l"  'hydra-highlight-symbol/body hjking-mode-map)
         (bind-key "l" #'hjking-hydra-highlight-symbol/body hjking-mode-map)

         (setq highlight-symbol-on-navigation-p t)
         (setq highlight-symbol-idle-delay 0.5)

         (defhydra hjking-hydra-highlight-symbol
           (:color red  :hint nil :post (progn (highlight-symbol-remove-all)))
           "
              highlight-symbol

        ^Highlight^          ^Next^              ^Prev
        ^^^^^^^^--------------------------------------------
        _._: highlight       _n_: next           _p_: prev
          ^ ^                _N_: next in defun  _P_: prev in defun
        "
           ("." highlight-symbol-at-point)
           ("n" highlight-symbol-next)
           ("p" highlight-symbol-prev)
           ("N" highlight-symbol-next-in-defun)
           ("P" highlight-symbol-prev-in-defun)
           ("r" highlight-symbol-query-replace  "replace" :color red)
           ("q" nil                             "cancel" :color blue))
         )
  :config
   (highlight-symbol-mode)
  )


;; Highlight Global
;; Source: https://github.com/glen-dai/highlight-global
;; highlights all matches accross ALL buffer.
;; highlight-frame-toggle: highlit/unhighlight the target
;; clear-highlight-frame:  unhighlights all highlighted target

;; Alternative highlighting package when `hl-anything' has issues
(use-package highlight-global
  :disabled t
  :load-path (lambda () (concat my-site-lisp-dir "highlight-global/"))
  :commands (highlight-frame-toggle
             clear-highlight-frame)
  ; :bind (("M-H"  .  highlight-frame-toggle)
  ;        ("M-+"  .  clear-highlight-frame))
  :init (progn
         (bind-key  "h"  'highlight-frame-toggle hjking-mode-map) ;; C-x m h
         (bind-key  "c"  'clear-highlight-frame hjking-mode-map) ;; C-x m H
         )
  )


;; highlight changes made by commands such as undo, yank-pop, etc.
(use-package volatile-highlights
  :load-path (lambda () (concat my-site-lisp-dir "volatile-highlights/"))
  :config
   (volatile-highlights-mode t))


;;; Highlight specified words ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; special words
(setq keywords-critical-pattern
      "\\(BUGS\\|FIXME\\|[Tt][Oo][Dd][Oo]\\|XXX\\|[Ee][Rr][Rr][Oo][Rr]\\|[Mm][Ii][Ss][Ss][Ii][Nn][Gg]\\|[Ii][Nn][Vv][Aa][Ll][Ii][Dd]\\|[Ff][Aa][Ii][Ll][Ee][Dd]\\|[Cc][Oo][Rr][Rr][Uu][Pp][Tt][Ee][Dd]\\)")
(make-face 'keywords-critical)
(GNUEmacs (set-face-attribute 'keywords-critical nil
                              :foreground "red" :background "yellow"
                              :weight 'bold))

(setq keywords-normal-pattern "\\([Ww][Aa][Rr][Nn][Ii][Nn][Gg]\\)")
(make-face 'keywords-normal)
(GNUEmacs (set-face-attribute 'keywords-normal nil
                              :foreground "magenta2" :background "yellow"))

;; set up highlighting of special words for proper selected major modes only
(dolist (mode '(fundamental-mode
                svn-log-view-mode
                text-mode
                c-mode
                java-mode
                verilog-mode
                vlog-mode
                python-mode
                cperl-mode
                html-mode-hook
                prog-mode-hook
                css-mode-hook
                emacs-lisp-mode))  ; no interference with Org-mode (which derives from text-mode)
  (font-lock-add-keywords mode
    `((,keywords-critical-pattern 1 'keywords-critical prepend)
      (,keywords-normal-pattern 1 'keywords-normal prepend))))

;; add fontification patterns (even in comments) to a selected major mode
;; *and* all major modes derived from it
(defun fontify-keywords ()
  (interactive)
;;;   (font-lock-mode -1)
;;;   (font-lock-mode 1)
  (font-lock-add-keywords nil
    `((,keywords-critical-pattern 1 'keywords-critical prepend)
      (,keywords-normal-pattern 1 'keywords-normal prepend))))

;; set up highlighting of special words for selected major modes *and* all
;; major modes derived from them
(dolist (hook '(c++-mode-hook
                c-mode-hook
                change-log-mode-hook
                cperl-mode-hook
                css-mode-hook
                emacs-lisp-mode-hook
                html-mode-hook
                java-mode-hook
                latex-mode-hook
                lisp-mode-hook
                makefile-mode-hook
                message-mode-hook
                php-mode-hook
                python-mode-hook
                sh-mode-hook
                shell-mode-hook
                verilog-mode-hook
                vlog-mode-hook
                prog-mode-hook
                ssh-config-mode-hook))
  (add-hook hook 'fontify-keywords))

;; Highlight Hex
(defvar hexcolor-keywords
    '(("#[ABCDEFabcdef[:digit:]]\\{6\\}"
        (0 (put-text-property (match-beginning 0)
                              (match-end 0)
                              'face (list :background
                                          (match-string-no-properties 0)))))))

(defun hexcolor-add-to-font-lock ()
    (interactive)
    (font-lock-add-keywords nil hexcolor-keywords))

(defun add-to-hooks (action &rest hooks)
    (mapc #'(lambda (x)
              (add-hook x action)) hooks))

(add-to-hooks 'hexcolor-add-to-font-lock
              'css-mode-hook
              'php-mode-hook
              'html-mode-hook
              'shell-script-mode
              'shell-mode-hook
              'emacs-lisp-mode-hook
              'text-mode-hook
              'haskell-mode-hook)

(provide 'highlight-conf)

;; --[ Highlight ]-----------------------------------------------------[ End ]--