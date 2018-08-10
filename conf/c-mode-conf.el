;;; c-mode-conf.el ---

;; Author: bestkindy@gmail.com
;; Keywords:
;; Requirements:
;; Reference: http://cc-mode.sourceforge.net
;;
;; (require 'cc-mode)

(defun my-c-mode-hook ()
    "C mode with adjusted defaults for use with the Linux kernal formatting."
    ;; (interactive)
    ;; (c-mode)
    (c-set-style "stroustrup")
    (c-set-offset 'member-init-intro '++)
    ; brackets should be at same indentation level as the statements they open
    ; if (0)          becomes        if (0)
    ;     {                          {
    ;        ;                           ;
    ;     }                          }
    (c-set-offset 'substatement-open 0)
    (c-set-offset 'inline-open '+)
    (c-set-offset 'block-open '+)
    ;first arg of arglist to functions: tabbed in once
    ;(default was c-lineup-arglist-intro-after-paren)
    (c-set-offset 'arglist-intro '+)
    ; all "opens" should be indented by the c-indent-level
    (c-set-offset 'brace-list-open '+)
    ;switch/case:  make each case line indent from switch
    (c-set-offset 'case-label '+)
    ; hungry-delete and auto-newline
    (c-toggle-auto-hungry-state 1)
    ;; Enabling hungry delete, all whitespace around the cursor will be
    ;; consumed when you press Backspace or C-d
    (c-toggle-hungry-state 1)
    ;make open-braces after a case: statement indent to 0 (default was '+)
    (c-set-offset 'statement-case-open 0)
    (c-toggle-electric-state -1)

    (setq tab-width 4)
    (setq indent-tabs-mode nil) ; Pressing TAB should cause indentation
    (setq c-indent-level 4)   ; A TAB is equivilent to four spaces
    (setq c-basic-offset 4)
    (setq c-continued-statement-offset 4)
    (setq comment-column 40)
    (setq backward-delete-function nil) ; DO NOT expand tabs when deleting
    (setq compile-command "make")
    (setq c-hungry-delete-key t)        ;delete more than one space
    (setq c-macro-shrink-window-flag t)
    (setq c-macro-preprocessor "cpp")
    (setq c-macro-cppflags " ")
    (setq c-macro-prompt-flag t)
    (setq c-style-variables-are-local-p nil)
    ;give me NO newline automatically after electric expressions are entered
    (setq c-auto-newline nil)
    ;syntax-highlight aggressively
    ;(setq font-lock-support-mode 'lazy-lock-mode)
    (setq lazy-lock-defer-contextually t)
    (setq lazy-lock-defer-time 0)
    ;make a #define be left-aligned
    (setq c-electric-pound-behavior '(alignleft))

    ;; minor mode
    (auto-fill-mode 1)
    (hs-minor-mode 1)
    (c-toggle-auto-state 1)

    ;; key binding
    ; (define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
    (local-set-key "\C-m" 'newline-and-indent)
;    (local-set-key  (kbd "C-c o") 'ff-find-other-file)

    (define-key c-mode-base-map [(f7)] 'compile)
  )

;; (add-hook 'c-mode-hook 'my-c-mode-hook)

(defun fix-c-indent-offset-according-to-syntax-context (key val)
  ;; remove the old element
  (setq c-offsets-alist (delq (assoc key c-offsets-alist) c-offsets-alist))
  ;; new value
  (add-to-list 'c-offsets-alist '(key . val)))

;; donot use c-mode-common-hook or cc-mode-hook because many major-modes use this hook
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              ;; indent
              (fix-c-indent-offset-according-to-syntax-context 'substatement 0)
              (fix-c-indent-offset-according-to-syntax-context 'func-decl-cont 0)

              ;; gtags (GNU global) stuff
              ; (setq gtags-suggested-key-mapping t)
              ; (if is-after-emacs-24 (ggtags-mode 1))
              )
            (when (derived-mode-p 'c-mode 'c++-mode)
              (my-c-mode-hook))
            ))

; ;; Whenever you type certain characters, a newline will be inserted automatically
; (add-hook 'c-mode-common-hook '(lambda () (c-toggle-auto-state 1)))

;; sets Linux style only if the filename (or the directory)
;; contains the string “linux” somewhere
(defun maybe-linux-style ()
      (when (and buffer-file-name
                 (string-match "linux" buffer-file-name))
        (c-set-style "Linux")))
(add-hook 'c-mode-hook 'maybe-linux-style)

;; c++ mode setting
(add-hook 'c++-mode-hook 'my-c-mode-hook)


;; eg
;; (c-add-style "my-c-style" '((c-continued-statement-offset 4)))
;; (c-set-style "my-c-style")

;; compilation window
(setq compilation-window-height 8)


;; C/C++ Mode
(use-package cc-mode
  :ensure nil
  :init
  (add-hook 'c-mode-common-hook
            (lambda ()
              (c-set-style "bsd")
              (setq tab-width 4)
              (setq c-basic-offset 4)

              ;; (local-set-key "\C-m" 'reindent-then-newline-and-indent)
              (local-set-key "\C-cc" 'compile))))


;; use google-c-style
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
;; RETURN key goes to the next line and space over to the right place
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
