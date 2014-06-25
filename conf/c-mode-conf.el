;;; c-mode-conf.el ---

;; Author: bestkindy@gmail.com
;; Keywords:
;; Requirements:
;; Reference: http://cc-mode.sourceforge.net
;;
(message "%d: >>>>> Loading [ C Mode ] Customizations ...." step_no)
(setq step_no (1+ step_no))

(require 'cc-mode)
(c-set-offset 'member-init-intro '++)
(c-set-offset 'substatement-open 0) ; brackets should be at same indentation level as the statements they open
(c-set-offset 'inline-open '+)
(c-set-offset 'block-open '+)
(c-set-offset 'arglist-intro '+)
(c-set-offset 'brace-list-open '+)   ; all "opens" should be indented by the c-indent-level
(c-set-offset 'case-label '+)        ; indent case labels by c-indent-level, too
(c-toggle-auto-hungry-state 1)       ; hungry-delete and auto-newline

;; Enabling hungry delete, all whitespace around the cursor will be
;; consumed when you press Backspace or C-d
(c-toggle-hungry-state 1)


(defun my-c-mode-hook ()
    "C mode with adjusted defaults for use with the Linux kernal formatting."
    ;; (interactive)
    ;; (c-mode)
    (c-set-style "stroustrup")
    (setq tab-width 4)
    (setq indent-tabs-mode t) ; Pressing TAB should cause indentation
    (setq c-indent-level 4)   ; A TAB is equivilent to four spaces
    (setq c-basic-offset 4)
    (c-toggle-electric-state -1)
    (setq c-continued-statement-offset 4)
    (setq comment-column 40)
    (setq backward-delete-function nil) ; DO NOT expand tabs when deleting
    (setq compile-command "make")
    (setq c-hungry-delete-key t)        ;delete more than one space
    (setq c-macro-shrink-window-flag t)
    (setq c-macro-preprocessor "cpp")
    (setq c-macro-cppflags " ")
    (setq c-macro-prompt-flag t)

    ;; minor mode
    (auto-fill-mode 1)
    (hs-minor-mode 1)
    (c-toggle-auto-state 1)

    ;; key binding
    (define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

    (define-key c-mode-base-map [(f7)] 'compile)

  )

(add-hook 'c-mode-hook 'my-c-mode-hook)

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


;; use google-c-style
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
;; RETURN key goes to the next line and space over to the right place
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
