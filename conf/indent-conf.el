
(message "%d: >>>>> Loading [ Indentation ] Customization ...." step_no)
(setq step_no (1+ step_no))
;;;;;;;;;;;;;;
;; electric.el
;;  indent automatically (from 24.1)
(electric-indent-mode 1)
;; set default tab char's display width to 4 spaces
(setq-default tab-width 4)
;; set current buffer's tab char's display width to 4 spaces
(setq tab-width 4)
;; Use spaces, not tabs
(setq-default indent-tabs-mode nil)
;; a single space does end a sentence
(setq sentence-end-double-space nil)

(setq backward-delete-char-untabify nil)

;; make tab key always call a indent command.
;; (set-default tab-always-indent t)

;; make tab key call indent command or insert tab character, depending on cursor position
(set-default tab-always-indent nil)

;; make tab key do indent first then completion.
;; (set-default tab-always-indent 'complete)

;; `C-M-\' runs the command `indent-region' (which does the job of
;; the imaginary command `unsuck-html-layout' in `html-mode')

(require 'indent-guide)
(add-hook 'prog-mode-hook (lambda () (indent-guide-mode 1)))

;;; aggressive-indent-mode
(global-aggressive-indent-mode 1)
(add-to-list 'aggressive-indent-excluded-modes 'html-mode)