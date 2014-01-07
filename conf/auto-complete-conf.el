;;
;; Filename: auto-complete-conf.el
;; Description: Setting for auto-complete.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2014-01-07 09:32:19
;; available for Emacs 22/23

(message "%d: >>>>> Loading [ auto-complete ] Customizations ...." step_no)
(setq step_no (1+ step_no))

(require 'auto-complete-config)
(ac-config-default)

(setq-default ac-expand-on-auto-complete t)

; Not to complete automatically, need to trigger
(setq ac-auto-start t)
;; (ac-set-trigger-key "TAB")
(setq-default ac-dwim nil) ; To get pop-ups with docs even if a word is uniquely completed
;;(require 'auto-complete-etags)
;;(require 'auto-complete-extension)
;;(require 'auto-complete-octave)
;;(require 'auto-complete-verilog)
;;(require 'auto-complete-yasnippet)

;;----------------------------------------------------------------------------
;; Use Emacs' built-in TAB completion hooks to trigger AC (Emacs >= 23.2)
;;----------------------------------------------------------------------------
(setq tab-always-indent 'complete)  ;; use 't when auto-complete is disabled
(add-to-list 'completion-styles 'initials t)

;; height of completion menu
(setq ac-menu-height 20)    ;; 20 lines

;; add mode to auto-complete mode
;(add-to-list 'ac-modes 'new-mode)
(setq ac-modes
      (append ac-modes '(org-mode objc-mode jde-mode sql-mode
                                  change-log-mode text-mode
                                  makefile-gmake-mode makefile-bsdmake-mo
                                  autoconf-mode makefile-automake-mode)))

;; Ignore case
;; (setq ac-ignore-case t)
;; Ignore case if completion target string doesn't include upper characters
(setq ac-ignore-case 'smart)
;; Distinguish case
;; (setq ac-ignore-case nil)

;; color
(set-face-background 'ac-candidate-face "lightgray")
(set-face-underline 'ac-candidate-face "darkgray")
(set-face-background 'ac-selection-face "steelblue")

(setq ac-use-menu-map t)
(setq ac-candidate-menu-height 20)
(setq ac-candidate-max ac-candidate-menu-height)
;; Only when completion menu is displayed
(define-key ac-menu-map "\C-n" 'ac-next)      ;; default
(define-key ac-menu-map "\C-p" 'ac-previous)  ;; default

;; Quick help
(setq ac-use-quick-help t)

;; delay time of show menu
(setq ac-auto-show-menu 1)
(setq ac-comphist-file (concat my-cache-dir "ac-comphist.dat"))
;; dictionary directories
;(setq auto-comp-dict-load-path (concat auto-comp-load-path "dict"))
;;(add-to-list 'ac-dictionary-directories auto-comp-dict-load-path)

;; User defined dictionary files
;; (add-to-list 'ac-user-dictionary-files "~/.dict")   ;; default

(set-default 'ac-sources
               '(ac-source-semantic
               	 ac-source-functions
                 ac-source-yasnippet
                 ac-source-abbrev
                 ac-source-variables
                 ac-source-symbols
                 ac-source-features
                 ac-source-dictionary
                 ac-source-words-in-buffer
                 ac-source-words-in-all-buffer
                 ac-source-imenu
                 ac-source-files-in-current-dir
                 ac-source-filename
                 ac-source-words-in-same-mode-buffers))

(dolist (command `(backward-delete-char-untabify delete-backward-char))
    (add-to-list 'ac-trigger-commands command))
;; Change default sources
;; (setq-default ac-sources '(ac-source-words-in-all-buffer))
(add-hook 'auto-complete-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-filename)))

;; key binding
;;  ;; Use M-n/M-p to select candidates
;; (global-set-key "\M-/" 'auto-complete)
;; (define-key ac-mode-map (kbd "M-TAB")             'auto-complete)
;; (define-key ac-mode-map (kbd "C-\\") 'auto-complete-selective)
(global-set-key "\M-]" 'auto-complete)
;; (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
;; (define-key ac-mode-map (kbd "M-]") 'auto-complete-selective)

;; Operation of completions
;; (define-key ac-completing-map 	 (kbd "C-\\") 	  'ac-complete)
;; (define-key ac-completing-map    "\M-/"           'ac-stop)
;;
;; (define-key ac-complete-mode-map (kbd "<return>") 'ac-complete)
