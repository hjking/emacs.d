;;
;; Filename: auto-complete-conf.el
;; Description: Setting for auto-complete.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2014-02-12 11:10:57
;; available for Emacs 22/23

;; start auto-complete
(require 'auto-complete)
;; do default config for auto-complete
(require 'auto-complete-config)
(ac-config-default)

; ;; Stop completion
;   (define-key ac-completing-map "\M-/" 'ac-stop)

;   ;; Finish completion by TAB
;   (define-key ac-completing-map "\t" 'ac-complete)
;   (define-key ac-completing-map "\r" nil)   ;; not Enter

; (define-key ac-menu-map "\C-n" 'ac-next)      ;; default
; (define-key ac-menu-map "\C-p" 'ac-previous)  ;; default

(defun auto-complete-setting ()
  "Setting for `auto-complete'."
  (setq ac-expand-on-auto-complete t)

  ;;;==== Not to complete automatically, need to trigger
  ;; (setq ac-auto-start nil)
  ;; start completion automatically when inserted 4 or more characters
  (setq ac-auto-start 4)
  ;; trigger auto complete by ALT-]
  (global-set-key "\M-]" 'auto-complete)
  (ac-set-trigger-key  "\M-]") ;; trigger key when ac-auto-start=nil

  ;;;==== Not to show completion menu automatically
  ;; delay time of show menu
  (setq ac-auto-show-menu 0.8)
  (setq ac-delay 0.5)

  ;; Stop completion
  (define-key ac-completing-map "\M-/" 'ac-stop)

  ;; Finish completion by TAB
  (define-key ac-completing-map "\t" 'ac-complete)
  (define-key ac-completing-map "\r" nil)   ;; not Enter
  (define-key ac-completing-map "\C-e" 'ac-complete)

  ;; Select candidates
  (setq ac-use-menu-map t)
  ;; Only select candidates with C-n/C-p only when completion menu is displayed
  (define-key ac-menu-map "\C-n" 'ac-next)      ;; default
  (define-key ac-menu-map "\C-p" 'ac-previous)  ;; default
  (define-key ac-menu-map "\r" 'ac-complete)

  ;; height of completion menu
  (setq ac-menu-height 10)

  ;; add mode to auto-complete mode
  (dolist (mode '(magit-log-edit-mode log-edit-mode org-mode text-mode haml-mode
                sass-mode yaml-mode csv-mode espresso-mode haskell-mode
                html-mode nxml-mode sh-mode smarty-mode clojure-mode
                lisp-mode textile-mode markdown-mode tuareg-mode
                sass-mode scss-mode ruby-mode verilog-mode vlog-mode
                js2-mode css-mode rhtml-mode prog-mode))
  (add-to-list 'ac-modes mode))

  ;; Just ignore case
  ;; (setq ac-ignore-case t)
  ;; Ignore case if completion target string doesn't include upper characters
  (setq ac-ignore-case 'smart)  ;; default

  (setq ac-stop-words '("/" "//" "/*" "//*" "///" "////"))
  (setq ac-use-fuzzy t) ;; enable fuzzy auto complete

  ;; color
  ; (set-face-background 'ac-candidate-face "lightgray")
  ; (set-face-underline 'ac-candidate-face "darkgray")
  ; (set-face-background 'ac-selection-face "steelblue")

  (setq ac-candidate-menu-height 10)
  (setq ac-candidate-max ac-candidate-menu-height)

  ;; Quick help
  (setq ac-use-quick-help nil)

  (setq ac-comphist-file (concat my-cache-dir "ac-comphist.dat"))
  ;; dictionary directories
  ;(setq auto-comp-dict-load-path (concat auto-comp-load-path "dict"))
  ;;(add-to-list 'ac-dictionary-directories auto-comp-dict-load-path)

  ;; Change default sources
  (set-default 'ac-sources
               '(ac-source-functions
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

  ;; Change sources for specific major modes
  (add-hook 'c++-mode (lambda () (add-to-list 'ac-sources 'ac-source-semantic)))

  (dolist (command `(backward-delete-char-untabify delete-backward-char))
    (add-to-list 'ac-trigger-commands command)))

(with-eval-after-load 'auto-complete
  (auto-complete-setting))


;; auto-complete-c-headers
(defun my/ac-c-header-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers))
(add-hook 'c++-mode-hook 'my/ac-c-header-init)
(add-hook 'c-mode-hook 'my/ac-c-header-init)
