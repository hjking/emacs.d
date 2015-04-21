
(message "%d: >>>>> Loading [ Helm ] Customization ...." step_no)
(setq step_no (1+ step_no))

; (require 'helm)
; (require 'helm-config)
; (require 'helm-eshell)
; (require 'helm-files)
; (require 'helm-grep)

(use-package helm
  :diminish helm-mode
  :init
  (progn
    (require 'helm-config)
    ;; From https://gist.github.com/antifuchs/9238468
    (setq enable-recursive-minibuffers t
          helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>
          ; helm-quick-update t ; do not display invisible candidates
          ; helm-idle-delay 0.01 ; be idle for this many seconds, before updating in delayed sources.
          ; helm-input-idle-delay 0.01 ; be idle for this many seconds, before updating candidate buffer
          helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.
          ; helm-split-window-default-side 'other ;; open helm buffer in another window. below/above
          helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window
          helm-candidate-number-limit 500 ; limit the number of displayed canidates
          helm-M-x-requires-pattern 0     ; show all candidates when set to 0
          ; helm-boring-file-regexp-list
          ;  '("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "\\.i$") ; do not show these files in helm buffer
          helm-ff-file-name-history-use-recentf t
          helm-ff-skip-boring-files t
          helm-move-to-line-cycle-in-source t ; move to end or beginning of source
                                            ; when reaching top or bottom of source.
          ido-use-virtual-buffers t         ; Needed in helm-buffers-list
          helm-buffers-fuzzy-matching t     ; fuzzy matching buffer names when non--nil
                                            ; useful in helm-mini that lists buffers
          helm-recentf-fuzzy-match t
          helm-M-x-fuzzy-match t            ; optional fuzzy matching for helm-M-x
          helm-display-header-line nil      ; disable the header line
          helm-autoresize-max-height 30
          helm-autoresize-min-height 30

          helm-mini-default-sources '(helm-source-buffers-list
                                      helm-source-recentf
                                      helm-source-bookmarks
                                      helm-source-buffer-not-found)
    )
    ; (set-face-attribute 'helm-source-header nil :height 0.1)
    ; (helm-autoresize-mode 1)    ; auto resizing window
    (ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally
    (helm-mode))
  :config
  (progn
    (global-set-key (kbd "M-x") 'helm-M-x)  ;; replace smex
    (global-set-key (kbd "M-y") 'helm-show-kill-ring)
    (global-set-key (kbd "C-x b") 'helm-mini)
    (global-set-key (kbd "C-x C-f") 'helm-find-files)
    (global-set-key (kbd "C-c h") 'helm-command-prefix)
    (global-unset-key (kbd "C-x c"))
    ; (global-set-key (kbd "C-c h m") 'helm-man-woman)
    ; (global-set-key (kbd "C-c h g") 'helm-do-grep)
    ; (global-set-key (kbd "C-c h f") 'helm-find)
    ; (global-set-key (kbd "C-c h l") 'helm-locate)
    (global-set-key (kbd "C-c h o") 'helm-occur)
    ; (global-set-key (kbd "C-c h r") 'helm-resume)
    ; (global-set-key (kbd "C-h C-f") 'helm-apropos)

    ;; I don't like the way switch-to-buffer uses history, since
    ;; that confuses me when it comes to buffers I've already
    ;; killed. Let's use ido instead.
    (add-to-list 'helm-completing-read-handlers-alist '(switch-to-buffer . ido))
    ;; Unicode
    (add-to-list 'helm-completing-read-handlers-alist '(insert-char . ido))

    (when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p t))

    ;;; Save current position to mark ring
    (add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; helm-swoop - quickly finding lines
    ;; List match lines to another buffer, which is able to squeeze by any
    ;; words you input. At the same time, the original buffer's cursor is
    ;; jumping line to line according to moving up and down the line list

    ; (require 'helm-swoop)

    (autoload 'helm-swoop "helm-swoop" nil t)
    (autoload 'helm-back-to-last-point "helm-swoop" nil t)
    (global-set-key (kbd "C-c h o") 'helm-swoop)
    (global-set-key (kbd "C-c s") 'helm-multi-swoop-all)
    (eval-after-load "helm-swoop"
      '(
        ;; When doing isearch, hand the word over to helm-swoop
        ; (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
        ;; From helm-swoop to helm-multi-swoop-all
        ; (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
        ;; Save buffer when helm-multi-swoop-edit complete
        (setq helm-multi-swoop-edit-save t)
        ;; If this value is t, split window inside the current window
        (setq helm-swoop-split-with-multiple-windows nil)
        ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
        (setq helm-swoop-split-direction 'split-window-vertically)
        ;; If nil, you can slightly boost invoke speed in exchange for text color
        (setq helm-swoop-speed-or-color nil)
        ;; Go to the opposite side of line from the end or beginning of line
        (setq helm-swoop-move-to-line-cycle t)
        ;; Optional face for line numbers
        ;; Face name is `helm-swoop-line-number-face`
        (setq helm-swoop-use-line-number-face t)
        ))
  )
  :bind (("C-c h" . helm-mini)
         ("M-x" . helm-M-x))
  )

(provide 'helm-conf)