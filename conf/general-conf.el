
;; https://github.com/noctuid/general.el

;; provides a more convenient way to bind keys in emacs
;; for both evil and non-evil users.

(use-package general
  :config
  (general-define-key
    ;; replace default keybindings
    ; "C-'" 'avy-goto-word-1
    "C-s" 'swiper             ; search for string in current buffer
    "M-x" 'counsel-M-x        ; replace default M-x with ivy backend
    )

  ; (general-define-key
  ;   :prefix "C-c"
  ;   ;; bind to simple key press
  ;    "b"   'ivy-switch-buffer  ; change buffer, chose using ivy
  ;    "/"   'counsel-git-grep   ; find string in git project
  ;    ;; bind to double key press
  ;    "f"   '(:ignore t :which-key "files")  ; Which-key integration to General
  ;    "ff"  'counsel-find-file  ; find file using ivy
  ;    "fr"  'counsel-recentf    ; find recently edited files
  ;    "p"   '(:ignore t :which-key "project")
  ;    "pf"  'counsel-git        ; find file in git project
  ;    )

  ;; bind a key in a specific keymap (keymaps must be quoted)
  (general-define-key :keymaps 'org-mode-map
                      "TAB" 'org-cycle)

  ;; if you prefer an explicit (kbd) or don't want (kbd) at all:
  ; (setq general-implicit-kbd nil)
  ; (general-define-key
  ;  (kbd "C-c a") 'some-command
  ;  (kbd "C-c b") 'another-command)
  )

(provide 'general-conf)