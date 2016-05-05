;;
;; --[ Abbrevs ]----------------------------------------------------------------

;; abbrev
;; expanding from a fixed list
;; ref: http://ergoemacs.org/emacs/emacs_abbrev_mode.html
;;      http://www.emacswiki.org/emacs/AbbrevMode

(use-package abbrev
  :commands abbrev-mode
  :diminish abbrev-mode
  :init
  (progn
    ;; (setq abbrev-file-name "~/.emacs.d/abbrev_defs")
    (setq abbrev-file-name (concat my-personal-dir "abbrev_defs"))
    ;; save my abbreviations when file saved
    (setq save-abbrevs 'silently) ; t

    ;; ensure abbrev mode is always on in current buffer
    ;; (abbrev-mode 1)
    ;; turn on abbrev mode globally
    (setq-default abbrev-mode t)
    )
  :config
  (progn
    ;; reads the abbreviations file on startup
    (if (file-exists-p abbrev-file-name)
        (quietly-read-abbrev-file))

    (add-hook 'expand-load-hook
              (lambda ()
                (add-hook 'expand-expand-hook 'indent-according-to-mode)
                (add-hook 'expand-jump-hook 'indent-according-to-mode)))
    ;; abbrev-mode is on only in some modes
    ;;  (dolist (hook '(erc-mode-hook
    ;;                  emacs-lisp-mode-hook
    ;;                  text-mode-hook))
    ;;  (add-hook hook (lambda () (abbrev-mode 1))))

    (define-abbrev-table 'global-abbrev-table '(
        ("afaict" "as far as I can tell" nil 1)
        ("btw" "by the way" nil 3)
        ("ewiki" "http://www.emacswiki.org/" nil 3)
        ("pov" "point of view" nil 1)
        ))
    )
  )

;; --[ Abbrevs ]-------------------------------------------------------[ End ]--


;; [ Dynamic Abbreviation ]-----------------------------------------------------
;; expanding from matching text found in a buffer
(use-package dabbrev-expand
  :commands dabbrev-expand
  :init
    (setq dabbrev-case-fold-search     nil)
    (setq dabbrev-case-replace         nil))
;; [ Dynamic Abbreviation ]--------------------------------------------[ End ]--


;; [ dabbrev-expand-multiple ]--------------------------------------------------
;; extend standard Dynamic Abbreviation
;; show multiple candidates with tooltip
(use-package dabbrev-expand-multiple
  :commands (dabbrev-expand-multiple)
  :bind ("M-/" . dabbrev-expand-multiple)
  :init
  (progn
    ;; setting abbrev displayed at a time to five.
    (setq dabbrev-expand-multiple-select-keys '("a" "s" "d" "f" "g" "q" "w" "e" "r" "t"))
    ;; The seconds in which tooltip is displayed.
    ;; (setq dabbrev-expand-multiple-tooltip-timeout 2000)
    ;; setting to disappear at ten seconds.
    (setq dabbrev-expand-multiple-tooltip-timeout 10)
    ;; put highlight to first expanded string.
    (setq dabbrev-expand-multiple-highlight-face 'highlight)

    ;; Face used when inline display.
    (setq dabbrev-expand-multiple-inline-show-face 'underline)
    ;; Change inline display face. (not use underline.)
    ;; (setq dabbrev-expand-multiple-inline-show-face nil)

    ;; use tooltip.
    (setq dabbrev-expand-multiple-use-tooltip t)
    ;; use inline display. (not use tooltip.)
    ;; (setq dabbrev-expand-multiple-use-tooltip nil)
    ))
;; [ dabbrev-expand-multiple ]-----------------------------------------[ End ]--


;; [ pabbrev ]------------------------------------------------------------------
;; another abbreviation expansion
;; analyse text during idle time, the abbreviations are always displayed!
; (when (require 'pabbrev nil t)
(use-package pabbrev
  :disabled t
  :diminish pabbrev-mode
  :config
  (progn
    ;; don't print messages while scavenging on idle timer
    (setq pabbrev-idle-timer-verbose nil)
    ;; tab completion with continual, as-you-type feedback
    (global-pabbrev-mode)
    )
)
;; [ pabbrev ]---------------------------------------------------------[ End ]--

(provide 'abbrevs-conf)