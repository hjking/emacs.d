;;
;; --[ Abbrevs ]----------------------------------------------------------------
(message ">>>>> Loading [ Abbrevs ] Customizations ....")
;; abbrev
;; expanding from a fixed list
;; ref: http://ergoemacs.org/emacs/emacs_abbrev_mode.html
;; (setq abbrev-file-name "~/.emacs.d/abbrev_defs")
(setq abbrev-file-name (concat my-personal-dir "abbrev_defs"))
;; save my abbreviations when file saved
(setq save-abbrevs t)
;; reads the abbreviations file on startup
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))

;; ensure abbrev mode is always on in current buffer
;; (abbrev-mode 1)
;; turn on abbrev mode globally
(setq-default abbrev-mode t)

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

;; --[ Abbrevs ]-------------------------------------------------------[ End ]--


;; [ dabbrev-expand ]-----------------------------------------------------------
(setq dabbrev-case-fold-search     nil)
(setq dabbrev-case-replace         nil)
;; [ dabbrev-expand ]--------------------------------------------------[ End ]--


;; [ dabbrev-expand-multiple ]--------------------------------------------------
(message ">>>>> Loading [ dabbrev-expand-multiple ] Customizations ....")
(require 'dabbrev-expand-multiple)
;; (global-set-key "\M-/" 'dabbrev-expand-multiple)
(setq dabbrev-expand-multiple-select-keys '("a" "s" "d" "f" "g"))
;; The seconds in which tooltip is displayed.
(setq dabbrev-expand-multiple-tooltip-timeout 2000)
;; setting to disappear at ten seconds.
(setq dabbrev-expand-multiple-tooltip-timeout 10)
;; put highlight to first expanded string.
(setq dabbrev-expand-multiple-highlight-face 'highlight)
;; use tooltip.
(setq dabbrev-expand-multiple-use-tooltip t)
;; [ dabbrev-expand-multiple ]-----------------------------------------[ End ]--


;; [ pabbrev ]------------------------------------------------------------------
;; text found in a buffer
(message ">>>>> Loading [ Predictive Abbreviation ] Customizations ....")
(when (try-require 'pabbrev)
  ;; don't print messages while scavenging on idle timer
  (setq pabbrev-idle-timer-verbose nil)
  ;; tab completion with continual, as-you-type feedback
  (global-pabbrev-mode)
)
;; [ pabbrev ]---------------------------------------------------------[ End ]--


