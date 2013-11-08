;;
;; --[ Abbrevs ]----------------------------------------------------------------
(message ">>>>> Loading [ Abbrevs ] Customizations ....")
;; abbrev
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
(global-set-key "\M-/" 'dabbrev-expand-multiple)
;; [ dabbrev-expand-multiple ]-----------------------------------------[ End ]--


;; [ pabbrev ]------------------------------------------------------------------
(message ">>>>> Loading [ Predictive Abbreviation ] Customizations ....")
(when (try-require 'pabbrev)
  ;; don't print messages while scavenging on idle timer
  (setq pabbrev-idle-timer-verbose nil)
  ;; tab completion with continual, as-you-type feedback
  (global-pabbrev-mode)
)
;; [ pabbrev ]---------------------------------------------------------[ End ]--


;; [ hippie-expand ]------------------------------------------------------------
;; expand text trying various ways to find its expansion
;; Hippie Expand
;; (when (try-require 'hippie-exp)
;;     (load "hippie-exp-conf")
;; )

;; (global-set-key (kbd "M-/") 'hippie-expand)

;; I recommend you split the key binding of those two command.
;; I binding TAB yas/expand, and binding M-/ hippie-expand.
;; So yas/expand don't conflict with hippie/expand.

;; [ hippie-expand ]----------------------------------------------------[ End ]--

