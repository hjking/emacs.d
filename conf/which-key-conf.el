;; Time-stamp: <2015-11-03 15:26:17>

;; Which Key
;; https://github.com/justbur/emacs-which-key

(use-package which-key
  :diminish which-key-mode
  :init (progn
    ;; Popup Type Options
    ;;; side window
    (setq which-key-popup-type 'side-window) ; default
    ;;; minibuffer
    ;; (setq which-key-popup-type 'minibuffer)

    ;; side window options
    (setq which-key-side-window-max-width 0.33)

    ;; Replacements for how KEY is replaced when which-key displays
    ;;   KEY → FUNCTION
    ;; Eg: After "C-c", display "right → winner-redo" as "▶ → winner-redo"
    (setq which-key-key-replacement-alist
          '(("<\\([[:alnum:]-]+\\)>" . "\\1")
            ("left"                . "◀")
            ("right"               . "▶")
            ("up"                  . "▲")
            ("down"                . "▼")
            ; ("delete"              . "DLT") ; delete key
            ; ("\\`DEL\\'"           . "BS") ; backspace key
            ("next"                . "PgDn")
            ("prior"               . "PgUp")))

    (add-to-list 'which-key-replacement-alist '(("TAB" . nil) . ("↹" . nil))
    (add-to-list 'which-key-replacement-alist '(("RET" . nil) . ("⏎" . nil))
    (add-to-list 'which-key-replacement-alist '(("DEL" . nil) . ("⇤" . nil))
    (add-to-list 'which-key-replacement-alist '(("SPC" . nil) . ("␣" . nil))

    ;; Replacements for how part or whole of FUNCTION is replaced when
    ;; which-key displays
    ;;   KEY → FUNCTION
    ;; Eg: After "d" in `calc', display "6 → calc-hex-radix" as "6 → 🖩hex-radix"
    (setq which-key-description-replacement-alist
          '(("Prefix Command"           . "prefix")
            ("which-key-show-next-page" . "wk next pg")
            ("\\`calc-"                 . "") ; Hide "calc-" prefixes when listing M-x calc keys
            ("/body\\'"                 . "") ; Remove display the "/body" portion of hydra fn names
            ("\\`hydra-"                . "+h/")
            ("\\`org-babel-"            . "ob/")))

    ;; List of "special" keys for which a KEY is displayed as just K but with
    ;; "inverted video" face.
    (setq which-key-special-keys '("SPC"
                                   "TAB"
                                   "RET"
                                   "DLT" ; delete key
                                   "BS" ; backspace key
                                   "ESC"))

    (setq which-key-sort-order 'which-key-key-order-alpha)

    (setq which-key-idle-delay 0.05)

    (setq which-key-highlighted-command-list
          '(("\\`hydra-" . which-key-group-description-face)
            ;; Highlight using the default `which-key-highlighted-command-face'
            "\\(rectangle-\\)\\|\\(-rectangle\\)"
            "\\`org-")))
  :config (progn
    ;; Change what string to display for a given *complete* key binding
    ;; Eg: After "C-x", display "8 → +unicode" instead of "8 → +prefix"
    (which-key-add-key-based-replacements
      "C-x C-f" "find files"
      "C-x a"   "abbrev/expand"
      "C-x m"   "hjking defined"
      "C-x r"   "rect/reg"
      ; "C-c /"   "engine-mode-map"
      "C-x RET" "coding system - input"
      "C-c C-v" "org-babel")

    (which-key-add-major-mode-key-based-replacements 'org-mode
      "C-c C-c" "Org C-c C-c"
      "C-c C-a" "Org Attach")

    (which-key-add-major-mode-key-based-replacements 'markdown-mode
      "C-c C-a" "insert"
      "C-c C-c" "export"
      "C-c TAB" "images"
      "C-c C-s" "text"
      "C-c C-t" "header"
      "C-c C-x" "move")

    (which-key-setup-side-window-right-bottom)

    ;; Take over minibuffer
    ;(which-key-setup-minibuffer)

    (which-key-mode)
    ))


(provide 'which-key-conf)