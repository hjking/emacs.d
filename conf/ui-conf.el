;; Set up the looks of emacs

;; Contents:
;;
;;  Variables
;;  Bars
;;    Menu bar
;;    Tool bar
;;    Scroll bar
;;  Themes
;;  Frame Title
;;  Fonts
;;    Font Lock
;;    Windows Font
;;  Cursor
;;  Fringes
;;  Coloring regions with ANSI color codes
;;  Whitespace Mode/Show Long Lines

(message "%d: >>>>> Loading [ UI Setup ] ...." step_no)
(setq step_no (1+ step_no))

;;; Variables
;;; === startup ===
;; no GNU emacs startup logo
(setq inhibit-startup-echo-area-message "HJKing")
;; Don't display the 'Welcome to GNU Emacs' buffer on startup
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)
;; turns off the splash screen, straightly into the scratch buffer
(setq inhibit-splash-screen t)
;; Don't open new annoying windows under X, use the echo area
(setq tooltip-mode -1)

;;; Bars

;;;; Menu bar
;; Show the menu bar with File|Edit|Options|...
(setq menu-bar-mode t)

;;;; tool bar
;; hide toolbar with emacs version >=21
(if (>= emacs-major-version 21)
  (when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
;;(setq tool-bar-mode nil)
)

;;;; Scroll bar
(scroll-bar-mode t)
;; (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) ; disable the scroll bars
;; (setq scroll-bar-mode-explicit t)
;; scroll bar at right hand
(set-scroll-bar-mode `right)

;;; Frame Title

;;(setq frame-title-format "%n%F/%b")
;; (setq frame-title-format '(
;;    "Emacs:"
;;    (:eval ( user-full-name))
;;    "@"
;;    (:eval (getenv-internal "HOSTNAME"))
;;    ":"
;;    (:eval (or (buffer-file-name) (buffer-name))))
;; )
;; (setq frame-title-format
;;   '("%b - "
;;    (:eval ( user-full-name))
;;    "@"
;;    (:eval (or (system-name) (getenv-internal "HOSTNAME")))
;;    ":"
;;    (:eval (or (buffer-file-name) (buffer-name)))
;;    " - Emacs " emacs-version
;;   )
;; )
(defun hjking/update-frame-title ()
  "Update the `frame-title-format'."
  (interactive)
  (setq frame-title-format
    '(
    	(:eval (if (buffer-file-name)
               (abbreviate-file-name (buffer-file-name))
              "%b"))
    	" - "
     (:eval ( user-full-name))
     "@"
     (:eval (or (system-name) (getenv-internal "HOSTNAME")))
     " - Emacs " emacs-version
    )
  ))
(add-hook 'after-init-hook #'hjking/update-frame-title)

;;; Fonts

;;;; Font Lock
;; See highlight-conf.el


;;; Cursor
;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)


;;; Fringes
;; make the fringe (gutter) smaller
;; the argument is a width in pixels (the default is 8)
; (if (fboundp 'fringe-mode)
;     (fringe-mode 4))

;; Show the top/bottom buffer boundaries only in the right fringe
(setq-default indicate-buffer-boundaries '((top    . right)
                                           (bottom . right)))

(defconst fringe-styles
  '(("default" . nil)
    ("no-fringes" . 0)
    ("right-only" . (0 . nil))
    ("left-only" . (nil . 0))
    ("half-width" . (4 . 4))
    ("minimal" . (1 . 1)))
  "Alist mapping fringe mode names to fringe widths.
Each list element has the form (NAME . WIDTH), where NAME is a
mnemonic fringe mode name and WIDTH is one of the following:
- nil, which means the default width (8 pixels).
- a cons cell (LEFT . RIGHT), where LEFT and RIGHT are
  respectively the left and right fringe widths in pixels, or
  nil (meaning the default width).
- a single integer, which specifies the pixel widths of both
fringes.")

;; list of frame parameters for creating the initial frame
;;      (setq initial-frame-alist '((top . 0) (left . 0)))
;;      (setq initial-frame-alist
;;          (append (list
;;                   '(internal-border-width . 2)
;;                   '(line-spacing          . 1))
;;                  initial-frame-alist))
;;
;;      ;; list of default values for frame creation
;;      (setq default-frame-alist
;;          (cond ((= (x-display-pixel-height) 1280)
;;                 '((left . 0) (height . 70)))
;;
;;                ((= (x-display-pixel-height) 1024)
;;                 '((left . 0) (height . 60)))
;;
;;                ((= (x-display-pixel-height) 800)
;;                 (cond (win32p
;;                        '((left . 0) (height . 55)))
;;                       (linuxp
;;                        '((left . 0) (height . 47)
;;                          (vertical-scroll-bars . right)))))
;;                ((= (x-display-pixel-height) 768)
;;                 '((left . 0) (height . 46)))))

;; initial window
(setq initial-frame-alist
      '((width . 142)   ; characters in a line
        (height . 42))) ; number of lines

;; sebsequent frame
(setq default-frame-alist
      '((width . 140)   ; characters in a line
        (height . 40))) ; number of lines

;;; Whitespace Mode/Show Long Lines
(use-package whitespace
  :defer t
  :config
  (progn
    (setq whitespace-line-column nil) ; When nil, set the value to `fill-column'
    ))


;;*** ---- Dialog Boxes
;; don't use dialog boxes to ask questions
(setq use-dialog-box nil)
;; don't use a file dialog to ask for files
(setq use-file-dialog nil)


;; display line number at the left side of each window
; (if is-after-emacs-23
;     (global-linum-mode 1))
(setq display-line-number-format "%3d ")
(setq line-number-display-limit 100000)
;; line-num face setting
(defun linum-face-settings ()
  "Face settings for `linum'."
  (custom-set-faces
   '(linum
     ((((background dark))
       :foreground "cyan")
      (t :foreground "gray")))))

(with-eval-after-load 'linum
  (linum-face-settings))

;; (am-def-active-fun linum-mode linum-mode-active)

;; adjust the size of Emacs window
;; (setq default-frame-alist
;;     '((width . 100)
;;       (height . 30)
;;       (left . 80)
;;       (top . 20)
;;       (menu-bar-lines . 1)
;;      ))
; (add-to-list 'default-frame-alist '(height . 40))
; (add-to-list 'default-frame-alist '(width . 100))

;; the cursor's line position and column position will show in the status bar
(column-number-mode 1)

;; Convert certain words into symbols. Prime example: lambda becomes λ.
(global-prettify-symbols-mode)

(provide 'ui-conf)