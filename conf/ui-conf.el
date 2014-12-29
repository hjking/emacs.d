
(message "%d: >>>>> Loading [ UI Setup ] ...." step_no)
(setq step_no (1+ step_no))

;; display file name on the frame title
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
)

;; menu bar
(setq menu-bar-mode t)

;; tool bar
;; hide toolbar with emacs version >=21
(if (>= emacs-major-version 21)
  (when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
;;(setq tool-bar-mode nil)
)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;;; === startup ===
;; no GNU emacs startup logo
(setq inhibit-startup-echo-area-message "HJKing")
;; Don't display the 'Welcome to GNU Emacs' buffer on startup
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)

;; make the fringe (gutter) smaller
;; the argument is a width in pixels (the default is 8)
(if (fboundp 'fringe-mode)
    (fringe-mode 4))

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

;;*** ---- Dialog Boxes
;; don't use dialog boxes to ask questions
(setq use-dialog-box nil)
;; don't use a file dialog to ask for files
(setq use-file-dialog nil)

;; Don't open new annoying windows under X, use the echo area
(setq tooltip-mode -1)

;; Send deletions to the Trash folder - Emacs 23.2
(setq delete-by-moving-to-trash nil)

;; display line number at the left side of each window
(if is-after-emacs-23
    (global-linum-mode 1))
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

(eval-after-load 'linum
  `(linum-face-settings))

;; (am-def-active-fun linum-mode linum-mode-active)

;; adjust the size of Emacs window
;; (setq default-frame-alist
;;     '((width . 100)
;;       (height . 30)
;;       (left . 80)
;;       (top . 20)
;;       (menu-bar-lines . 1)
;;      ))

;; the cursor's line position and column position will show in the status bar
(column-number-mode 1)

;; turns off the splash screen, straightly into the scratch buffer
(setq inhibit-splash-screen t)

(provide 'ui-conf)