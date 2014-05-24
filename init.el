
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   ____  __  __  __ _  ___ ___
;;  / __ \/__\/__\/ _` |/ __/ __|
;; | ____/|| || || (_| | (__\__ \
;;  \____ || || ||\__,_|\___|___/
;;
;; File Name:       init.el
;; Author:          Hong Jin
;; Email:           hon9jin (at) gmail.com
;; Created:         2010-04-22 10:20 (+800)
;; Description:     Emacs customization file
;; Copyright:       (C) 2010 ~ 2014, Hong Jin

;; Thanks to http://www.mygooglest.com/fni/dot-emacs.html
;; Use autoloads, which delay the loading of the complete package until one of
;; the interactive functions is used.
;;
;; If you want to set options which need to be evaluated after a package is
;; loaded, you can use `eval-after-load'.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Let's Rock and Roll
;;
(message "")
(message "***** >>>>> [ Loading Emacs Startup File ], Be patient!")
(setq step_no 1)


;; uptimes
(setq emacs-load-start-time (current-time))

;; turn on Common Lisp support
(require 'cl)  ; provides useful things like `loop' and `setf'
(require 'cl-lib)

;; allow quick include/exclude of setup parts
(defvar section-environment t)  ; required
(defvar section-loading-libraries t)  ; required
(defvar section-debugging t)
(defvar section-basic nil)  ; no
(defvar section-minibuffer t)
(defvar section-help t) ; no
(defvar section-mark t)
(defvar section-killing t)
(defvar section-yanking t)
(defvar section-rectangles t)
(defvar section-cua nil)
(defvar section-register nil)
(defvar section-bookmark t)
(defvar section-search t)
(defvar section-keyboard-macros nil)  ; no
(defvar section-ibuffer t)
(defvar section-ido t)
(defvar section-windows t)
(defvar section-ui t)
(defvar section-coding t)
(defvar section-indentation t)
(defvar section-python t)
(defvar section-perl t)
(defvar section-abbrevs t)
(defvar section-dired t)
(defvar section-calendar-diary t)
(defvar section-document-view nil)
(defvar section-gnus t) ; no
(defvar section-eshell t)
(defvar section-hdl t)
(defvar section-verilog nil)
(defvar section-vlog t)
(defvar section-vhdl nil)
(defvar section-emacs-server nil) ; no
(defvar section-org t)
(defvar section-eproject nil)
(defvar section-ecb nil)
(defvar section-session nil)
(defvar section-desktop nil)
(defvar section-muse nil)
(defvar section-cvs nil)
(defvar section-svn nil)
(defvar section-git t)
(defvar section-emms nil)
(defvar section-vm nil)
(defvar section-ac t)
(defvar section-company nil)
(defvar section-helm t)
(defvar section-icicles nil)
(defvar section-scratch t)
(defvar section-c-mode t)
(defvar section-markdown-mode t)
(defvar section-elisp-mode t)
(defvar section-shell-mode t)
(defvar section-defuns t)
(defvar section-alias t)
(defvar section-vi nil)
(defvar section-artist nil)
(defvar section-yasnippet t)
(defvar section-cygwin t)
(defvar section-package t)
(defvar section-tramp t)
(defvar section-cedet t)
(defvar section-cedet-1.1 nil)
(defvar section-drag-stuff t)
(defvar section-mmm-mode t)
(defvar section-table nil)
(defvar section-undo t)
(defvar section-header t)
(defvar section-ergoemacs-mode nil)
(defvar section-irc nil)
(defvar section-w3m nil)
(defvar section-smex t)
(defvar section-slime t)
(defvar section-cscope t)
(defvar section-wl nil)
(defvar section-color-theme t)
(defvar section-weibo t)
(defvar section-workgroups t)
(defvar section-powerline nil)
(defvar section-sml t)

;;;###autoload
(defmacro define-kbd  (keymap key def) `(define-key ,keymap (kbd ,key) ,def))
;;;###autoload
(defmacro local-set-kbd  (key command)    `(local-set-key (kbd ,key) ,command))
;;;###autoload
(defmacro global-set-kbd (key command)    `(global-set-key (kbd ,key) ,command))

(when section-loading-libraries
;;;; Debugging
  (message "%d: >>>>> Debugging On...." step_no)
  (setq step_no (1+ step_no))
  (setq
    eval-expression-debug-on-error t       ; debugger on errors in eval-expression
    stack-trace-on-error nil               ; backtrace of error on debug
    debug-on-error t                       ; debugger on errors
    debug-on-quit nil                      ; hit `C-g' while it's frozen to get an ELisp backtrace
    debug-on-signal nil)                   ; debug any/every error
)

;; --[ Load Path ]--------------------------------------------------------------
(when section-loading-libraries
  (message "%d: >>>>> Loading [ Default Path ] ...." step_no)
  (setq step_no (1+ step_no))
  ; (when win32p
  ;     (message "We are in Windows Platform")
  ;     (setq my-home "F:/Kuaipan/Workspace/src")
  ;     (setenv "HOME" my-home)
  ;     (setenv "PATH" (concat my-home ";" (getenv "PATH")))
  ; )

  (defconst win32p
    (eq system-type 'windows-nt)
    "Are we running on a WinTel system?")

  (defconst cygwinp
    (eq system-type 'cygwin)
    "Are we running on a WinTel cygwin system?")

  (defconst linuxp
    (or (eq system-type 'gnu/linux)
        (eq system-type 'linux))
    "Are we running on a GNU/Linux system?")

  (defconst unixp
    (or linuxp
      (eq system-type 'usg-unix-v)
      (eq system-type 'berkeley-unix))
    "Are we running unix")
  (when linuxp
    (message "We are in Linux Platform")
    (setq my-home "/home/jinhong"))

  (when linuxp
    (add-to-list 'exec-path "~/bin"))

  ;;set the default file path
  (setq default-directory "~/")

  ;; My Emacs home directory
  ; (defvar my-emacs-dir (expand-file-name "~/.emacs.d/")
  ;         "The Root directory of my .emacs.d")
  (defvar my-emacs-init-file (or load-file-name buffer-file-name))
  (defvar my-emacs-dir (file-name-directory my-emacs-init-file)
      "The Root directory of my .emacs.d")
  ;; My site-lisp directory
  ; (defvar my-site-lisp-dir (concat my-emacs-dir "plugin/")
  ;     "This directory keeps Emacs Lisp packages")
  (defvar my-site-lisp-dir (concat my-emacs-dir "vendor/")
      "This directory keeps Emacs Lisp packages")
  ;; My configuration files directory
  (defvar my-site-lisp-conf-dir (concat my-emacs-dir "conf/")
      "This directory keeps my Emacs Lisp for packages")
  ;; Personal configuration files
  (defvar my-personal-dir (concat my-emacs-dir "personal/")
      "This directory keeps my personal configuration")
  (defvar my-cache-dir (concat my-emacs-dir "cache/")
      "This directory keeps cache files")
  (unless (file-exists-p my-cache-dir)
      (make-directory my-cache-dir))
  (defmacro add-load-path (path)
      `(setq load-path (append (list, path) load-path)))
  ;    `(add-to-list 'load-path path))
  (defmacro add-site-lisp-load-path (path)
      `(add-to-list 'load-path (concat my-site-lisp-dir, path)))
  (defmacro add-site-lisp-info-path (path)
      `(add-to-list 'Info-default-directory-list (concat my-site-lisp-dir, path)))
  (add-load-path my-emacs-dir)
  (add-load-path my-site-lisp-dir)
  (add-load-path my-site-lisp-conf-dir)
  (add-load-path my-personal-dir)
  (add-load-path my-cache-dir)
  (message ">>>>> Loading Path ... Done")

  ;; Load all elisp files in ./init.d
  ; (if (file-exists-p my-site-lisp-conf-dir)
  ;     (dolist (file (directory-files my-site-lisp-conf-dir t "\\.el$"))
  ;        (load file)))

  ;; Add external projects to load path
  ; (dolist (project (directory-files my-site-lisp-dir t "\\w+"))
  ;     (when (file-directory-p project)
  ;       (add-to-list 'load-path project)))
)
;; --[ Load Path ]-----------------------------------------------------[ End ]--


;; --[ Environment ]------------------------------------------------------------

(require 'init-compat)
(require 'diminish)

(when section-environment
  (load "env-conf"))

;; --------------------------------------------------------------------[ End ]--


;; --[ cygwin setting ]---------------------------------------------------------
(when section-cygwin
  (when win32p
    (defconst my-cygwin-dir "d:/cygwin/" "Cygwin root path.")
    (if (file-directory-p my-cygwin-dir)
      (progn
        (defvar my-cygwin-bin-dir (concat my-cygwin-dir "bin/") "Cygwin bin folder")
        (defvar my-cygwin-usr-dir (concat my-cygwin-dir "usr/") "Cygwin usr folder")
        (add-site-lisp-load-path "cygwin/")
        (load "cygwin-conf")
      ))))
;; --------------------------------------------------------------------[ End ]--


;; use eval-after-load to speed up the startup
;; http://emacser.com/eval-after-load.htm
(require 'eval-after-load)

;; [ package ]------------------------------------------------------------------
;; Packages managment
(when section-package
    (load "package-conf"))
;; --------------------------------------------------------------------[ End ]--


;; --[ Personal ]---------------------------------------------------------------
(message "%d: >>>>> Loading [ Personal Profile ] ...." step_no)
(setq step_no (1+ step_no))
;;
;; my TODO
;;todo_path: ~/.emacs.d/todo/
;; (setq my-todo-dir (concat my-personal-dir "todo/"))
;; (setq todo-file-do (concat my-todo-dir "do"))
;; (setq todo-file-done (concat my-todo-dir "done"))
;; (setq todo-file-top (concat my-todo-dir "top"))

;; personal variables
(setq personal-vars (concat my-personal-dir "personal.el"))
(when (file-exists-p personal-vars)
  (load personal-vars))

;; custom file: modified setting by menu bar
(setq custom-file (concat my-personal-dir "my-custom.el"))

;; load all el files in personal dir
;; (when (file-exists-p my-personal-dir)
;;   (mapc 'load (directory-files my-personal-dir 't "^[^#].*el$")))
;; --[ Personal ]------------------------------------------------------[ End ]--


;; --[ GNUS ]-------------------------------------------------------------------
(when section-gnus
;;    (setq gnus-init-file (concat my-site-lisp-conf-dir "gnus.el"))
)
;; --------------------------------------------------------------------[ End ]--


;; --[ Alias ]------------------------------------------------------------------
;; load user defined alias
(when section-alias
    (load "defalias"))
;; --------------------------------------------------------------------[ End ]--


;; --[ Function ]---------------------------------------------------------------
;; load user defined functions
(when section-defuns
    (load "defuns"))
;; --------------------------------------------------------------------[ End ]--


;; --[ Basic ]------------------------------------------------------------------
(message "%d: >>>>> Loading [ Basic ] Customization ...." step_no)
(setq step_no (1+ step_no))

(setq max-lisp-eval-depth 3000)
(setq max-specpdl-size 10000)

;; Load global settings
(load "global")

;; don't truncate the message log buffer when it becomes large
(setq message-log-max t)

;; turn on Common Lisp support
(require 'cl)  ; provides useful things like `loop' and `setf'

;; Load face+ after loading face
(eval-after-load "faces" '(require 'faces+ nil t))

(when section-scratch
    (load "scratch-conf"))

;; use clipboard, share with other applications
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      ;; Save clipboard strings into kill ring before replacing them
      save-interprogram-paste-before-kill t)

(message "%d: >>>>> Loading [ Misc ] Customization ...." step_no)
(setq step_no (1+ step_no))

;; make the help, apropos and completion windows the right height for their contents
;; (temp-buffer-resize-mode t)
(temp-buffer-resize-mode 1)

;; enable the use of the command `narrow-to-region' without confirmation
(put 'narrow-to-region 'disable nil)

;; enable the use of the commands `downcase-region' and `upcase-region'
;; without confirmation
;; Enable conversion of the selected region to upper case using `C-x C-u`
(put 'upcase-region 'disabled nil)
;; Enable conversion of the selected region to lower case using `C-x C-l`
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disable nil)

;; Not open new frame when WoMan
(setq woman-use-own-frame nil)
(setq woman-fill-column 90)

;; display images
;;  (require 'image-mode)
(auto-image-file-mode 1)
;;  (define-key image-mode-map (kbd "'")  'switch-to-other-buffer)

;; handle compressed file
(require 'jka-compr)
(auto-compression-mode 1)

;; display function the cursor is in
(which-function-mode 1)
(setq which-func-unknown "unknown")

(message ">>>>> Loading [ Misc ] Customization Done")

;; NEW @  2010-10-23-23:08
(add-hook 'message-mode-hook (lambda ()
    (setq fill-column 80)
    (turn-on-auto-fill)))

(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)

;;use proxy server
;;(setq url-proxy-services '(("http" . "proxy.km.local:8080")))

;;; Library

;; dash
(require 'dash)
(dash-enable-font-lock)

;; sams-lib
(require 'sams-lib nil t)

;; --[ Basic ]---------------------------------------------------------[ End ]--


;; --[ Font ]-------------------------------------------------------------------
(load "font-conf")
;; --[ Font ]----------------------------------------------------------[ End ]--


;; --[ Frame Display ]----------------------------------------------------------
(when section-ui
    (load "ui-conf"))
;; --[ Frame Display ]-------------------------------------------------[ End ]--


;; --[ Bookmark ]---------------------------------------------------------------
(when section-bookmark
  (message "%d: >>>>> Loading [ Bookmark ] Customization ...." step_no)
  (setq step_no (1+ step_no))
  (require 'bookmark)
  ;; set bookmark file: ~/.emacs.d/emacs_bookmarks
  (setq bookmark-default-file (concat my-emacs-dir "emacs_bookmarks"))
  ;; each command that sets a bookmark will also save your bookmarks
  (setq bookmark-save-flag t)
  ;; (switch-to-buffer "*Bookmark List*")
)
;; --[ Bookmark ]------------------------------------------------------[ End ]--


;; [ cedet ]--------------------------------------------------------------------
(when section-cedet-1.1
    (setq byte-compile-warnings nil)
    (setq my-cedet-path (concat my-site-lisp-dir "cedet-1.1/"))
    (setq my-cedet-comm-path (concat my-cedet-path "common/"))
    (setq my-cedet-contrib-path (concat my-cedet-path "contrib/"))
    (add-site-lisp-load-path "cedet-1.1/")
    (add-site-lisp-load-path "cedet-1.1/common/")
    (add-site-lisp-load-path "cedet-1.1/contrib/")
    (add-site-lisp-load-path "cedet-1.1/ede")
    (add-site-lisp-load-path "cedet-1.1/eieio")
    (add-site-lisp-load-path "cedet-1.1/semantic")
    (add-site-lisp-load-path "cedet-1.1/speedbar")
    (add-site-lisp-load-path "cedet-1.1/srecode")

    (load-file (concat my-cedet-comm-path "cedet.el"))

    ;; (add-to-list 'load-path my-cedet-path)
    ;; (add-to-list 'load-path my-cedet-comm-path)
    ;; (add-to-list 'load-path my-cedet-contrib-path)

    ;; (load-file "cedet.el")
    (load "cedet-1.1-conf")
    ;; (load "cedet-conf")
)
(when section-cedet
  (load "cedet-conf"))
;; --------------------------------------------------------------------[ End ]--


;; --[ Minibuffer ]-------------------------------------------------------------
(when section-minibuffer
  (message "%d: >>>>> Loading [ Minibuffer ] Customization ...." step_no)
  (setq step_no (1+ step_no))
  ;; ignore case when reading a file name completion
  (setq read-file-name-completion-ignore-case t)
  ;; minibuffer window expands vertically as necessary to hold the text that you
  ;; put in the minibuffer
  (setq resize-minibuffer-mode t)
  ;; Enable recursive minibuffer
  (setq enable-recursive-minibuffers t)
  ;; auto-complete on in minibuffer
  (unless is-after-emacs-23
      partial-completion-mode 1)
  ;; ignore case when reading a buffer name
  (setq read-buffer-completion-ignore-case t)
  ;; auto-complete in minibuffer when execute M-x functions and variables
  (icomplete-mode 1)
  ;; do not consider case significant in completion (GNU Emacs default)
  (setq completion-ignore-case t)
  ;; type SPACE to auto-complete in minibuffer
  (define-key minibuffer-local-completion-map (kbd "SPC") 'minibuffer-complete-word)
  ;; auto-complete
  (minibuffer-electric-default-mode t)
  ;;; Electric minibuffer!
  ;;; When selecting a file to visit, // will mean / and
  ;;; ~ will mean $HOME regardless of preceding text.
  (setq file-name-shadow-tty-properties '(invisible t))
  (file-name-shadow-mode 1)

  (defun completion-faces ()
    (unless is-before-emacs-21
      (custom-set-faces
       '(completions-first-difference
         ((((class color) (background dark)) (:foreground "red")))))
      (set-face-foreground 'completions-common-part "yellow")))
  ;; (am-add-hooks 'completion-setup-hook 'completion-faces)

)
;; --[ Minibuffer ]----------------------------------------------------[ End ]--


;; --[ mark and region ]--------------------------------------------------------
(when section-mark
    (message "%d: >>>>> Loading [ Mark and Region ] Customization ...." step_no)
    (setq step_no (1+ step_no))
    ;; highlight marked region
    (transient-mark-mode 1)    ; highlight text selection
    (delete-selection-mode 1) ; delete seleted text when typing
    ;; C-u C-SPC C-SPC ... cycles through the buffer local mark ring
    (setq set-mark-command-repeat-pop t)
    (setq select-active-region t)
    (setq delete-active-region 'kill)

    ;;; rect-mark.el
    (global-set-key (kbd "C-x r C-/") 'rm-set-mark)
    (global-set-key (kbd "C-x r C-x") 'rm-exchange-point-and-mark)
    (global-set-key (kbd "C-x r C-w") 'rm-kill-region)
    (global-set-key (kbd "C-x r M-w") 'rm-kill-ring-save)
    (autoload 'rm-set-mark "rect-mark"
      "Set mark for rectangle." t)
    (autoload 'rm-exchange-point-and-mark "rect-mark"
      "Exchange point and mark for rectangle." t)
    (autoload 'rm-kill-region "rect-mark"
      "Kill a rectangular region and save it in the kill ring." t)
    (autoload 'rm-kill-ring-save "rect-mark"
      "Copy a rectangular region to the kill ring." t)

    (require 'expand-region)
    (global-set-key (kbd "C-=") 'er/expand-region)
)
;; --[ mark and region ]-----------------------------------------------[ End ]--


;; --[ killing ]----------------------------------------------------------------
(when section-killing
    (message "%d: >>>>> Loading [ Killing ] Customization ...." step_no)
    (setq step_no (1+ step_no))
    ;; use a bigger kill ring
    (setq kill-ring-max (* 20 kill-ring-max))
    ;; C-k delete a whole line
    (setq-default kill-whole-line t)
    ;; enables wrapping but kill-line still kills the whole line
    (setq-default word-wrap t)
    ;; when press copy or cut when no region is active, copy or cut the current line
    ;; <http://www.zafar.se/bkz/Articles/EmacsTips>
    (defadvice kill-ring-save (before slickcopy activate compile)
      "When called interactively with no active region, copy the current line instead."
      (interactive
      (if mark-active
        (list (region-beginning) (region-end))
        (progn
          (message "Current line is copied")
          (list (line-beginning-position) (line-end-position))))))

    (defadvice kill-region (before slickcut activate compile)
      "When called interactively with no active region, cut the current line instead."
      (interactive
      (if mark-active
        (list (region-beginning) (region-end))
        (progn
            (message "Current line is cut.")
            (list (line-beginning-position) (line-end-position))))))
)
;; --[ killing ]-------------------------------------------------------[ End ]--


;; --[ yanking ]----------------------------------------------------------------
(when section-yanking
    (message "%d: >>>>> Loading [ Yanking ] Customization ...." step_no)
    (setq step_no (1+ step_no))
    ;; middle button for paste
    (setq mouse-yank-at-point t)
    ;; auto-indent pasted code
    (defadvice yank (after indent-region activate)
      (if (member major-mode
                  '(emacs-lisp-mode scheme-mode lisp-mode c-mode c++-mode
                    objc-mode latex-mode plain-tex-mode python-mode))
          (indent-region (region-beginning) (region-end) nil)))

    (defadvice yank-pop (after indent-region activate)
      (if (member major-mode
                  '(emacs-lisp-mode scheme-mode lisp-mode c-mode c++-mode
                    objc-mode latex-mode plain-tex-mode python-mode))
          (indent-region (region-beginning) (region-end) nil)))
)
;; --[ yanking ]-------------------------------------------------------[ End ]--


;; --[ CUA Mode]----------------------------------------------------------------
(when section-cua
  (when is-after-emacs-22
      (load "cua-conf")))
;; --------------------------------------------------------------------[ End ]--


;; --[ Scrolling ]--------------------------------------------------------------
(message "%d: >>>>> Loading [ Scrolling ] Customization ...." step_no)
(setq step_no (1+ step_no))
;; no scroll bar
(scroll-bar-mode t)
;;(when (fboundp 'scroll-bar-mode)
;;    (scroll-bar-mode -1))
;;  (setq scroll-bar-mode-explicit t)
;; scroll bar at right hand
(set-scroll-bar-mode `right)
;; scroll when point 2 lines far away from the bottom
(setq scroll-margin 3)
; Scroll just one line when hitting bottom of window
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position 1)
; (setq scroll-preserve-screen-position 'always
;       scroll-conservatively           most-positive-fixnum
;       scroll-step                     0)
(setq hscroll-step 2)
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq redisplay-dont-pause t)
(setq scroll-lock-mode 1)
;; scroll one line at a time (less "jumpy" than defaults)
;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; don't add new lines when scrolling point
(setq next-line-add-newlines nil)
;; <pager> provides a better scrolling in Emacs

;; Keep cursor away from edges when scrolling up/down
(add-site-lisp-load-path "smooth-scrolling/")
(require 'smooth-scrolling)
;; --[ Scrolling ]-----------------------------------------------------[ End ]--


;; --[ search and replace ]-----------------------------------------------------
(when section-search
    (add-site-lisp-load-path "visual-regexp/")
    (add-site-lisp-load-path "anzu/")
    (load "search-conf"))
;; --[ Search and Replace ]--------------------------------------------[ End ]--


;; --[ Mode Line ]--------------------------------------------------------------
;; (load "mode-line-conf")

;; mode-line-stats
;; Display CPU/Memory/Disk status on mode line
;; (add-site-lisp-load-path "mode-line-stats/")
;; (load "mode-line-stats-conf")

;; [ powerline ]
(when section-powerline
    (add-site-lisp-load-path "powerline/")
    (load "powerline-conf"))

(when section-sml
  (add-site-lisp-load-path "smart-mode-line/")
  (require 'smart-mode-line)
  (setq sml/position-percentage-format "%p")
  (setq sml/shorten-directory t)
  (setq sml/name-width 15)
  (sml/apply-theme 'dark)  ;; respectful
  (sml/setup)
  )

;; --[ Mode Line ]-----------------------------------------------------[ End ]--


;; --[ Cursor and Point ]-------------------------------------------------------
(message "%d: >>>>> Loading [ Cursor and Point ] Customization ...." step_no)
(setq step_no (1+ step_no))
;; move cursor when point is coming
;; (mouse-avoidance-mode 'animate)
;; keep point at the end of the line
(setq track-eol t)
;; Support Mouse Wheel Scrolling
(mouse-wheel-mode t)
;; default setting
;;(global-set-key (kbd "<mouse-1>") 'mouse-set-point)
;;(global-set-key (kbd "<down-mouse-1>") 'mouse-drag-region)
;;(global-set-key (kbd "<mouse-2>") 'mouse-yank-at-click)
;;(global-set-key (kbd "<C-down-mouse-1>") 'mouse-buffer-menu)
;;(global-set-key (kbd "<mouse-3>") 'mouse-save-then-kill)
;;(global-set-key (kbd "<S-down-mouse-1>") 'mouse-set-font)
;;(global-set-key (kbd "<C-down-mouse-2>") 'mouse-popup-menuar-stuff)
;; fast copy and paste
(require 'mouse-copy)
(global-set-key [M-down-mouse-1] 'mouse-drag-secondary-pasting)
(global-set-key [M-S-down-mouse-1] 'mouse-drag-secondary-moving)

;; using cursor color to indicate some modes (read-only, insert and overwrite modes)
(setq my-set-cursor-color-color "")
(setq my-set-cursor-color-buffer "")

(defun my-set-cursor-color-according-to-mode ()
  "Change cursor color according to some minor modes."
  (let ((color
         (if buffer-read-only "purple1"
           (if overwrite-mode "red"
             "rgb:15/FF/00"))))  ;; insert mode
    (unless (and (string= color my-set-cursor-color-color)
                 (string= (buffer-name) my-set-cursor-color-buffer))
      (set-cursor-color (setq my-set-cursor-color-color color))
      (setq my-set-cursor-color-buffer (buffer-name)))))
(add-hook 'post-command-hook 'my-set-cursor-color-according-to-mode)

;; --[ Cursor and Point ]----------------------------------------------[ End ]--


;; --[ Spell Correction ]-------------------------------------------------------
;;  (load "spelling-check-conf")
;; --[ Spell Correction ]----------------------------------------------[ End ]--


;; --[ Saving File ]------------------------------------------------------------
(message "%d: >>>>> Loading [ Saving File ] Customization ...." step_no)
(setq step_no (1+ step_no))
;; make file executable when saving
;; (add-hook 'after-save-hook
;;           'executable-make-buffer-file-executable-if-script-p)
;; offer save of `*scratch*' buffer on exit
;;  (save-excursion
;;    (set-buffer "*scratch*")
;;    (setq buffer-file-name "~/emacs_scratch"))

;; add a new line at the end of file when saving
;; Ask me whether to add a final newline to files which don't have one
(setq require-final-newline 'ask)

;;; put a timestamp
;; time-stamp on
(setq time-stamp-active t)
;; if "Last Change: " in file, then save time stamp
(setq time-stamp-start " Last Changed:  ")
;; time-stamp warning off
(setq time-stamp-warn-inactive t)
;; time-stamp end
(setq time-stamp-end "\n")
;; time-stamp format
;; YYYY-MM-DD Weekday HH:MM:SS
(setq time-stamp-format "%04y-%02m-%02d %3a %02H:%02M:%02S")
;; when saving file, save time-stamp
; (add-hook 'write-file-hooks 'time-stamp)
(add-hook 'before-save-hook 'time-stamp)
;; --------------------------------------------------------------------[ End ]--


;; --[ Backup ]-----------------------------------------------------------------
(message "%d: >>>>> Loading [ Backup ] Customization ...." step_no)
(setq step_no (1+ step_no))
;; default backup folder:~/emacs.d/auto-save/
;; set backup file path
(setq my-backup-dir (concat my-emacs-dir "backup/"))
(add-to-list 'backup-directory-alist '(cons "." my-backup-dir))
(setq my-auto-save-dir (concat my-emacs-dir "auto-save/"))
(setq auto-save-list-file-name (concat my-auto-save-dir "auto-save"))
; (setq auto-save-file-name-transforms '((".*" "auto-save-list" t)))
;; version control on
(setq version-control t)
;; backup older versions twice:before 1st edit and 2nd edit
(setq kept-old-versions 2)
;; backup latest version 5 times
(setq kept-new-versions 5)
;; delete old versions except above 7 version
(setq delete-old-versions t)
;; backup method: copy directly
(setq backup-by-copying t)
;; no backup file
;; stop creating those backup~ files
(setq make-backup-files nil)
; stop creating those #autosave# files
(setq auto-save-default nil)
;; auto save interval:every 100 input event
(setq auto-save-interval 100)
;; auto save after 20 senconds idle time
(setq auto-save-timeout 20)
(setq delete-auto-save-files t)
(setq backup-inhibited t)

;; make the message "FILE has auto saved data" unmissable
(defface recover-this-file
  '((t :background "orange"))
  "Face for buffers visiting files with auto saved data."
  :group 'files)
(defvar recover-this-file nil
  "If non-nil, an overlay indicating that the visited file has auto save data.")

(defun recover-this-file-find-file-hook ()
  ;; see after-find-file
  (let ((warn (not buffer-read-only)))
    (when (and warn
               ;; No need to warn if buffer is auto-saved
               ;; under the name of the visited file.
               (not (and buffer-file-name
                         auto-save-visited-file-name))
               (file-newer-than-file-p (or buffer-auto-save-file-name
                                           (make-auto-save-file-name))
                                       buffer-file-name))
      (set (make-local-variable 'recover-this-file)
           (make-overlay (point-min) (point-max)))
      (overlay-put recover-this-file 'face 'recover-this-file))))
(add-hook 'find-file-hook 'recover-this-file-find-file-hook)
;; --[ Auto Save ]-----------------------------------------------------[ End ]--


;; --[ Compare File ]-----------------------------------------------------------
(message "%d: >>>>> Loading [ Compare File ] Customization ...." step_no)
(setq step_no (1+ step_no))
;; default to unified diffs
(setq diff-switches "-u")
;; compare text in current window with text in next window
;; use 'compare-windows function

;; run `diff' in compilation-mode
(autoload 'diff-mode "diff-mode" "Diff major mode" t)

;; use diff-mode- to enhance diff-mode
;;  ;; extensions to `diff-mode.el'
;;  (require 'diff-mode-)
;;
;;  ;; ediff, a comprehensive visual interface to diff & patch
;;  ;; setup for Ediff's menus and autoloads
;;  (require 'ediff-hook)
;;
;;  ;; auto-refine only the regions of this size (in bytes) or less
;;  (setq ediff-auto-refine-limit (* 2 14000))
;;
;; do everything in one frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-custom-diff-options "-u")
;;
;;  ;; split the window depending on the frame width
;;  (setq ediff-split-window-function (lambda (&optional arg)
;;                                      (if (> (frame-width) 160)
;;                                          (split-window-horizontally arg)
;;                                        (split-window-vertically arg))))
;; --[ Compare File ]--------------------------------------------------[ End ]--


;; --[ Buffer Handling ]--------------------------------------------------------
(message "%d: >>>>> Loading [ Buffer Handling ] Customization ...." step_no)
(setq step_no (1+ step_no))

;; meaningful names for buffers with the same name
(require 'uniquify)
;; if open a same name buffer, then forward to same name buffer
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers
(setq uniquify-non-file-buffer-names t)

;; use current buffer when read man
(setq Man-notify-method 'pushy)

;; better buffer switching
(iswitchb-mode 1)
;; prevent certain buffers from showing up in the completion list
(setq iswitchb-buffer-ignore '("^ " "*Buffer"))
;; prevent switching to another frame
(setq iswitchb-default-method 'samewindow)
;; Quickly switch between buffer with tab-complete
;; See "ido"
;;
;; Save where i was in each file
;; See saveplace
;; --[ Buffer Handling ]-----------------------------------------------[ End ]--


;; [ saveplace ]----------------------------------------------------------------
;; remembers your location in a file when saving files

(when (require 'saveplace nil t)
  (message "%d: >>>>> Loading [ saveplace ] Customization ...." step_no)
  (setq step_no (1+ step_no))
  ;; automatically save place in each file
  (setq-default save-place t)
  ;; name of the file that records `save-place-alist' value
  (setq save-place-file (concat my-cache-dir "emacs.places"))
  ;; do not make backups of master save-place file
  (setq save-place-version-control "never")
  (define-key ctl-x-map "p" 'toggle-save-place-globally))
;; --------------------------------------------------------------------[ End ]--


;; --[ Window ]-----------------------------------------------------------------
;; use "C-c <--" back to previous window layout
(when section-windows
    (load "window-conf"))
;; --------------------------------------------------------------------[ End ]--


;; --[ Indentation ]------------------------------------------------------------
(when section-indentation
    (message "%d: >>>>> Loading [ Indentation ] Customization ...." step_no)
    (setq step_no (1+ step_no))
    ;;  indent automatically (from 24.1)
    (electric-indent-mode +1)
    ;; Tab width
    (setq-default tab-width 4)
    ;; Use spaces, not tabs
    (setq-default indent-tabs-mode nil)
    ;; a single space does end a sentence
    (setq sentence-end-double-space nil)

    (setq backward-delete-char-untabify nil)

    ;; `C-M-\' runs the command `indent-region' (which does the job of
    ;; the imaginary command `unsuck-html-layout' in `html-mode')
    (defun indent-whole-buffer ()
      (interactive)
      (save-excursion
        (mark-whole-buffer)
        (indent-for-tab-command)))

    (defun toggle-indent-tabs-mode ()
      "Set `indent-tabs-mode' to what it isn't"
      (interactive)
      (setq indent-tabs-mode (not indent-tabs-mode))
      (if indent-tabs-mode
        (message "`indent-tabs-mode' now on")
      (message  "`indent-tabs-mode' now off")))

    (require 'indent-guide)
    (add-hook 'prog-mode-hook (lambda () (indent-guide-mode 1)))
)
;; --[ Indentation ]---------------------------------------------------[ End ]--


;; --[ Documentation ]----------------------------------------------------------
(message "%d: >>>>> Loading [ Documentation ] Customization ...." step_no)
(setq step_no (1+ step_no))
(autoload 'turn-on-eldoc-mode "eldoc" nil t)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook lisp-interaction-mode-hook))
  (add-hook hook 'turn-on-eldoc-mode))
;; --[ Documentation ]-------------------------------------------------[ End ]--


;; --[ Compilation ]------------------------------------------------------------
(message "%d: >>>>> Loading [ Compilation ] Customization ...." step_no)
(setq step_no (1+ step_no))
;; highlight and parse the whole compilation output as soon as it arrives
(setq compile-auto-highlight t)
;; display compiler error message, check key bindings:
;; first-error / next-error / previous-error
;; --[ Compilation ]---------------------------------------------------[ End ]--


;; --[ Calendar ]---------------------------------------------------------------
(when section-calendar-diary
    ;; (setq diary-file "~/.emacs.d/diary")
    (setq diary-file (concat my-personal-dir "diary"))
    (load "calendar-conf"))
;; --[ Calendar ]------------------------------------------------------[ End ]--


;; --[ Printer ]----------------------------------------------------------------
(message "%d: >>>>> Loading [ Printer ] Customization ...." step_no)
(setq step_no (1+ step_no))
;; in case of press "print" of menu bar
(fset 'print-buffer 'ignore)
(setq lpr-command "")
(setq printer-name "")
;; --[ Printer ]-------------------------------------------------------[ End ]--


;; --[ Game ]-------------------------------------------------------------------
(message "%d: >>>>> Loading [ Game ] Customization ...." step_no)
(setq step_no (1+ step_no))
;; get rid of the Games in the Tools menu
(define-key menu-bar-tools-menu [games] nil)
;; --[ Game ]----------------------------------------------------------[ End ]--


;; --[ Parentheses ]------------------------------------------------------------
(add-site-lisp-load-path "smartparens/")
(load "parens-conf")
;; --[ Parentheses ]---------------------------------------------------[ End ]--


;; [ Emacs Server ]-------------------------------------------------------------
;;; EmacsClient
(when section-emacs-server
  (require 'server)
  (when (and (= emacs-major-version 23)
             (= emacs-minor-version 1)
             (equal window-system 'w32))
    (defun server-ensure-safe-dir (dir) "Noop" t)) ; Suppress error "directory
                                                   ; ~/emacs.d/server is unsafe"
                                                   ; on windows.
  (server-start 1))
;; --------------------------------------------------------------------[ End ]--


;; [ TAG ]----------------------------------------------------------------------
(load "ctags-conf")
;; [ TAG ]-------------------------------------------------------------[ End ]--


;; [ Wrap Line ]----------------------------------------------------------------
(message "%d: >>>>> Loading [ Wrap Line ] Customization ...." step_no)
(setq step_no (1+ step_no))

;; Do `M-x toggle-truncate-lines` to jump in and out of truncation mode.
;; Don't break lines for me, please
;; (setq-default truncate-lines t)

;; insert a [line ending] after the last word that occurs
;; before the value of option ‘fill-column’
;; (auto-fill-mode 1)
; (add-hook 'text-mode-hook 'turn-on-auto-fill)
; (remove-hook 'text-mode-hook 'turn-on-auto-fill)
;; ask whether to use Auto Fill Mode
; (add-hook 'text-mode-hook
;               (lambda ()
;                 (when (y-or-n-p "Auto Fill mode? ")
;                   (turn-on-auto-fill))))

;; LongLines
;; automatically wrap long lines after the last word before ‘fill-column’
; (autoload 'longlines-mode
;   "longlines.el"
;   "Minor mode for automatically wrapping long lines." t)
; (when (load "longlines" t)
;     (setq longlines-show-hard-newlines t))
;; (add-hook 'text-mode-hook 'longlines-mode)


;; visual-line-mode, wrap a line right before the window edge
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
;; (global-visual-line-mode -1) ;; Disable wrapping lines at word boundaries
(global-visual-line-mode t)
(setq line-move-visual nil)
;; Enable/Disable visual-line mode in specific major modes. Enabling visual
;; line mode does word wrapping only at word boundaries
;; turn off
;; (add-hook 'sh-mode-hook      'turn-off-visual-line-mode) ;; e.g. sim.setup file
;; turn on
;; (dolist (hook '(text-mode-hook org-mode-hook))
;;   (add-hook hook 'visual-line-mode))

(setq default-justification 'full)
(setq adaptive-fill-mode nil)
(setq fill-column 80)
;; --------------------------------------------------------------------[ End ]--


;; [ Dired ]--------------------------------------------------------------------
(when section-dired
  (add-site-lisp-load-path "dired/")
  (add-site-lisp-load-path "dired/dired-hacks/")
  (load "dired-conf"))
;; [ Dired ]-----------------------------------------------------------[ End ]--


;; [ ibuffer ]------------------------------------------------------------------
;; buffer switch
(when section-ibuffer
  (load "ibuffer-conf"))
;; [ ibuffer ]---------------------------------------------------------[ End ]--


;; [ ido ]----------------------------------------------------------------------
(when section-ido
    (add-site-lisp-load-path "ido-hacks/")
    (add-site-lisp-load-path "ido-ubiquitous/")
    (add-site-lisp-load-path "flx-ido/")
    (add-site-lisp-load-path "ido-vertical-mode/")
    (add-site-lisp-load-path "ido-at-point/")
    (load "ido-conf"))
;; [ ido ]-------------------------------------------------------------[ End ]--


;; [ Table ]--------------------------------------------------------------------
(when section-table
    (message "%d: >>>>> Loading [ table ] Customization ...." step_no)
    (setq step_no (1+ step_no))
    (require 'table nil t)
    (autoload 'table-insert "table" "WYGIWYS table editor")
    (add-hook 'text-mode-hook 'table-recognize))
;; --------------------------------------------------------------------[ End ]--


;; [ htmlize ]------------------------------------------------------------------
(load "htmlize-conf")
;; [ htmlize ]---------------------------------------------------------[ End ]--


;; [ kill-ring ]----------------------------------------------------------------
;; enhance kill ring function
(add-site-lisp-load-path "browse-kill-ring/")
(load "kill-ring-conf")
;; [ kill-ring ]-------------------------------------------------------[ End ]--


;; [ multi-term ]---------------------------------------------------------------
;; available for Emacs 23
; (message "%d: >>>>> Loading [ multi-term ] Customization ...." step_no)
; (load "multi-term-conf")
;; [ multi-term ]------------------------------------------------------[ End ]--


;; [ Version Control ]----------------------------------------------------------
(message "%d: >>>>> Loading [ Version Control ] Customization ...." step_no)
(setq step_no (1+ step_no))

;; Don't show whitespace in diff, but show context
(setq vc-diff-switches '("-b" "-B" "-u"))

;; *** --- PCL-CVS
(when section-cvs
  (when (require 'pcvs nil t)
    (eval-after-load 'pcvs
      (load "pcvs-conf"))
))

;; *** --- Subversion
(when section-svn
  (when (require 'psvn nil t)
    (eval-after-load 'psvn
      (load "psvn-conf"))
))

;; *** --- Change Log
;; don't make a new entry, when the last entry was made by you and on the same date
(setq add-log-always-start-new-record nil)
;; adds the file's version number to the change log entry
(setq change-log-version-info-enabled t)

;;
;; Git
(when section-git
  (add-site-lisp-load-path "git-modes/")

  ;; *** --- magit
  (add-site-lisp-load-path "magit/")
  (add-site-lisp-info-path "magit/")
  (load "git-conf"))

;; [ Version Control ]-------------------------------------------------[ End ]--


;; [ smart-compile ]------------------------------------------------------------
(load "smart-compile-conf")
;; [ smart-compile ]---------------------------------------------------[ End ]--


;; [ highlight ]----------------------------------------------------------------
(load "highlight-conf")
;; --------------------------------------------------------------------[ End ]--


;; [ auto-header ]--------------------------------------------------------------
(when section-header
    (load "header2-conf"))
;; [ auto-header ]----------------------------------------------------[ End ]---


;; [ goto change ]--------------------------------------------------------------
(require 'goto-chg)
;; [ goto change ]-----------------------------------------------------[ End ]--


;; [ column-marker ]------------------------------------------------------------
;; highlight columns 75, 80, 100 in some modes
(load "column-marker-conf")
;; [ column-marker ]---------------------------------------------------[ End ]--


;; [ recent files ]-------------------------------------------------------------
(load "recentf-conf")
;; [ recent files ]---------------------------------------------------[ End ]---


;; [ helm ]---------------------------------------------------------------------
;; available for Emacs 22/23
(when section-helm
    ;; helm is new version of anything
    (add-site-lisp-load-path "emacs-helm/")
    (add-site-lisp-info-path "emacs-helm/doc/")
    ;; helm-swoop
    (add-site-lisp-load-path "helm-swoop/")
    (load "helm-conf"))
;; [ helm ]-------------------------------------------------------------[ End ]--


;;;; ================ CategoryCompletion ================
;; [ icicles ]------------------------------------------------------------------
(when section-icicles
    (add-site-lisp-load-path "icicles/")
    (load "icicles-conf"))
;; --------------------------------------------------------------------[ End ]--

;; [ auto-complete ]------------------------------------------------------------
;; available for Emacs 22/23
(when section-ac
  (setq auto-comp-load-path (concat my-site-lisp-dir "auto-complete/"))
  (add-site-lisp-load-path "auto-complete/")
  (add-site-lisp-load-path "auto-complete/lib/popup/")
  (add-site-lisp-load-path "auto-complete/lib/fuzzy/")
  (add-site-lisp-load-path "auto-complete/lib/ert/lisp/emacs-lisp")
  (add-site-lisp-info-path "auto-complete/doc/")
  (setq my-ac-dict-dir (concat auto-comp-load-path "dict/"))
  (load "auto-complete-conf")
  (add-to-list 'ac-dictionary-directories my-ac-dict-dir)
  )
;; [ auto-complete ]---------------------------------------------------[ End ]--


;; --[ Company ]----------------------------------------------------------------
(when section-company
  (add-site-lisp-load-path "company-mode/")
  (load "company-conf"))
;; --------------------------------------------------------------------[ End ]--


;; --[ Abbrevs ]----------------------------------------------------------------
(when section-abbrevs
  (load "abbrevs-conf")
  (load "hippie-exp-conf"))
;; --[ Abbrevs ]-------------------------------------------------------[ End ]--


;; [ yasnippet ]----------------------------------------------------------------
;; available for Emacs 22/23
(when section-yasnippet
    (add-site-lisp-load-path "yasnippet/")
    (add-site-lisp-info-path "yasnippet/doc/")
    (load "yasnippet-conf"))
;; [ yasnippet ]-------------------------------------------------------[ End ]--


;; [ Find File ]----------------------------------------------------------------
;; find file or URL at point
;; Find file in Project
(load "find-file-conf")
;; --------------------------------------------------------------------[ End ]--


;; [ VM ]-----------------------------------------------------------------------
(when section-vm
  (setq my-vm-load-path (concat my-site-lisp-dir "vm/"))
  (setq my-vm-lisp-path (concat my-vm-load-path "lisp/"))
  (add-site-lisp-load-path "vm/lisp/")
  (add-site-lisp-info-path "vm/info/")

  (load "vm-conf"))
;; [ VM ]--------------------------------------------------------------[ End ]--


;; [ Hide-Show ]----------------------------------------------------------------
(require 'hideshow nil t)
(when (featurep 'hideshow)
  (message "%d: >>>>> Loading [ hide-show ] Customization ...." step_no)
  (setq step_no (1+ step_no))
  (dolist (hook '(c++-mode-hook
                  c-mode-hook
                  emacs-lisp-mode-hook
                  verilog-mode-hook
                  python-mode-hook
                  sh-mode-hook
                  cperl-mode-hook))
  (add-hook hook 'hs-minor-mode))
  ;; (global-set-key [f1] ‘hs-toggle-hiding)
)

;; hideshowvis
(autoload 'hideshowvis-enable "hideshowvis" "Highlight foldable regions")
(autoload 'hideshowvis-minor-mode
  "hideshowvis"
  "Will indicate regions foldable with hideshow in the fringe."
  'interactive)

(dolist (hook (list 'emacs-lisp-mode-hook
                    'c++-mode-hook
                    'lisp-mode-hook
                    'ruby-mode-hook
                    'perl-mode-hook
                    'php-mode-hook
                    'python-mode-hook
                    'lua-mode-hook
                    'c-mode-hook
                    'java-mode-hook
                    'js-mode-hook
                    'vlog-mode-hook
                    'css-mode-hook))
  (add-hook hook 'hideshowvis-enable))
;; [ Hide-Show ]-------------------------------------------------------[ End ]--


;; [ hide ]---------------------------------------------------------------------
;; hide region
(require 'hide-region)
(global-set-key (kbd "C-c h r") 'hide-region-hide)
(global-set-key (kbd "C-c h u") 'hide-region-unhide)

;; hide lines
(require 'hide-lines)
(autoload 'hide-lines "hide-lines" "Hide lines based on a regexp" t)
(global-set-key (kbd "C-c h l") 'hide-lines)
;; --------------------------------------------------------------------[ End ]--


;; [ folding ]------------------------------------------------------------------
;; folding (hiding) parts of the text
(require 'folding)
;; if always use folding
;; (if (load "folding" 'nomessage 'noerror)
;;     (folding-mode-add-find-file-hook))

;; autoload when turn on `folding-mode'
(autoload 'folding-mode          "folding" "Folding mode" t)
(autoload 'turn-off-folding-mode "folding" "Folding mode" t)
(autoload 'turn-on-folding-mode  "folding" "Folding mode" t)

(folding-add-to-marks-list 'ruby-mode "#{{{" "#}}}" nil t)
(folding-add-to-marks-list 'php-mode    "//{"  "//}"  nil t)
(folding-add-to-marks-list 'html-mode   "<!-- {{{ " "<!-- }}} -->" " -->" nil t)
(folding-add-to-marks-list 'verilog-mode "// {"  "// }"  nil t)
(folding-add-to-marks-list 'sh-mode "#{{{" "#}}}" nil t)
(folding-add-to-marks-list 'emacs-lisp-mode ";;{"  ";;}"  nil t)
(folding-add-to-marks-list 'c-mode "/* {{{ " "/* }}} */" " */" t)
;; --------------------------------------------------------------------[ End ]--


;; [ tabbar ]-------------------------------------------------------------------
(when (require 'tabbar nil t)
    (tabbar-mode t))
;; --------------------------------------------------------------------[ End ]--


;; [ Email ]--------------------------------------------------------------------
;;  (require 'email)
;;  (setq send-mail-function 'email-send)
;;  (setq sendmail-program "/usr/sbin/sendmail")
;;  (setq smtpmail-smtp-server "your.smtp.server.jp")
;;  (setq mail-user-agent 'message-user-agent)
;;  (setq message-send-mail-function 'message-smtpmail-send-it)
;;
;; --------------------------------------------------------------------[ End ]--


;; [ org ]----------------------------------------------------------------------
(when section-org
  (add-site-lisp-load-path "org/lisp/")
  (add-site-lisp-load-path "org/contrib/lisp/")
  (add-site-lisp-info-path "org/doc/")
  (add-site-lisp-load-path "org-jekyll-mode/")
  (load "org-conf"))
;; [ org ]-------------------------------------------------------------[ End ]--


;; [ eproject ]-----------------------------------------------------------------
(when section-eproject
  (add-site-lisp-load-path "eproject/")
  (load "eproject-conf"))
;; --------------------------------------------------------------------[ End ]--


;; [ vi ]------------------------------------------------------------------------
(when section-vi
    (add-site-lisp-load-path "evil/")
;;  (load "evil-conf")
;;
;;  (load "viper-conf")

    (add-site-lisp-load-path "vimpulse/")
;;  (require 'vimpulse)
)
;; --------------------------------------------------------------------[ End ]--


;; [ eshell ]-------------------------------------------------------------------
(when section-eshell
    (load "eshell-conf"))
;; --------------------------------------------------------------------[ End ]--


;; [ erc ]-----------------------------------------------------------------------
(when section-irc
    (load "erc-conf")
    (erc :server "irc.freenode.net" :port 6667 :nick "hjking")
)
;; --------------------------------------------------------------------[ End ]--


;; [ tramp ]---------------------------------------------------------------------
(when section-tramp
    (load "tramp-conf"))
;; --------------------------------------------------------------------[ End ]---


;; [ rot13 ]---------------------------------------------------------------------
;; perform Caesar ciphers
(when (require 'rot13 nil t)
    (load "rot13-conf"))
;; --------------------------------------------------------------------[ End ]--


;; [ doc-view ]-----------------------------------------------------------------
(when section-document-view
    (if is-after-emacs-23
        (load "doc-view-conf")))
;; --------------------------------------------------------------------[ End ]--


;; [ artist ]-------------------------------------------------------------------
(when section-artist
    (require 'artist-conf))
;; --------------------------------------------------------------------[ End ]--


;; [ muse ]---------------------------------------------------------------------
(when section-muse
    (add-site-lisp-load-path "muse/lisp/")
    (load "muse-conf"))
;; --------------------------------------------------------------------[ End ]--


;; [ ecb ]----------------------------------------------------------------------
(when section-ecb
  (add-site-lisp-load-path "ecb-2.40/")
  (add-site-lisp-info-path "ecb-2.40/info-help/")
  (load "ecb-conf"))
;; --------------------------------------------------------------------[ End ]--


;; [ drag-stuff ]---------------------------------------------------------------
(when section-drag-stuff
  ;; use M-up/down/right/left to move lines, regions, words up/down or right/left
  ;; Drag line
  ;; To drag a line up and down. Put the cursor on that line and press <C-S-up> and
  ;; <C-S-down>.

  ;; Drag lines
  ;; To drag several lines up and down. Select the lines you want to drag and
  ;; press <C-S-up> and <C-S-down>.

  ;; Drag region
  ;; A region can be dragged to the left and right. Select the region you want to
  ;; drag and press <C-S-left> and <C-S-right>.

  ;; Drag word
  ;; To drag a word. Place the cursor on the word and press <C-S-left> and <C-S-right>.
  (add-site-lisp-load-path "drag-stuff/")
  (require 'drag-stuff)
  (drag-stuff-mode t))
;; --------------------------------------------------------------------[ End ]--


;; [ mmm-mode ]-----------------------------------------------------------------
;; Multiple Major Modes coexist in one buffer
(when section-mmm-mode
    (add-site-lisp-load-path "mmm-mode/")
    (add-site-lisp-info-path "mmm-mode/")
    (load "mmm-mode-conf"))
;; --------------------------------------------------------------------[ End ]--

;;
;;;; Mode setting
;;;
;; [ Choosing Mode ]------------------------------------------------------------
;; use right mode for specified file
(setq auto-mode-alist
      (append
       '(("\\.txt\\'"                     . text-mode)
         ("/Message[0-9]*\\'"             . text-mode)
         ("README\\'"                     . text-mode)
         ("\\.doc\\'"                     . text-mode)
         ("\\.[Cc][Oo][Mm]\\'"            . text-mode)
         ("\\.bat\\'"                     . bat-generic-mode)
         ("\\.inf\\'"                     . inf-generic-mode)
         ;; C
         ("\\.c\\'"                       . c-mode)
         ("\\.ext\\'"                     . c-mode)
         ("\\.C\\'"                       . c++-mode)
         ("\\.cpp\\'"                     . c++-mode)
         ("\\.h\\'"                       . c++-mode)
         ("\\.java\\'"                    . java-mode)
         ;; shellscript
         ("/etc/profile"                  . sh-mode)
         ("/etc/bash_completion"          . sh-mode)
         ("\\.SH"                         . sh-mode)
         ("\\.bashrc"                     . sh-mode)
         ("\\.sh\\'"                      . shell-script-mode)
         ("\\.csh\\'"                     . shell-script-mode)
         ("\\.zsh\\'"                     . shell-script-mode)
         ("\\rc\\'"                       . shell-script-mode)
         ;; cperl-mode
         ("\\.PL$"                        . cperl-mode)
         ("\\.pl$"                        . cperl-mode)
         ("\\.perl$"                      . cperl-mode) ; git source tree
         ("\\.pm$"                        . cperl-mode)
         ("\\.t$"                         . cperl-mode)
         ("\\.psgi$"                      . cperl-mode)
         ("\\.comp$"                      . cperl-mode)
         ("\\.pl\\'"                      . cperl-mode)
         ("\\.perl\\'"                    . cperl-mode)
         ;; emacs
         ("\\.el\\'"                      . emacs-lisp-mode)
         ("\\.tex\\'"                     . latex-mode)
         ("\\.reg\\'"                     . reg-generic-mode)
         ("mutt-"                         . mail-mode)
         ;; makefile
         ("makefile"                      . makefile-mode)
         ("Makefile"                      . makefile-mode)
         ;; JavaScript
         ("\\.js$"                        . javascript-mode)
         ("\\.json$"                      . javascript-mode)
         ;; HTML
         ("\\.html\\'"                    . html-mode)
         ("\\.htm\\'"                     . html-mode)
         ("\\.bin\\'"                     . hexl-mode)
         ("\\.py\\'"                      . python-mode)
         ;; php
         ("\\.php$"                       . php-mode)
         ;; org-mode
         ("\\.org$"                       . org-mode)
         (".ssh/config\\'"                . ssh-config-mode)
         ("sshd?_config\\'"               . ssh-config-mode)
         ("ChangeLog\\'"                  . change-log-mode)
         ("change\\.log\\'"               . change-log-mode)
         ("changelo\\'"                   . change-log-mode)
         ("ChangeLog\\.[0-9]+\\'"         . change-log-mode)
         ("changelog\\'"                  . change-log-mode)
         ("changelog\\.[0-9]+\\'"         . change-log-mode)
         ("\\$CHANGE_LOG\\$\\.TXT"        . change-log-mode)
         ;; conf-mode
         ("\\.vhost$"                     . conf-mode)
         ("\\.hgrc$"                      . conf-mode)
         ("\\.rc$"                        . conf-mode)
         ("\\.*rc$"                       . conf-unix-mode)
         ("\\.cnf$"                       . conf-mode)
         ("\\.gitconfig$"                 . conf-mode)
         ("\\.gitmodules$"                . conf-mode)
         ("\\.offlineimaprc"              . conf-mode)
         ("^/etc/apache2/sites-"          . conf-mode)
         ("^/etc/ssh/sshd"                . conf-mode)
         ("^/etc/deb-packages"            . conf-mode)
         ("\\.screenrc"                   . conf-mode)
         ("\\.screenrc\.d/"               . conf-mode)
         ("\\.aliases$"                   . conf-mode)
         ;; ruby
         ("\\.pp$"                        . puppet-mode)
         ("\\.rhtml$"                     . ruby-mode)
         ("\\.rjs$"                       . ruby-mode)
         ("\\.rxml$"                      . ruby-mode)
         ("\\.erb$"                       . ruby-mode)
         ("\\.builder$"                   . ruby-mode)
         ("\\.rake$"                      . ruby-mode)
         ("\\.kick$"                      . ruby-mode)
         ("TODO\\'"                       . outline-mode)
         ("\\.[ck]?sh\\'\\|\\.shar\\'\\|/\\.z?profile\\'" . sh-mode)
         ("\\(/\\|\\`\\)\\.\\(bash_profile\\|z?login\\|bash_login\\|z?logout\\)\\'" . sh-mode)
         ("\\(/\\|\\`\\)\\.\\(bash_logout\\|[kz]shrc\\|bashrc\\|t?cshrc\\|esrc\\)\\'" . sh-mode)
         ("\\(/\\|\\`\\)\\.\\([kz]shenv\\|xinitrc\\|startxrc\\|xsession\\)\\'" . sh-mode)
       )
       auto-mode-alist
     ))


;; [ CSV Mode ]-----------------------------------------------------------------
(message "%d: >>>>> Loading [ CSV Mode ] Customization ...." step_no)
(setq step_no (1+ step_no))
;; major mode for editing comma-separated value files
(require 'csv-mode nil t)
;; field separators: a list of *single-character* strings
(setq csv-separators '("," ";"))
;; --------------------------------------------------------------------[ End ]--


;; [ Text Mode ]----------------------------------------------------------------
(message "%d: >>>>> Loading [ Text Mode ] Customization ...." step_no)
(setq step_no (1+ step_no))
;; default mode is Text Mode
(setq default-major-mode 'text-mode)
(defun my-textmode-startup ()
  (interactive)
;;  (filladapt-mode t)
;;  (flyspell-mode t)
  (setq tab-width 4))
(add-hook 'text-mode-hook 'my-textmode-startup)
;; --------------------------------------------------------------------[ End ]--


;; [ Debug Mode ]---------------------------------------------------------------
;;  (define-key debugger-mode-map "o" 'other-window)
;; --------------------------------------------------------------------[ End ]--


;; [ Occur Mode ]---------------------------------------------------------------
;; In isearch mode, ‘M-s o’ – call ‘occur’ with the current search term
(add-hook 'occur-mode-hook (lambda () (setq truncate-lines t)))
;; [ Occur Mode ]------------------------------------------------------[ End ]--


;; [ View Mode ]----------------------------------------------------------------
(message "%d: >>>>> Loading [ View Mode ] Customization ...." step_no)
(setq step_no (1+ step_no))
;; vim style
(setq view-mode-hook
  (lambda ()
    (define-key view-mode-map "h" 'backward-char)
    (define-key view-mode-map "l" 'forward-char)
    (define-key view-mode-map "j" 'next-line)
    (define-key view-mode-map "k" 'previous-line)
))
;; --------------------------------------------------------------------[ End ]--


;;;; ================ ProgrammingModes ================
;;;
;; [ HDL Mode ]-----------------------------------------------------------------
(when section-hdl
  (when section-verilog
    ;; Verilog mode
      (setq my-verilog-load-path (concat my-site-lisp-dir "verilog-mode/"))
      (add-site-lisp-load-path "verilog-mode/")
      (load "verilog-conf"))

  (when section-vlog
      ;; Vlog mode: The verilog code maker
      (setq my-vlog-load-path (concat my-site-lisp-dir "vlog-mode/"))
      (add-site-lisp-load-path "vlog-mode/")
      (load "vlog-conf"))

  (when section-vhdl
      ;; VHDL mode
      (setq my-vhdl-load-path (concat my-site-lisp-dir "vhdl-mode/"))
      (add-site-lisp-load-path "vhdl-mode/")
      (add-site-lisp-info-path "vhdl-mode/")
      (load "vhdl-conf"))

  ;; Systemc mode
  (autoload 'systemc-mode "systemc-mode" "Mode for SystemC files." t)
  ;; (add-hook 'systemc-mode-hook 'c++-mode-hook)
  ; (require 'auto-complete-verilog)
)
;; --------------------------------------------------------------------[ End ]--


;; [ Python Mode ]--------------------------------------------------------------
(when section-python
    (setq python-load-path (concat my-site-lisp-dir "python/"))
    (add-site-lisp-load-path "python/")
    (load "python-conf"))
;; --------------------------------------------------------------------[ End ]--


;; [ Perl Mode ]----------------------------------------------------------------
;; cperl-mode is preferred to perl-mode,
;; replace the standard perl-mode with cperl-mode
(when section-perl
    (setq perl-load-path (concat my-site-lisp-dir "cperl/"))
    (add-site-lisp-load-path "cperl/")
    (load "cperl-conf")

    (setq pde-load-path (concat my-site-lisp-dir "pde/lisp/"))
    (add-site-lisp-load-path "pde/lisp/")
    (add-site-lisp-info-path "pde/lisp/doc/")
    (load "pde-load"))
;; --------------------------------------------------------------------[ End ]--


;; [ Shell Mode ]---------------------------------------------------------------
;; set shell type
(when section-shell-mode
    (when linuxp
        (setq shell-file-name "/bin/bash"))
    (load "shell-mode-conf"))
;; --------------------------------------------------------------------[ End ]--


;; [ Shell script Mode ]--------------------------------------------------------
(message ">>>>> Loading [ Shell Script Mode ] Customization ....")
(defun my-shellscript-startup ()
  "Setup shell script mode."
  (interactive)
  (setq sh-basic-offset '8)
  (setq sh-indentation '8)
  (setq sh-indent-comment t)
  (setq indent-tabs-mode t)
  (setq sh-indent-for-case-label '0)
  (setq sh-indent-for-case-alt '+))
(add-hook 'sh-mode-hook 'my-shellscript-startup)
;; --------------------------------------------------------------------[ End ]--


;; [ Makefile Mode ]------------------------------------------------------------
(message ">>>>> Loading [ Makefile Mode ] Customization ....")
(defun my-makefile-startup ()
  "Setup how I like editing makefiles."
  (interactive)
;;    (start-programing-mode)
;;    (local-set-key "\C-css" 'insert-script-seperator-line)
;;    (local-set-key "\C-csh" 'insert-script-section-header)
;;    (local-set-key "\C-csb" 'insert-script-big-header)
)
(add-hook 'makefile-mode-hook 'my-makefile-startup)
;; --------------------------------------------------------------------[ End ]--


;; [ C Mode ]-------------------------------------------------------------------
;; including c++ mode
(when section-c-mode
    ;; CC Mode is an Emacs and XEmacs mode for editing C
    ;; and other languages with similar syntax
    (add-site-lisp-load-path "cc-mode/")
    (add-site-lisp-info-path "cc-mode/")
    (load "c-mode-conf"))
;; --------------------------------------------------------------------[ End ]--


;; [ Markdown Mode ]------------------------------------------------------------
(when section-markdown-mode
    ;; Markdown mode - TAB for <pre></pre> block
    (add-site-lisp-load-path "markdown-mode/")
    (load "markdown-mode-conf"))
;; --------------------------------------------------------------------[ End ]--


;; --[ Lisp Mode ]--------------------------------------------------------------
(when section-slime
    (add-site-lisp-load-path "slime/")
    (add-site-lisp-load-path "slime/contrib/")
    (add-site-lisp-info-path "slime/doc")
    (load "slime-conf"))
;; --------------------------------------------------------------------[ End ]--


;; [ Emacs Lisp Mode ]----------------------------------------------------------
(when section-elisp-mode
    (load "elisp-mode-conf"))

;; Edebug
(setq edebug-trace t)
;; --------------------------------------------------------------------[ End ]--


;; [ Comint Mode ]--------------------------------------------------------------
(message ">>>>> Loading [ Comint Mode ] Customization ....")
(add-hook 'comint-mode-hook
          '(lambda ()
             (setq comint-scroll-show-maximum-output t)
             (setq comint-input-ignoredups t)
             (setq comint-input-ring-size 64)
             (setq comint-buffer-maximum-size (expt 2 16))))
;; --------------------------------------------------------------------[ End ]--

;;;; ================ ProgrammingModes End ================

;; [ undo ]---------------------------------------------------------------------
(when section-undo
    (add-site-lisp-load-path "undo-tree/")
    (require 'undo-tree)
    ;; replace the standard Emacs' undo system
    (global-undo-tree-mode)
    (defalias 'redo 'undo-tree-redo)
    ;; (global-set-key (kbd "C-z") 'undo)  ; CTRL+Z
    ;; (global-set-key (kbd "C-S-z") 'redo)  ; CTRL+Shift+Z
    (global-set-key (kbd "M-z") 'undo)  ; ALT+Z
    (global-set-key (kbd "M-S-z") 'redo)  ; ALT+Shift+Z
)
;; --------------------------------------------------------------------[ End ]--


;; [ ace-jump ]-----------------------------------------------------------------
(setq my-ace-jump-mode-load-path (concat my-site-lisp-dir "ace-jump-mode/"))
(add-site-lisp-load-path "ace-jump-mode/")
(load "ace-jump-conf")
;; --------------------------------------------------------------------[ End ]--

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Encoding setting
(when section-coding
    (load "encoding-conf"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; [ EMMS ]---------------------------------------------------------------------
;; Music player
;; set the location of the music player in Win
(when win32p
  (if (file-directory-p "D:/Tools/MPlayer")
      (add-to-list 'exec-path "D:/Tools/MPlayer")
    (message "*** Please install MPlayer first!!")))
(when section-emms
    (setq my-emms-load-path (concat my-site-lisp-dir "emms/"))
    (add-site-lisp-load-path "emms/")
    (add-site-lisp-info-path "emms/")
    (load "emms-conf"))

(add-site-lisp-load-path "DoubanMusic/")
(autoload 'douban-music "douban-music-mode" nil t)

;; [ EMMS ]------------------------------------------------------------[ End ]--


;; --[ tree ]-------------------------------------------------------------------
(add-site-lisp-load-path "tree/")
(load "tree-conf")
;; --------------------------------------------------------------------[ End ]--


;; --[ epg ]--------------------------------------------------------------------
(load "epg-conf")
;; --------------------------------------------------------------------[ End ]--


;; --[ gdb ]--------------------------------------------------------------------
(load "gdb-conf")
;; --------------------------------------------------------------------[ End ]--


;; --[ w3m ]--------------------------------------------------------------------
;; Web browser
(when section-w3m
    (setq my-w3m-path (concat my-site-lisp-dir "emacs-w3m/"))
    (add-site-lisp-load-path "emacs-w3m/")
    (add-site-lisp-info-path "emacs-w3m/doc/")
    (load "w3m-conf"))
;; --------------------------------------------------------------------[ End ]--


;; --[ smex ]-------------------------------------------------------------------
;; Smart M-x
;; remember recently and most frequently used commands
(when section-smex
    (setq my-smex-path (concat my-site-lisp-dir "smex/"))
    (add-site-lisp-load-path "smex/")
    (load "smex-conf"))
;; --------------------------------------------------------------------[ End ]--


;; --[ xcscope ]----------------------------------------------------------------
(when linuxp
  (when section-cscope
    (add-site-lisp-load-path "xcscope/")
    (load "xcscope-conf")))
;; --------------------------------------------------------------------[ End ]--


;; --[ wl ]---------------------------------------------------------------------
;; Wanderlust is a mail/news management system with IMAP4rev1 support for Emacs.
(when section-wl
    (add-site-lisp-load-path "wl/wl/")
    (add-site-lisp-info-path "wl/doc/")
    (require 'wl))
;; --------------------------------------------------------------------[ End ]--


;; --[ Color Theme ]------------------------------------------------------------
(when section-color-theme
    (load "color-theme-conf"))
;; --[ Color Theme ]---------------------------------------------------[ End ]--


;; [ weibo ]--------------------------------------------------------------------
(when section-weibo
    (add-site-lisp-load-path "weibo/")
    (load "weibo-conf"))
;; [ weibo ]-----------------------------------------------------------[ End ]--


;; [ multiple-cursors ]---------------------------------------------------------
;; https://github.com/magnars/multiple-cursors.el
(add-site-lisp-load-path "multiple-cursors/")
(require 'multiple-cursors)
;; Add a cursor to each line in an active region that spans multiple lines
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; Add multiple cursors not based on continuous lines, but based on keywords in the buffer
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;; [ multiple-cursors ]------------------------------------------------[ End ]--


;; [ hungry-delete ]------------------------------------------------------------
(add-site-lisp-load-path "hungry-delete/")
(require 'hungry-delete)
(global-hungry-delete-mode)
;; --------------------------------------------------------------------[ End ]--


;; [ sr-speedbar ]--------------------------------------------------------------
(load "sr-speedbar-conf")
;; [ sr-speedbar ]-----------------------------------------------------[ End ]--


;; [ popwin ]-------------------------------------------------------------------
(add-site-lisp-load-path "popwin/")
(load "popwin-conf")
;; [ popwin ]-----------------------------------------------------------[ End ]--


;; [ guide-key ]-----------------------------------------------------------------
(add-site-lisp-load-path "guide-key/")
(load "guide-key-conf")
;; ---------------------------------------------------------------------[ End ]--


;; [ volatile-highlights ]-------------------------------------------------------
;; highlight changes made by commands such as undo, yank-pop, etc.
(add-site-lisp-load-path "volatile-highlights/")
(require 'volatile-highlights)
(volatile-highlights-mode t)
;; ---------------------------------------------------------------------[ End ]--


;; --[ Help ]-------------------------------------------------------------------
(when section-help
  (message "%d: >>>>> Loading [ Help ] Customization ...." step_no)
  (setq step_no (1+ step_no))
  ;; check all variables and non-interactive functions as well
  ;; Make C-h a act as C-u C-h a
  (setq apropos-do-all t)
  (add-to-list 'Info-default-directory-list (concat my-emacs-dir "info/"))
  ;; add apropos help about variables (bind `C-h A' to `apropos-variable')
  (GNUEmacs (define-key help-map (kbd "A") 'apropos-variable))
  ;; Help is provided according to the buffer’s major mode
  (load "info-look-conf")

  ;; show function name
  (add-hook 'prog-mode-hook (lambda () (which-function-mode 1)))

  (setq tooltip-delay 1)

  ;; [ show tip ]-----------------------------------------------------------------
  (add-site-lisp-load-path "clippy/")
  (require 'clippy)
  ;; --------------------------------------------------------------------[ End ]--
)
;; --[ Help ]----------------------------------------------------------[ End ]--


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ==== Define Function ====
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; Function Set for Edit
;;; for more functions,  see .emacs.d/conf/defuns.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'word-at-point "thingatpt" nil t)
(defun replace-word-at-point (from to)
  "Replace word at point."
  (interactive (let ((from (word-at-point)))
     (list from (query-replace-read-to from "Replace" nil))))
  (query-replace from to))

;; delete all the trailing whitespaces and tabs across the current buffer
(defun my-delete-trailing-whitespaces-and-untabify ()
  "Delete all the trailing white spaces, and convert all tabs to multiple
spaces across the current buffer."
  (interactive "*")
  (delete-trailing-whitespace)
  (untabify (point-min) (point-max)))
;; (global-set-key (kbd "C-c t") 'my-delete-trailing-whitespaces-and-untabify)

;; change the background to yellow when if open a read-only file
;; (add-hook 'find-file-hooks
;;     (lambda ()
;;       (when buffer-read-only
;;         (set-background-color "yellow"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ==== Gloabal Key Binding ====
(load "bindings")

;;
;; ergoemacs-keybindings
;;
(when section-ergoemacs-mode
    (setq ergoemacs-keybindings-load-path (concat my-site-lisp-dir "ergoemacs-mode/"))
    (add-site-lisp-load-path "ergoemacs-mode/")
    (add-site-lisp-info-path "ergoemacs-mode/")

    ;; load ErgoEmacs keybinding
    (require 'ergoemacs-mode)

    ;; turn on minor mode ergoemacs-mode
    (setq ergoemacs-theme nil)  ;; Uses standard ergoemacs keyboard theme
    (setq ergoemacs-keyboard-layout "us") ;; Assumes QWERTY keyboard layout
    (ergoemacs-mode 1))

(add-site-lisp-load-path "guru-mode/")
(require 'guru-mode)
(guru-global-mode +1)
(setq guru-warn-only t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Display missed packages
(when section-debugging
  (if missing-packages-list
    (progn
      (message "*** ------->>> W A R N I N G <<<-------")
      (message "*** Packages not found: %S" missing-packages-list)
      (message "*** -----------------------------------"))
))


;; [ session ]-------------------------------------------------------------------
;; session
(when section-session
    (add-site-lisp-load-path "session/lisp/")
    (load "session-conf"))
;; --------------------------------------------------------------------[ End ]--

;; Save a minibuffer input history
(eval-after-load 'savehist
  '(setq savehist-save-minibuffer-history t
        savehist-autosave-interval 180))
(savehist-mode t)

;; [ desktop ]------------------------------------------------------------------
(when section-desktop
    (load "desktop-conf"))
;; --------------------------------------------------------------------[ End ]--


;; [ workgroups2 ]--------------------------------------------------------------
(when section-workgroups
    (add-site-lisp-load-path "workgroups2/src/")
    (load "workgroups-conf"))
;; [ workgroups2 ]-----------------------------------------------------[ End ]--


;; [ diminish ]-----------------------------------------------------------------
;; diminish keeps the modeline tidy
(diminish 'abbrev-mode "Abv")
(diminish 'undo-tree-mode)
(diminish 'dired-view-minor-mode)
(diminish 'auto-revert-mode)
(diminish 'guru-mode)
(diminish 'workgroups-mode)
(diminish 'anzu-mode)
(diminish 'guide-key-mode)
(diminish 'smartparens-mode)
(diminish 'drag-stuff-mode)
(diminish 'volatile-highlights-mode)
;;  (diminish 'wrap-region-mode)
;;  (diminish 'yas/minor-mode)
;; [ diminish ]--------------------------------------------------------[ End ]--


(load custom-file 'noerror)

;; (setq debug-on-error nil)
(message ">>>>> Emacs startup time: %d seconds."
         (time-to-seconds (time-since emacs-load-start-time)))

(message "***** >>>>> [ Loading my Emacs Init File Completed!! ] <<<<< *****")
