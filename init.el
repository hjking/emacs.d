
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

;; I don't use XEmacs.  This file does not work with XEmacs.
(when (featurep 'xemacs)
  (error "This .emacs file does not work with XEmacs."))

;; uptimes
(setq emacs-load-start-time (current-time))

;; turn on Common Lisp support
(require 'cl)  ; provides useful things like `loop' and `setf'
(require 'cl-lib)

;; allow quick include/exclude of setup parts
(defvar section-environment t)  ; required
(defvar section-loading-libraries t)  ; required
(defvar section-debugging t)
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
(defvar section-gnus nil)
(defvar section-eshell t)
(defvar section-hdl t)
(defvar section-verilog nil)
(defvar section-vlog t)
(defvar section-vhdl nil)
(defvar section-emacs-server nil)
(defvar section-org t)
(defvar section-eproject nil)
(defvar section-ecb nil)
(defvar section-session t)
(defvar section-desktop nil)
(defvar section-muse nil)
(defvar section-cvs nil)
(defvar section-svn t)
(defvar section-git t)
(defvar section-emms t)
(defvar section-vm nil)
(defvar section-ac nil)
(defvar section-company t)
(defvar section-helm nil)
(defvar section-icicles nil)
(defvar section-scratch t)
(defvar section-c-mode t)
(defvar section-markdown-mode t)
(defvar section-elisp-mode t)
(defvar section-html-mode t)
(defvar section-shell-mode t)
(defvar section-defuns t)
(defvar section-alias t)
(defvar section-vi nil)
(defvar section-artist nil)
(defvar section-yasnippet t)
(defvar section-cygwin t)
(defvar section-package t)
(defvar section-tramp t)
(defvar section-cedet nil)
(defvar section-cedet-1.1 t)
(defvar section-drag-stuff t)
(defvar section-mmm-mode t)
(defvar section-csv-mode t)
(defvar section-table nil)
(defvar section-undo t)
(defvar section-header t)
(defvar section-irc nil)
(defvar section-w3m nil)
(defvar section-smex t)
(defvar section-slime t)
(defvar section-cscope t)
(defvar section-wl nil)
(defvar section-color-theme t)
(defvar section-weibo t)
(defvar section-workgroups nil)
(defvar section-powerline nil)
(defvar section-sml nil)

(random t)

;;;###autoload
(defmacro define-kbd  (keymap key def) `(define-key ,keymap (kbd ,key) ,def))
;;;###autoload
(defmacro local-set-kbd  (key command)    `(local-set-key (kbd ,key) ,command))
;;;###autoload
(defmacro global-set-kbd (key command)    `(global-set-key (kbd ,key) ,command))

(when section-debugging
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

  (defvar my-emacs-init-file (or load-file-name buffer-file-name))
  (defvar my-emacs-dir (file-name-directory my-emacs-init-file)
      "The Root directory of my .emacs.d")
  ;; My site-lisp directory
  (defvar my-site-lisp-dir (concat my-emacs-dir "vendor/")
      "This directory keeps Emacs Lisp packages")
  ;; My configuration files directory
  (defvar my-site-lisp-conf-dir (concat my-emacs-dir "conf/")
      "This directory keeps my Emacs Lisp for packages")
  ;; Personal configuration files
  (defvar my-personal-dir (concat my-emacs-dir "personal/")
      "This directory keeps my personal configuration")
  ;; Directory for temporary file
  (defvar my-cache-dir (concat my-emacs-dir "cache/")
      "This directory keeps cache files")
  (unless (file-exists-p my-cache-dir)
      (make-directory my-cache-dir))
  (defmacro add-load-path (path)
      `(setq load-path (append (list, path) load-path)))
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

  ;; Load all elisp files in directory
  ; (if (file-exists-p my-site-lisp-conf-dir)
  ;     (dolist (file (directory-files my-site-lisp-conf-dir t "\\.el$"))
  ;        (load file)))

  ;; Add directories to load path
  ; (dolist (project (directory-files my-site-lisp-dir t "\\w+"))
  ;     (when (file-directory-p project)
  ;       (add-to-list 'load-path project)))
  (when win32p
  ;     (message "We are in Windows Platform")
  ;     (setq my-home "F:/Kuaipan/Workspace/src")
  ;     (setenv "HOME" my-home)
  ;     (setenv "PATH" (concat my-home ";" (getenv "PATH")))
    (setenv "PATH" (concat "D:/DEV/global/bin;" (getenv "PATH")))
    (setq exec-path (append exec-path '("D:/DEV/global/bin")))
  )
)
;; --[ Load Path ]-----------------------------------------------------[ End ]--


;; --[ Environment ]------------------------------------------------------------

(require 'init-compat)

(when section-environment
  (load "env-conf"))

;; --------------------------------------------------------------------[ End ]--


;; --[ cygwin setting ]---------------------------------------------------------
(when section-cygwin
  (when win32p
    (defconst my-cygwin-dir "d:/DEV/cygwin/" "Cygwin root path.")
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
(when (window-system)
  (setq-default
        x-select-enable-primary nil ; stops killing/yanking interacting with primary X11 selection
        x-select-enable-clipboard t ; makes killing/yanking interact with clipboard X11 selection
        mouse-drag-copy-region nil  ; stops selection with a mouse being immediately injected to the kill ring
        x-stretch-cursor t
        ;; middle button for paste
        mouse-yank-at-point t
        ;; Active region should set primary X11 selection.
        select-active-regions t
        ;; Set middle mouse button to paste from primary X11 selection.
        ))
(global-set-key [mouse-2] 'mouse-yank-primary)
;; Rebind to new clipboard functions when available.
(when (fboundp 'clipboard-kill-region)
  (global-set-key [remap kill-region] 'clipboard-kill-region))
(when (fboundp 'clipboard-kill-ring-save)
  (global-set-key [remap kill-ring-save] 'clipboard-kill-ring-save))
(when (fboundp 'clipboard-yank)
  (global-set-key [remap yank] 'clipboard-yank))

;; Save clipboard strings into kill ring before replacing them
(setq save-interprogram-paste-before-kill t)

;; either copies the current region to the system clipboard or,
;; if no region is active, yanks the clipboard at point
(defun clipboard-dwim ()
  (interactive)
  (if (region-active-p)
      (clipboard-kill-ring-save (region-beginning) (region-end))
    (clipboard-yank)))
(global-set-key (kbd "C-c w") 'clipboard-dwim)

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

;; use 24-hour format
(setq display-time-24hr-format t)
(setq display-time-interval 10)
;; display time, day and date
(setq display-time-day-and-date t)
(setq display-time-format "%R %y-%m-%d")
(display-time)

(message ">>>>> Loading [ Misc ] Customization Done")

(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)

;;use proxy server
;;(setq url-proxy-services '(("http" . "proxy.km.local:8080")))

;;; Library

;; dash
(require 'dash)
(dash-enable-font-lock)

;; sams-lib
; (require 'sams-lib nil t)

;; --[ Basic ]---------------------------------------------------------[ End ]--


;; --[ Font ]-------------------------------------------------------------------
(load "font-conf")
;; --[ Font ]----------------------------------------------------------[ End ]--


;; --[ Frame Display ]----------------------------------------------------------
(when section-ui
    (load "ui-conf"))
;; --[ Frame Display ]-------------------------------------------------[ End ]--


;; --[ Encoding ]---------------------------------------------------------------
;;;; Encoding setting
(when section-coding
    (load "encoding-conf"))
;; --[ Encoding ]------------------------------------------------------[ End ]--


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
; (when section-cedet
;   (load "cedet-conf"))
;; --------------------------------------------------------------------[ End ]--


;; --[ Minibuffer ]-------------------------------------------------------------
(when section-minibuffer
  (message "%d: >>>>> Loading [ Minibuffer ] Customization ...." step_no)
  (setq step_no (1+ step_no))
  ;; ignore case when reading a file name completion
  (setq read-file-name-completion-ignore-case t)
  ;; ignore case when reading a buffer name
  (setq read-buffer-completion-ignore-case t)
  ;; do not consider case significant in completion (GNU Emacs default)
  (setq completion-ignore-case t)
  ;; minibuffer window expands vertically as necessary to hold the text that you
  ;; put in the minibuffer
  (setq resize-minibuffer-mode t)
  ;; Enable recursive minibuffer
  (setq enable-recursive-minibuffers t)
  ;; auto-complete on in minibuffer
  (unless is-after-emacs-23
      partial-completion-mode 1)
  ;; auto-complete in minibuffer when execute M-x functions and variables
  (icomplete-mode 1)
  ;; Ignore case when using completion for file names
  (setq read-file-name-completion-ignore-case t)
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
    ;; change buffer, or focus, disable the current buffer’s mark
    (transient-mark-mode t)    ; highlight text selection
    ;;;;;;;;;;;;
    ;; delsel.el
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
(setq auto-window-vscroll nil)
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
(require 'smooth-scrolling)
(setq smooth-scroll-margin 4)
;; --[ Scrolling ]-----------------------------------------------------[ End ]--


;; --[ search and replace ]-----------------------------------------------------
(when section-search
    (add-site-lisp-load-path "visual-regexp/")
    (add-site-lisp-load-path "anzu/")
    (load "search-conf"))
;; --[ Search and Replace ]--------------------------------------------[ End ]--


;; --[ Mode Line ]--------------------------------------------------------------

;; mode-line-stats
;; Display CPU/Memory/Disk status on mode line
;; (add-site-lisp-load-path "mode-line-stats/")
;; (load "mode-line-stats-conf")

;; Show buffer size in mode-line
(size-indication-mode 1)
;; display time
(display-time-mode 1)
;; Enable or disable the display of the current line number
(line-number-mode 1)
;; Enable or disable the display of the current column number
(column-number-mode 1)
;; Enable or disable laptop battery information
; (display-battery-mode 1)

;; displays the current function name in the mode line
(require 'which-func)
; (setq which-func-unknown "unknown")
(setq which-func-unknown "n/a")
; (add-to-list 'which-func-modes 'org-mode)
; (add-to-list 'which-func-modes 'ruby-mode)
; (add-to-list 'which-func-modes 'emacs-lisp-mode)
; (add-to-list 'which-func-modes 'c-mode)
; (add-to-list 'which-func-modes 'c++-mode)
; (add-to-list 'which-func-modes 'python-mode)
; (add-to-list 'which-func-modes 'perl-mode)
; (add-to-list 'which-func-modes 'verilog-mode)
; (add-to-list 'which-func-modes 'html-mode)
; (add-to-list 'which-func-modes 'sh-mode)
; (eval-after-load "which-func"
;       '(setq which-func-modes '(java-mode c++-mode org-mode)))
(which-func-mode 1)
(which-function-mode 1)

;; use inactive face for mode-line in non-selected windows
(setq mode-line-in-non-selected-windows nil)

;; set mode-line-format
(load "mode-line-conf")

;; [ powerline ]
(when section-powerline
    (add-site-lisp-load-path "powerline/")
    (load "powerline-conf"))

;; [ Smart Mode Line ]
(when section-sml
  (add-site-lisp-load-path "smart-mode-line/")
  (require 'smart-mode-line)
  (setq sml/position-percentage-format "%p")
  (setq sml/shorten-directory t)
  (setq sml/shorten-modes t)
  (setq sml/name-width 25)
  (setq sml/mode-width 'full)

  (add-to-list 'sml/hidden-modes " AC")
  (add-to-list 'sml/hidden-modes " SP")
  (add-to-list 'sml/hidden-modes " Fill")
  (add-to-list 'sml/hidden-modes " hs")
  (add-to-list 'sml/hidden-modes " ing")
  (add-to-list 'sml/hidden-modes " vl")
  (add-to-list 'sml/hidden-modes " GG")
  ; (sml/apply-theme 'dark)  ;; respectful/light
  ; (sml/setup)
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
(setq require-final-newline t)
;; Ask me whether to add a final newline to files which don't have one
;; (setq require-final-newline 'ask)

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
(setq my-auto-save-dir (concat my-emacs-dir "auto-save/"))
(dolist (dir (list my-backup-dir my-auto-save-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
(add-to-list 'backup-directory-alist '(cons "." my-backup-dir))
(setq auto-save-list-file-name (concat my-auto-save-dir "auto-save"))
; (setq auto-save-file-name-transforms '((".*" "auto-save-list" t))
;       auto-save-list-file-prefix (concat auto-saves-dir ".saves-"))
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
;;;;;;;;;;
;; diff.el
;; default to unified diffs
(setq diff-switches (list "-b" "-u"))
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
(setq uniquify-separator ":")
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
;; --[ Buffer Handling ]-----------------------------------------------[ End ]--


;; [ session ]------------------------------------------------------------------
;; remembers your location in a file when saving files
(load "saveplace-conf")

;; History
(load "savehist-conf")

(load "recentf-conf")
;; --------------------------------------------------------------------[ End ]--


;; --[ Window ]-----------------------------------------------------------------
;; use "C-c <--" back to previous window layout
(when section-windows
    (load "window-conf"))
;; --------------------------------------------------------------------[ End ]--


;; --[ Indentation ]------------------------------------------------------------
(when section-indentation
  (load "indent-conf"))
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
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
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
;; (add-site-lisp-load-path "smartparens/")
(load "parens-conf")
;; --[ Parentheses ]---------------------------------------------------[ End ]--


;; [ Emacs Server ]-------------------------------------------------------------
;;; EmacsClient
(when section-emacs-server
  (require 'server nil t)
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

;; ggtags: Emacs frontend to GNU Global source code tagging system
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'cperl-mode 'python-mode)
              (ggtags-mode 1))))
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
(add-hook 'prog-mode-hook 'auto-fill-mode)
(add-hook 'message-mode-hook (lambda ()
                                (setq fill-column 80)
                                (turn-on-auto-fill)))

;; LongLines
;; automatically wrap long lines after the last word before ‘fill-column’
; (autoload 'longlines-mode
;   "longlines.el"
;   "Minor mode for automatically wrapping long lines." t)
; (when (load "longlines" t)
;     (setq longlines-show-hard-newlines t))
;; (add-hook 'text-mode-hook 'longlines-mode)


;; visual-line-mode
;; wrap a line right before the window edge
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
;; (global-visual-line-mode -1) ;; Disable wrapping lines at word boundaries
(global-visual-line-mode t)
;; move around lines based on how they are displayed, rather than the actual line
(setq line-move-visual t)
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
; (setq vc-diff-switches '("-b" "-B" "-u"))
(setq vc-diff-switches diff-switches)
;; Follow symlinks
(setq vc-follow-symlinks t)
;; update VCS info on revert
(setq auto-revert-check-vc-info t)
(defadvice vc-next-action (before save-before-vc first activate)
  "Save all buffers before any VC next-action function calls."
  (hjking/save-all-file-buffers))

(defadvice vc-diff (before save-before-vc-diff first activate)
  "Save all buffers before vc-diff calls."
  (hjking/save-all-file-buffers))

;; *** --- PCL-CVS
(when section-cvs
  (when (require 'pcvs nil t)
      (load "pcvs-conf")
))

;; *** --- Subversion
(when section-svn
  (when (require 'psvn nil t)
      (load "psvn-conf")
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


;; [ column-marker ]------------------------------------------------------------
;; highlight columns 75, 80, 100 in some modes
(load "column-marker-conf")
;; [ column-marker ]---------------------------------------------------[ End ]--


;; [ volatile-highlights ]-------------------------------------------------------
;; highlight changes made by commands such as undo, yank-pop, etc.
(add-site-lisp-load-path "volatile-highlights/")
(require 'volatile-highlights)
(volatile-highlights-mode t)
;; ---------------------------------------------------------------------[ End ]--


;; [ auto-header ]--------------------------------------------------------------
(when section-header
    (load "header2-conf"))
;; [ auto-header ]----------------------------------------------------[ End ]---


;; [ goto change ]--------------------------------------------------------------
(require 'goto-chg)
;; [ goto change ]-----------------------------------------------------[ End ]--


;; [ helm ]---------------------------------------------------------------------
;; available for Emacs 22/23
(when section-helm
    ;; helm is new version of anything
    ; (add-site-lisp-load-path "emacs-helm/")
    ; (add-site-lisp-info-path "emacs-helm/doc/")
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
  ; (add-site-lisp-load-path "company-mode/")
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
;; fiplr, find-file-in-project, ffap
(load "find-file-conf")
;; projectile
(load "projectile-conf")
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
(when (require 'hideshow nil t)
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
(load "tabbar-conf")
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
  ; (add-site-lisp-load-path "ecb-2.40/")
  ; (add-site-lisp-info-path "ecb-2.40/info-help/")
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
         ("\\.bin\\'"                     . hexl-mode)
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
         ("\\.bat$"                       . dos-mode)
       )
       auto-mode-alist
     ))


;; [ CSV Mode ]-----------------------------------------------------------------
;; major mode for editing comma-separated value files
(when section-csv-mode
  (message "%d: >>>>> Loading [ CSV Mode ] Customization ...." step_no)
  (setq step_no (1+ step_no))
  (autoload 'csv-mode "csv-mode" "Major mode for editing comma-separated value files." t)
  (add-auto-mode 'csv-mode "\\.[Cc][Ss][Vv]\\'")
  (autoload 'csv-nav-mode "csv-nav-mode" "Major mode for navigating comma-separated value files." t)
  ;; field separators: a list of *single-character* strings
  (setq csv-separators '("," ";" "|" " "))
  )
;; --------------------------------------------------------------------[ End ]--


;; [ Text Mode ]----------------------------------------------------------------
(message "%d: >>>>> Loading [ Text Mode ] Customization ...." step_no)
(setq step_no (1+ step_no))
;; default mode is Text Mode
(setq-default major-mode 'text-mode)
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
(defun my-view-mode-hook ()
  "Set up some conveniences for Emacs Lisp."
  (local-set-key "h" 'backward-char)
  (local-set-key "l" 'forward-char)
  (local-set-key "j" 'next-line)
  (local-set-key "k" 'previous-line)
  )
(add-hook 'view-mode-hook 'my-view-mode-hook)
;; --------------------------------------------------------------------[ End ]--


;;;; ================ ProgrammingModes ================
;;;
;; [ HDL Mode ]-----------------------------------------------------------------
(when section-hdl
  (when section-verilog
    ;; Verilog mode
      (setq my-verilog-load-path (concat my-site-lisp-dir "verilog-mode/"))
      (add-site-lisp-load-path "verilog-mode/")
      (load "verilog-conf")
      ; (require 'auto-complete-verilog)
      )

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
    (load "perl-conf")

    ; (setq pde-load-path (concat my-site-lisp-dir "pde/lisp/"))
    ; (add-site-lisp-load-path "pde/lisp/")
    ; (add-site-lisp-info-path "pde/lisp/doc/")
    ; (load "pde-load")
    )
;; --------------------------------------------------------------------[ End ]--


;; [ Shell Mode ]---------------------------------------------------------------
;; invoke a shell
(when section-shell-mode
    (when linuxp
        (setq shell-file-name "/bin/bash"))
    (load "shell-mode-conf"))
;; --------------------------------------------------------------------[ End ]--


;; [ Shell script Mode ]--------------------------------------------------------
(load "sh-mode-conf")
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


;; --[ HTML Mode ]--------------------------------------------------------------
(when section-html-mode
  ;; HTML
  (add-to-list 'auto-mode-alist '("\\.html\\'"                    . html-mode))
  (add-to-list 'auto-mode-alist '("\\.htm\\'"                     . html-mode))

  (require 'zencoding-mode)
  (add-hook 'sgml-mode-hook 'zencoding-mode)
  (add-hook 'html-mode-hook 'zencoding-mode)

  (load "web-mode-conf")
)
;; --------------------------------------------------------------------[ End ]--


;; [ Comint Mode ]--------------------------------------------------------------
(message ">>>>> Loading [ Comint Mode ] Customization ....")
(add-hook 'comint-mode-hook
          '(lambda ()
             ;; Scroll automatically on new output.
             (setq comint-scroll-to-bottom-on-output 'others)
             (setq comint-scroll-show-maximum-output t)
             (setq comint-move-point-for-output 'others)
             ;; Scroll buffer to bottom in active frame on input.
             (setq comint-scroll-to-bottom-on-input 'this)
             ;; Make the prompt read only.
             (setq comint-prompt-read-only t)
             (setq comint-input-ignoredups t) ;; Don't store duplicates in history.
             (setq comint-input-ring-size 100) ;; Set a decent input history size.
             (setq comint-buffer-maximum-size (expt 2 16))
             ;;; (Bindings) ;;;
             ;; Cycling through command history.
             (define-key comint-mode-map [up] 'comint-previous-matching-input-from-input)
             (define-key comint-mode-map [down] 'comint-next-matching-input-from-input)
             ;; Skip past prompt.
             (define-key comint-mode-map [C-left] 'comint-bol)
             ))
;; --------------------------------------------------------------------[ End ]--

;;;; ================ ProgrammingModes End ================

;; [ undo ]---------------------------------------------------------------------
(when section-undo
    (add-site-lisp-load-path "undo-tree/")
    (require 'undo-tree)
    ;; replace the standard Emacs' undo system
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)
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


;; [ EMMS ]---------------------------------------------------------------------
;; Music player
;; set the location of the music player in Win
(when section-emms
    (setq my-emms-load-path (concat my-site-lisp-dir "emms/"))
    (add-site-lisp-load-path "emms/lisp/")
    (add-site-lisp-info-path "emms/doc/")
    ; (load "emms-conf")
    )
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

  (setq tooltip-delay 1)

  ;; [ show tip ]---------------------------------------------------------------
  (add-site-lisp-load-path "clippy/")
  (require 'clippy)
  ;; ------------------------------------------------------------------[ End ]--
)
;; --[ Help ]----------------------------------------------------------[ End ]--

(autoload 'turn-on-stripe-buffer-mode "stripe-buffer" "" nil)
(autoload 'turn-on-stripe-table-mode "stripe-buffer" "" nil)
(add-hook 'dired-mode-hook 'turn-on-stripe-buffer-mode)
(add-hook 'org-mode-hook 'turn-on-stripe-table-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ==== Define Function ====
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; Function Set for Edit
;;; for more functions,  see .emacs.d/conf/defuns.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'word-at-point "thingatpt" nil t)

;; change the background to yellow when if open a read-only file
;; (add-hook 'find-file-hooks
;;     (lambda ()
;;       (when buffer-read-only
;;         (set-background-color "yellow"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ==== Gloabal Key Binding ====
(load "keybindings")

;; Guru mode disables some common keybindings and
;; suggests the use of the established Emacs alternatives instead
; (add-site-lisp-load-path "guru-mode/")
; (require 'guru-mode)
; (guru-global-mode +1)
; (setq guru-warn-only t)

;; (global-set-key (kbd "C-c t") 'my-delete-trailing-whitespaces-and-untabify)

;; [ guide-key ]-----------------------------------------------------------------
;; displays the available key bindings automatically and dynamically
(add-site-lisp-load-path "guide-key/")
(add-site-lisp-load-path "guide-key-tip/")
(load "guide-key-conf")
;; ---------------------------------------------------------------------[ End ]--

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
(load "diminish-conf")
;; [ diminish ]--------------------------------------------------------[ End ]--


(load custom-file 'noerror)

; (sml/apply-theme 'dark)  ;; respectful/light
; (sml/setup)

;; (setq debug-on-error nil)
(message ">>>>> Emacs startup time: %d seconds."
         (time-to-seconds (time-since emacs-load-start-time)))

(message "***** >>>>> [ Loading my Emacs Init File Completed!! ] <<<<< *****")
