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
;; loaded, you can use `with-eval-after-load'.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Let's Rock and Roll
;;

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(message "")
(message "***** >>>>> [ Loading Emacs Startup File ], Be patient!")
(setq step_no 1)

;; I don't use XEmacs.  This file does not work with XEmacs.
(when (featurep 'xemacs)
  (error "This .emacs file does not work with XEmacs."))

;; uptimes
(setq emacs-load-start-time (current-time))

;; Increase the garbage collection threshold to 200 MB to ease startup
(setq gc-cons-threshold (* 200 1024 1024))

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
(defvar section-search t)
(defvar section-ido nil)
(defvar section-ui t)
(defvar section-coding t)
(defvar section-dired t)
(defvar section-document-view nil)
(defvar section-gnus nil)
(defvar section-hdl t)
(defvar section-verilog t)
(defvar section-vlog nil)
(defvar section-vhdl nil)
(defvar section-emacs-server nil)
(defvar section-org t)
(defvar section-ecb nil)
(defvar section-session t)
(defvar section-desktop nil)
(defvar section-muse nil)
(defvar section-cvs nil)
(defvar section-svn nil)
(defvar section-git t)
(defvar section-emms t)
(defvar section-ac nil)
(defvar section-helm nil)
(defvar section-scratch t)
(defvar section-c-mode t)
(defvar section-elisp-mode t)
(defvar section-html-mode nil)
(defvar section-shell-mode t)
(defvar section-defuns t)
(defvar section-alias t)
(defvar section-vi nil)
(defvar section-artist nil)
(defvar section-cygwin t)
(defvar section-tramp nil)
(defvar section-cedet nil)
(defvar section-cedet-1.1 nil)
(defvar section-slime t)
(defvar section-cscope nil)
(defvar section-color-theme t)

(random t)

;;;###autoload
(defmacro define-kbd  (keymap key def) `(define-key ,keymap (kbd ,key) ,def))
;;;###autoload
(defmacro local-set-kbd  (key command)    `(local-set-key (kbd ,key) ,command))
;;;###autoload
(defmacro global-set-kbd (key command)    `(global-set-key (kbd ,key) ,command))

;; ;;* Customize
;; From https://github.com/abo-abo/oremacs/blob/github/init.el
(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set) 'set-default) ',variable ,value))

(when section-debugging
  ;; Debugging
  (message "%d: >>>>> Debugging On...." step_no)
  (setq step_no (1+ step_no))
  ;; Turn on debugging, it will be turned off at the end.
  (setq
        debug-on-error t
        debug-on-quit t
        eval-expression-debug-on-error t       ; debugger on errors in eval-expression
        stack-trace-on-error nil               ; backtrace of error on debug
        debug-on-signal nil)                   ; debug any/every error
)

;; --[ Load Path ]--------------------------------------------------------------
(when section-loading-libraries
  (message "%d: >>>>> Loading [ Path ] ...." step_no)
  (setq step_no (1+ step_no))

  ;; Set up the base configuration directory
  (defconst my-emacs-init-file (or load-file-name buffer-file-name))
  (defconst my-emacs-dir (file-name-directory my-emacs-init-file)
    "The Root directory of my .emacs.d")
  ;; Manually installed/maintained elisp directory
  (defconst my-site-lisp-dir (expand-file-name (concat my-emacs-dir "vendor/"))
    "This directory keeps manually installed/maintained Emacs Lisp packages")
  ;; My configuration files directory
  (defconst my-site-lisp-conf-dir (expand-file-name (concat my-emacs-dir "conf/"))
    "This directory keeps my Emacs Lisp for packages")
  ;; Personal configuration files
  (defconst my-personal-dir (expand-file-name (concat my-emacs-dir "personal/"))
    "This directory keeps my personal configuration")
  ;; Non-elisp scripts directory
  (defconst my-scripts-dir (expand-file-name (concat my-emacs-dir "scripts/"))
    "Directory for non-elisp scripts")
  ;; Directory for temporary file
  (defconst my-cache-dir (expand-file-name (concat my-emacs-dir "cache/"))
    "This directory keeps cache files")
  (unless (file-exists-p my-cache-dir)
      (make-directory my-cache-dir))
  (defmacro add-load-path (path)
      `(setq load-path (append (list, path) load-path)))
  (defun add-to-load-path (dir) (add-to-list 'load-path dir))
  (defmacro add-site-lisp-load-path (path)
      `(add-to-list 'load-path (concat my-site-lisp-dir, path)))
  (defmacro add-site-lisp-info-path (path)
      `(add-to-list 'Info-default-directory-list (concat my-site-lisp-dir, path)))
  ;; (add-load-path my-emacs-dir)
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

  ;;set the default file path
  (setq default-directory "~/")
)
;; --[ Load Path ]-----------------------------------------------------[ End ]--


;; --[ Environment ]------------------------------------------------------------

(require 'init-compat)

(when section-environment
  (require 'env-conf)

  (when win32p
    ; (setenv "PATH" (concat "D:/DEV/global/bin;" (getenv "PATH")))
    (add-to-list 'exec-path "D:/DEV/global/bin")
    (add-to-list 'exec-path "D:/Program Files/Gow")
  )
  (when linuxp
    (message "We are in Linux Platform")
    (setq my-home "/home/hongjin"))

  (when linuxp
    (add-to-list 'exec-path "~/bin"))
  )
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
        (require 'cygwin-conf)
      ))))
;; --------------------------------------------------------------------[ End ]--


;; turn on Common Lisp support
(require 'cl)  ; provides useful things like `loop' and `setf'

; (require 'hjking-mode)
(define-prefix-command 'hjking-mode-map)
(global-set-key (kbd "C-x m") 'hjking-mode-map)

;; use eval-after-load to speed up the startup
;; http://emacser.com/eval-after-load.htm
(require 'eval-after-load)

;; [ package ]------------------------------------------------------------------
;; Packages managment
(require 'package-conf)
;; --------------------------------------------------------------------[ End ]--


;; --[ Personal ]---------------------------------------------------------------
(message "%d: >>>>> Loading [ Personal Profile ] ...." step_no)
(setq step_no (1+ step_no))

;; personal variables
(setq personal-vars (concat my-personal-dir "personal.el"))
(when (file-exists-p personal-vars)
  (load personal-vars))

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
;; what to do with redefinitions during Advice de/activation
(setq ad-redefinition-action 'accept)

;; Load global settings
(require 'global-conf)

;; don't truncate the message log buffer when it becomes large
(setq message-log-max t)

;; Load face+ after loading face
(with-eval-after-load 'faces
  (require 'faces+ nil t))

(when section-scratch
    (require 'scratch-conf))

;; use clipboard, share with other applications
(when (window-system)
  (setq-default
        ;; I'm actually not sure what this does but it's recommended?
        ;; stops killing/yanking interacting with primary X11 selection
        x-select-enable-primary nil
        ;; makes killing/yanking interact with the clipboard
        x-select-enable-clipboard t
        ;; stops selection with a mouse being immediately injected to the kill ring
        mouse-drag-copy-region nil
        x-stretch-cursor t
        ;; Mouse yank commands yank at point instead of at click.
        ;; middle button for paste
        mouse-yank-at-point t
        ;; Active region should set primary X11 selection.
        select-active-regions t
        ;; Save clipboard strings into kill ring before replacing them
        save-interprogram-paste-before-kill t
        ))
(global-set-key [mouse-2] 'mouse-yank-primary)
;; Rebind to new clipboard functions when available.
(when (fboundp 'clipboard-kill-region)
  (global-set-key [remap kill-region] 'clipboard-kill-region))
(when (fboundp 'clipboard-kill-ring-save)
  (global-set-key [remap kill-ring-save] 'clipboard-kill-ring-save))
(when (fboundp 'clipboard-yank)
  (global-set-key [remap yank] 'clipboard-yank))

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
(temp-buffer-resize-mode 1)

;;; Enable disabled commands
;; Enable conversion of the selected region to lower case using `C-x C-l`
(put 'downcase-region  'disabled nil)   ; Let downcasing work
(put 'erase-buffer     'disabled nil)
(put 'eval-expression  'disabled nil)   ; Let ESC-ESC work
(put 'narrow-to-page   'disabled nil)   ; Let narrowing work
;; enable the use of the command `narrow-to-region' without confirmation
(put 'narrow-to-region 'disabled nil)   ; Let narrowing work
(put 'set-goal-column  'disabled nil)
;; Enable conversion of the selected region to upper case using `C-x C-u`
(put 'upcase-region    'disabled nil)   ; Let upcasing work

;; Do Not open new frame when WoMan
(setq woman-use-own-frame nil)
(setq woman-fill-column 90)

;; display images
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

;;; Library

;; dash
(require 'dash)
(with-eval-after-load 'dash
  (dash-enable-font-lock))

;; use-package
(eval-when-compile
  (require 'use-package)
  (setq use-package-verbose t))
(require 'diminish)
(require 'bind-key)                ;; if you use any :bind variant

;; hydra
(require 'hydra-conf)

;; general
(require 'general-conf)

;; [ guide-key ]-----------------------------------------------------------------
;; displays the available key bindings automatically and dynamically
; (require 'guide-key-conf)
(require 'which-key-conf)

;; --[ Basic ]---------------------------------------------------------[ End ]--


;; --[ Font ]-------------------------------------------------------------------
(require 'font-conf)
;; --[ Font ]----------------------------------------------------------[ End ]--


;; --[ Frame Display ]----------------------------------------------------------
(when section-ui
  (require 'ui-conf))
;; --[ Frame Display ]-------------------------------------------------[ End ]--


;; --[ Encoding ]---------------------------------------------------------------
;;;; Encoding setting
(when section-coding
  (require 'encoding-conf))
;; --[ Encoding ]------------------------------------------------------[ End ]--


;; --[ Bookmark ]---------------------------------------------------------------
(use-package bookmark
  :init
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
  (setq find-file-suppress-same-file-warnings t)
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
  (setq minibuffer-message-timeout 1)
  (minibuffer-depth-indicate-mode 1)
  ;; auto-complete on in minibuffer
  (unless is-after-emacs-23
      partial-completion-mode 1)
  ;; auto-complete in minibuffer when execute M-x functions and variables
  ; (icomplete-mode 1)
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

  (use-package miniedit
    :commands minibuffer-edit
    :init
     (miniedit-install))

)
;; --[ Minibuffer ]----------------------------------------------------[ End ]--


;; --[ mark and region ]--------------------------------------------------------
(when section-mark
  (message "%d: >>>>> Loading [ Mark and Region ] Customization ...." step_no)
  (setq step_no (1+ step_no))
  ;; highlight marked region
  ;; change buffer, or focus, disable the current buffer’s mark
  (transient-mark-mode t)    ; highlight text selection

  ;; delsel.el
  (delete-selection-mode 1) ; delete seleted text when typing
  ;; C-u C-SPC C-SPC ... cycles through the buffer local mark ring
  (setq set-mark-command-repeat-pop t)
  (setq select-active-region t)
  (setq delete-active-region 'kill)

  ;;; rect-mark.el
  (use-package rect-mark
    :bind (("C-x r C-/" . rm-set-mark)
           ("C-x r C-x" . rm-exchange-point-and-mark)
           ("C-x r C-w" . rm-kill-region)
           ("C-x r M-w" . rm-kill-ring-save))
    :commands (rm-set-mark
               rm-exchange-point-and-mark
               rm-kill-region
               rm-kill-ring-save)
  )

  ;; Expand Region
  ;; https://github.com/magnars/expand-region.el
  (use-package expand-region
    :bind ("C-=" . er/expand-region)
    :commands (er/expand-region)
    :config
    (progn
      (setq expand-region-contract-fast-key "|")
      (setq expand-region-reset-fast-key "<ESC><ESC>"))
    )
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
      (require 'cua-conf)))
;; --------------------------------------------------------------------[ End ]--


;; --[ Scrolling ]--------------------------------------------------------------
(message "%d: >>>>> Loading [ Scrolling ] Customization ...." step_no)
(setq step_no (1+ step_no))
;; scroll when point 2 lines far away from the bottom
(setq scroll-margin 3)
; Scroll just one line when hitting bottom of window
(setq scroll-conservatively 10000)
;; Keep point at its screen position if the scroll command moved it vertically
;; out of the window, e.g. when scrolling by full screens using C-v.
(setq scroll-preserve-screen-position t)
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
(use-package smooth-scrolling
  :init
  (setq smooth-scroll-margin 5
        scroll-conservatively 101
        scroll-preserve-screen-position t
        auto-window-vscroll nil)
)
;; --[ Scrolling ]-----------------------------------------------------[ End ]--


;; --[ search and replace ]-----------------------------------------------------
(when section-search
  ; (add-site-lisp-load-path "visual-regexp/")
  ;; displays current match and total matches.
  ; (add-site-lisp-load-path "anzu/")
  (require 'search-conf)
  )
;; --[ Search and Replace ]--------------------------------------------[ End ]--


;; --[ Mode Line ]--------------------------------------------------------------

;; mode-line-stats
;; Display CPU/Memory/Disk status on mode line
;; (add-site-lisp-load-path "mode-line-stats/")
;; (load "mode-line-stats-conf")

;; Show buffer size in mode-line
(size-indication-mode 1)
;; display time in your mode-line
(display-time-mode 1)
;; Enable or disable the display of the current line number in your mode-line
(line-number-mode 1)
;; Enable or disable the display of the current column number in your mode-line
(column-number-mode 1)
;; Enable or disable laptop battery information
; (display-battery-mode 1)

;; displays the current function name in the mode line
(use-package which-func
  :init
  (setq which-func-unknown "n/a")
  ;; Don't set `which-function-mode' to be enabled by default for all modes
  ;; Major modes needing this mode should do:
  ;;   (add-to-list 'which-func-mode 'MAJOR-MODE)
  (setq which-func-modes nil)
  ; :config (which-function-mode 1)
  )

;; use inactive face for mode-line in non-selected windows
(setq mode-line-in-non-selected-windows nil)

;; set mode-line-format
; (require 'mode-line-conf)

;; [ powerline ]
; (add-site-lisp-load-path "powerline/")
; (load "powerline-conf")

;; [ Smart Mode Line ]
(use-package smart-mode-line
  :load-path (lambda () (concat my-site-lisp-dir "smart-mode-line/"))
  :disabled t
  :init (progn
         (setq sml/position-percentage-format "%p")
         (setq sml/shorten-directory t)
         (setq sml/shorten-modes t)
         (setq sml/name-width 25)
         (setq sml/mode-width 'full)
         (setq sml/theme 'respectful)  ;; respectful/light/dark
         )
  :config (sml/setup)
)

; (require 'spaceline-config)
; (spaceline-spacemacs-theme)
(use-package spaceline
  :demand t
  :init
  (setq powerline-default-separator 'arrow-fade)
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
  (spaceline-helm-mode))

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
; (autoload 'diff-mode "diff-mode" "Diff major mode" t)
(use-package diff-mode
  :commands diff-mode)

;; do everything in one frame
(use-package ediff
  :commands (ediff)
  :init (progn
         ;; first we set some sane defaults
         (setq-default
          ediff-window-setup-function 'ediff-setup-windows-plain
          ;; emacs is evil and decrees that vertical shall henceforth be horizontal
          ediff-custom-diff-options "-u"
          ediff-merge-split-window-function 'split-window-horizontally)
         ; ediff-split-window-function 'split-window-horizontally
         (setq ediff-split-window-function (if (> (frame-width) 150)
                                          'split-window-horizontally
                                        'split-window-vertically))
         ))

;; --[ Compare File ]--------------------------------------------------[ End ]--


;; --[ Buffer Handling ]--------------------------------------------------------
(message "%d: >>>>> Loading [ Buffer Handling ] Customization ...." step_no)
(setq step_no (1+ step_no))

;; When multiple buffers are visible (like in a frame with 2 or more windows),
;; do not display an already visible buffer when switching to next/previous
;; buffers or after killing buffers.
(setq switch-to-visible-buffer nil)

;; Make meaningful names for buffers with the same name
;; When several buffers visit identically-named files,
;; Emacs must give the buffers distinct names. The usual method
;; for making buffer names unique adds ‘<2>’, ‘<3>’, etc. to the end
;; of the buffer names (all but one of them).
;; The forward naming method includes part of the file's directory
;; name at the beginning of the buffer name
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(use-package uniquify
  :init
    ;; if open a same name buffer, then forward to same name buffer
    (setq uniquify-buffer-name-style 'post-forward)
    (setq uniquify-separator ":")
    (setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
    (setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers
    (setq uniquify-non-file-buffer-names t))

;; use current buffer when read man
(setq Man-notify-method 'pushy)

; ;; better buffer switching
; (iswitchb-mode 1)
; ;; prevent certain buffers from showing up in the completion list
; (setq iswitchb-buffer-ignore '("^ " "*Buffer"))
; ;; prevent switching to another frame
; (setq iswitchb-default-method 'samewindow)
;; Quickly switch between buffer with tab-complete
;; See "ido"
;;
;; --[ Buffer Handling ]-----------------------------------------------[ End ]--


;; [ session ]------------------------------------------------------------------
;; remembers your location in a file when saving files
(require 'saveplace-conf)

;; History
(require 'savehist-conf)

;; Turn on recent file mode so that you can more easily switch to
;; recently edited files when you first start emacs
(require 'recentf-conf)
;; --------------------------------------------------------------------[ End ]--


;; --[ Window ]-----------------------------------------------------------------
;; use "C-c <--" back to previous window layout
(require 'window-conf)
;; --------------------------------------------------------------------[ End ]--


;; --[ Indentation ]------------------------------------------------------------
(require 'indent-conf)
;; --[ Indentation ]---------------------------------------------------[ End ]--


;; --[ Documentation ]----------------------------------------------------------
;; displays information in the minibuffer about the thing at point.
(message "%d: >>>>> Loading [ Documentation ] Customization ...." step_no)
(use-package eldoc
  :diminish eldoc-mode
  :commands turn-on-eldoc-mode
  :init (progn
    (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)))
;; --[ Documentation ]-------------------------------------------------[ End ]--


;; --[ Compilation ]------------------------------------------------------------
;; highlight and parse the whole compilation output as soon as it arrives
(setq compile-auto-highlight t)

;; Turn off trailing space notification
(add-hook 'compilation-mode-hook '(lambda () (setq show-trailing-whitespace nil)))

;; display compiler error message, check key bindings:
;; first-error / next-error / previous-error
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))

(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; smart-compile
(require 'smart-compile-conf)

;; --[ Compilation ]---------------------------------------------------[ End ]--


;; --[ Calendar ]---------------------------------------------------------------
(require 'calendar-conf)
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
(require 'parens-conf)
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
; (load "ctags-conf")

;; ggtags: Emacs frontend to GNU Global source code tagging system
; (add-hook 'c-mode-common-hook
;           (lambda ()
;             (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'cperl-mode 'python-mode)
;               (ggtags-mode 1))))
(setq tags-revert-without-query 1)
;; [ TAG ]-------------------------------------------------------------[ End ]--


;; [ Wrap Line ]----------------------------------------------------------------
(message "%d: >>>>> Loading [ Wrap Line ] Customization ...." step_no)
(setq step_no (1+ step_no))

;; Do `M-x toggle-truncate-lines` to jump in and out of truncation mode.
;; Don't break lines for me, please
(setq truncate-lines t)
; (bind-key "t" #'toggle-truncate-lines hjking-mode-map)

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

;; visual-line-mode
;; wrap a line right before the window edge
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
;; (global-visual-line-mode -1) ;; Disable wrapping lines at word boundaries
(global-visual-line-mode t)
;; move around lines based on how they are displayed, rather than the actual line
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
(setq-default fill-column 80)
; (setq whitespace-line-column 80)
;; --------------------------------------------------------------------[ End ]--


;; [ Dired ]--------------------------------------------------------------------
;; File Manager
(when section-dired
  (add-site-lisp-load-path "dired/")
  (add-site-lisp-load-path "dired/dired-hacks/")
  (require 'dired-conf)
  ;; file manager based on dired
  (require 'ranger-conf)
  ;; emacs tree plugin like NERD tree for Vim
  (require 'neotree-conf)
  )
;; [ Dired ]-----------------------------------------------------------[ End ]--


;; [ ibuffer ]------------------------------------------------------------------
;; buffer switch
(require 'ibuffer-conf)
;; [ ibuffer ]---------------------------------------------------------[ End ]--


;; [ ido ]----------------------------------------------------------------------
;; Replaced with helm
(when section-ido
  (add-site-lisp-load-path "ido-hacks/")
  (add-site-lisp-load-path "ido-ubiquitous/")
  (add-site-lisp-load-path "flx-ido/")
  (add-site-lisp-load-path "ido-vertical-mode/")
  (add-site-lisp-load-path "ido-at-point/")
  (require 'ido-conf)
  )
;; [ ido ]-------------------------------------------------------------[ End ]--


;; [ Table ]--------------------------------------------------------------------
(use-package table
  :commands table-insert
  :init
   (add-hook 'text-mode-hook 'table-recognize))
;; --------------------------------------------------------------------[ End ]--


;; [ htmlize ]------------------------------------------------------------------
(require 'htmlize-conf)
;; [ htmlize ]---------------------------------------------------------[ End ]--


;; [ kill-ring ]----------------------------------------------------------------
;; enhance kill ring function
(require 'kill-ring-conf)
;; [ kill-ring ]-------------------------------------------------------[ End ]--


;; [ multi-term ]---------------------------------------------------------------
;; available for Emacs 23
; (message "%d: >>>>> Loading [ multi-term ] Customization ...." step_no)
; (require "multi-term-conf")
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

;; *** --- Change Log
;; don't make a new entry, when the last entry was made by you and on the same date
(setq add-log-always-start-new-record nil)
;; adds the file's version number to the change log entry
(setq change-log-version-info-enabled t)


;; *** --- PCL-CVS
(load "pcvs-conf")


;; *** --- Subversion
(when section-svn
  (when (require 'psvn nil t)
      (load "psvn-conf")
))


;; *** --- Git
(when section-git
  (add-site-lisp-load-path "git-modes/")

  ;; *** --- magit
  (add-site-lisp-load-path "magit/")
  (add-site-lisp-info-path "magit/")

  (autoload 'magit-status "magit" nil t)
  (autoload 'magit-show "magit" "" t nil)
  (autoload 'magit-show-commit "magit" "" t nil)
  (global-set-key (kbd "M-g s") 'magit-status)
  (global-set-key (kbd "M-g c") 'magit-cheat-sheet)
  ; (global-set-key (kbd "C-x g") 'magit-status)

  (with-eval-after-load 'magit
    (require 'git-conf))
  )

;; [ Version Control ]-------------------------------------------------[ End ]--


;; [ highlight ]----------------------------------------------------------------
(require 'highlight-conf)
;; --------------------------------------------------------------------[ End ]--


;; [ column-marker ]------------------------------------------------------------
;; highlight columns 75, 80, 100 in some modes
(require 'column-marker-conf)
;; [ column-marker ]---------------------------------------------------[ End ]--


;; [ auto-header ]--------------------------------------------------------------
(require 'header2-conf)
;; [ auto-header ]----------------------------------------------------[ End ]---


;; [ goto change ]--------------------------------------------------------------
;; Move point through buffer-undo-list positions
(use-package goto-chg
  :commands (goto-last-change))
;; [ goto change ]-----------------------------------------------------[ End ]--


;; [ helm ]---------------------------------------------------------------------
;; available for Emacs 22/23
; (when section-helm
;   ;; helm is new version of anything
;   ; (add-site-lisp-load-path "emacs-helm/")
;   ; (add-site-lisp-info-path "emacs-helm/doc/")
;   ;; helm-swoop
;   (add-site-lisp-load-path "helm-swoop/")
;   (require 'helm-conf)
;   )
;; [ helm ]-------------------------------------------------------------[ End ]--


;; [ ivy ]---------------------------------------------------------------------
;; replace a lot of ido or helms
(require 'ivy-conf)
;; [ ivy ]-------------------------------------------------------------[ End ]--


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
(require 'company-conf)
;; --------------------------------------------------------------------[ End ]--


;; --[ Abbrevs ]----------------------------------------------------------------
(require 'abbrevs-conf)
;; "hippie expand" for text autocompletion
(require 'hippie-exp-conf)
;; --[ Abbrevs ]-------------------------------------------------------[ End ]--


;; [ yasnippet ]----------------------------------------------------------------
;; available for Emacs 22/23
(require 'yasnippet-conf)
;; [ yasnippet ]-------------------------------------------------------[ End ]--


;; [ Find File ]----------------------------------------------------------------
;; find file or URL at point
;; Find file in Project
;; fiplr, find-file-in-project, ffap
; (require 'find-file-conf)

;; projectile
(require 'projectile-conf)
;; --------------------------------------------------------------------[ End ]--


;; [ Folding ]------------------------------------------------------------------
(require 'folding-conf)
;; --------------------------------------------------------------------[ End ]--


;; [ tabbar ]-------------------------------------------------------------------
(require 'tabbar-conf)
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
  (add-site-lisp-load-path "org-journal/")

  (require 'org-conf)
  (require 'org-trello-conf)
  )
;; [ org ]-------------------------------------------------------------[ End ]--


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
(require 'eshell-conf)
;; --------------------------------------------------------------------[ End ]--


;; [ erc ]-----------------------------------------------------------------------
(require 'erc-conf)
;; --------------------------------------------------------------------[ End ]--


;; [ tramp ]---------------------------------------------------------------------
(when section-tramp
    (require 'tramp-conf))
;; --------------------------------------------------------------------[ End ]---


;; [ doc-view ]-----------------------------------------------------------------
(when section-document-view
    (if is-after-emacs-23
        (load "doc-view-conf")))
;; --------------------------------------------------------------------[ End ]--


;; [ muse ]---------------------------------------------------------------------
(when section-muse
    (add-site-lisp-load-path "muse/lisp/")
    (load "muse-conf"))
;; --------------------------------------------------------------------[ End ]--


;; [ ecb ]----------------------------------------------------------------------
(when section-ecb
  ; (add-site-lisp-load-path "ecb/")
  (require 'ecb-conf))
;; --------------------------------------------------------------------[ End ]--


;; [ drag-stuff ]---------------------------------------------------------------
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
; (add-site-lisp-load-path "drag-stuff/")
(use-package drag-stuff
  :diminish ""
  :load-path (lambda () (concat my-site-lisp-dir "drag-stuff/"))
  :config
  (progn
    (drag-stuff-mode t)
    (add-to-list 'drag-stuff-except-modes 'org-mode)))
;; --------------------------------------------------------------------[ End ]--


;; [ mmm-mode ]-----------------------------------------------------------------
;; Multiple Major Modes coexist in one buffer
(use-package mmm-mode
  :load-path (lambda () (concat my-site-lisp-dir "mmm-mode/"))
  :disabled t
  :init
   (setq mmm-global-mode 'maybe)
  :config
   (add-site-lisp-info-path "mmm-mode/")
)
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
         ("\\.cc\\'"                      . c++-mode)
         ("\\.h\\'"                       . c++-mode)
         ;; java
         ("\\.java\\'"                    . java-mode)
         ;; python-mode
         ("\\.py\\'"                      . python-mode)
         ;; cperl-mode
         ("\\.PL$"                        . cperl-mode)
         ("\\.pl$"                        . cperl-mode)
         ("\\.perl$"                      . cperl-mode)
         ("\\.pm$"                        . cperl-mode)
         ("\\.t$"                         . cperl-mode)
         ("\\.psgi$"                      . cperl-mode)
         ("\\.comp$"                      . cperl-mode)
         ("\\.pl\\'"                      . cperl-mode)
         ("\\.perl\\'"                    . cperl-mode)
         ("\\.\\([pP][Llm]\\|al\\)\\'"    . cperl-mode)
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
         ("\\.rb$"                        . ruby-mode)
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
(use-package csv-mode
  :mode ("\\.[Cc][Ss][Vv]\\'")
  :init (progn
         ;; field separators: a list of *single-character* strings
         (setq csv-separators '("," ";" "|" " ")))
)
;; --------------------------------------------------------------------[ End ]--


;; [ Text Mode ]----------------------------------------------------------------
;; default mode is Text Mode
(setq-default major-mode 'text-mode)
(defun my-textmode-startup ()
  (interactive)
  ; (whitespace-mode 1)
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
      ; (setq my-verilog-load-path (concat my-site-lisp-dir "verilog-mode/"))
      ; (add-site-lisp-load-path "verilog-mode/")
      ;; load verilog mode only when needed
      ; (add-to-list 'auto-mode-alist '("\\.[ds]?\\(v\\|vp\\)\\'" . verilog-mode))
      ; (autoload 'verilog-mode "verilog-mode" "Verilog mode" t)
      (require 'verilog-conf)
      )

  (when section-vlog
      ;; Vlog mode: The verilog code maker
      ; (setq my-vlog-load-path (concat my-site-lisp-dir "vlog-mode/"))
      ; (add-site-lisp-load-path "vlog-mode/")
      (require 'vlog-conf))

  (when section-vhdl
      ;; VHDL mode
      (setq my-vhdl-load-path (concat my-site-lisp-dir "vhdl-mode/"))
      (add-site-lisp-load-path "vhdl-mode/")
      (add-site-lisp-info-path "vhdl-mode/")
      (require 'vhdl-conf))

  ;; Systemc mode
  (autoload 'systemc-mode "systemc-mode" "Mode for SystemC files." t)
  ;; (add-hook 'systemc-mode-hook 'c++-mode-hook)

)
;; --------------------------------------------------------------------[ End ]--


;; [ Python Mode ]--------------------------------------------------------------
(require 'python-conf)
;; --------------------------------------------------------------------[ End ]--


;; [ Perl Mode ]----------------------------------------------------------------
;; cperl-mode is preferred to perl-mode,
;; replace the standard perl-mode with cperl-mode
(require 'perl-conf)
;; --------------------------------------------------------------------[ End ]--


;; [ Shell Mode ]---------------------------------------------------------------
;; invoke a shell
; (when section-shell-mode
;     (when linuxp
;         (setq shell-file-name "/bin/bash"))
;     ; (load "shell-mode-conf")
;     (with-eval-after-load 'shell
;       (require 'shell-mode-conf))
;     )
;; --------------------------------------------------------------------[ End ]--


;; [ Shell script Mode ]--------------------------------------------------------
(load "sh-mode-conf")
;; --------------------------------------------------------------------[ End ]--


;; [ Makefile Mode ]------------------------------------------------------------
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
    (load "c-mode-conf")
    )
;; --------------------------------------------------------------------[ End ]--


;; [ Markdown Mode ]------------------------------------------------------------
;; Markdown mode - TAB for <pre></pre> block
(require 'markdown-mode-conf)
;; --------------------------------------------------------------------[ End ]--


;; [ Emacs Lisp Mode ]----------------------------------------------------------
(require 'elisp-mode-conf)

;; Edebug
(setq edebug-trace t)
;; --------------------------------------------------------------------[ End ]--


;; --[ HTML Mode ]--------------------------------------------------------------
(require 'web-mode-conf)
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


;; plantuml-mode
; (require 'plantuml-mode)
; (setq plantuml-jar-path (expand-file-name (concat my-scripts-dir "/plantuml.jar")))


;; Graphviz dot mode for emacs
(use-package graphviz-dot-mode)

;;;; ================ ProgrammingModes End ================

;; [ undo ]---------------------------------------------------------------------
(use-package undo-tree
  :load-path (lambda () (concat my-site-lisp-dir "undo-tree/"))
  :diminish ""
  :commands (redo undo)
  :config (progn
           (setq undo-tree-visualizer-timestamps t)
           (setq undo-tree-visualizer-diff t)
           (global-undo-tree-mode t)
           (defalias 'redo 'undo-tree-redo)
           (define-key undo-tree-map (kbd "C-x u") 'undo-tree-visualize)
           (define-key undo-tree-map (kbd "C-/") 'undo-tree-undo))
  :bind (("M-z" . undo)
         ("M-S-z" . redo)))
;; --------------------------------------------------------------------[ End ]--

;;;; Navigation
; (require 'ace-jump-conf)
;; [ avy ]----------------------------------------------------------------------
;; Jump to visible text using a char-based decision tree
(require 'avy-conf)
;; --------------------------------------------------------------------[ End ]--

;; [ EMMS ]---------------------------------------------------------------------
;; Music player
;; set the location of the music player in Win
(when section-emms
    (setq my-emms-load-path (concat my-site-lisp-dir "emms/"))
    (add-site-lisp-load-path "emms/lisp/")
    (add-site-lisp-info-path "emms/doc/")
    (load "emms-conf")
    )
;; [ EMMS ]------------------------------------------------------------[ End ]--


;; --[ epg ]--------------------------------------------------------------------
(load "epg-conf")
;; --------------------------------------------------------------------[ End ]--


;; --[ gdb ]--------------------------------------------------------------------
(load "gdb-conf")
;; --------------------------------------------------------------------[ End ]--


;; --[ w3m ]--------------------------------------------------------------------
;; Web browser
(require 'w3m-conf)
;; --------------------------------------------------------------------[ End ]--


;; --[ smex ]-------------------------------------------------------------------
;; Smart M-x
;; remember recently and most frequently used commands
(require 'smex-conf)
;; --------------------------------------------------------------------[ End ]--


;; --[ xcscope ]----------------------------------------------------------------
(when linuxp
  (when section-cscope
    (add-site-lisp-load-path "xcscope/")
    (load "xcscope-conf")))
;; --------------------------------------------------------------------[ End ]--


;; --[ WanderLust ]-------------------------------------------------------------
;; Wanderlust is a mail/news management system with IMAP4rev1 support for Emacs.
(use-package wl
  :load-path (lambda () (concat my-site-lisp-dir "wl/wl/"))
  :config
  (add-site-lisp-info-path "wl/doc/")
  )
;; --------------------------------------------------------------------[ End ]--


;; --[ Color Theme ]------------------------------------------------------------
(when section-color-theme
    (require 'color-theme-conf))
;; --[ Color Theme ]---------------------------------------------------[ End ]--


;; [ weibo ]--------------------------------------------------------------------
(require 'weibo-conf)
;; [ weibo ]-----------------------------------------------------------[ End ]--


;; [ multiple-cursors ]---------------------------------------------------------
;; https://github.com/magnars/multiple-cursors.el
; ;; Add a cursor to each line in an active region that spans multiple lines
; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
; ;; Add multiple cursors not based on continuous lines, but based on keywords in the buffer
; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(use-package multiple-cursors
  :load-path (lambda () (concat my-site-lisp-dir "multiple-cursors/"))
  :bind (("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)))
;; [ multiple-cursors ]------------------------------------------------[ End ]--


;; [ hungry-delete ]------------------------------------------------------------
(use-package hungry-delete
  :disabled t
  :load-path (lambda () (concat my-site-lisp-dir "hungry-delete/"))
  :config
    (global-hungry-delete-mode))
;; --------------------------------------------------------------------[ End ]--


;; [ sr-speedbar ]--------------------------------------------------------------
; (load "sr-speedbar-conf")
;; [ sr-speedbar ]-----------------------------------------------------[ End ]--


;; [ popwin ]-------------------------------------------------------------------
; (add-site-lisp-load-path "popwin/")
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
  (use-package clippy
    :load-path (lambda () (concat my-site-lisp-dir "clippy/")))
  ;; ------------------------------------------------------------------[ End ]--
)
;; --[ Help ]----------------------------------------------------------[ End ]--


;;; stripe-buffer
;;; Use different background colors for even and odd lines.
(use-package stripe-buffer
  :commands (turn-on-stripe-buffer-mode
             turn-on-stripe-table-mode)
  :init  (progn
          (add-hook 'dired-mode-hook 'turn-on-stripe-buffer-mode)
          (add-hook 'org-mode-hook 'turn-on-stripe-table-mode)
          )
  )

;; Whenever the window scrolls a light will shine
;; on top of your cursor so you know where it is.
(require 'beacon-conf)

;; Modernizing Emacs' Package Menu
;; Use `paradox-list-packages' instead of the regular `list-packages'
(require 'paradox-conf)

;; A Collection of Ridiculously Useful eXtensions for Emacs
(require 'crux-conf)

;; Weather
(require 'wttrin-conf)

;; deft
(require 'deft-conf)

;; comment/uncomment with M-;
(require 'evil-nerd-commenter-conf)

(use-package bongo
  :commands (bongo)
  :init
  (if (file-directory-p "D:/Tools/MPlayer")
      (add-to-list 'exec-path "D:/Tools/MPlayer")
    (message "*** Warning!! Please install MPlayer first!!"))
  )

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
(require 'workgroups-conf)
;; [ workgroups2 ]-----------------------------------------------------[ End ]--


;; [ diminish ]-----------------------------------------------------------------
;; diminish keeps the modeline tidy
(load "diminish-conf")
;; [ diminish ]--------------------------------------------------------[ End ]--


;; custom file: modified setting by menu bar
(if (not custom-file)
  (setq custom-file (concat my-personal-dir "my-custom.el")))
(load custom-file 'noerror)

; (sml/apply-theme 'dark)  ;; respectful/light
; (sml/setup)

(setq debug-on-error nil
      debug-on-quit nil
      stack-trace-on-error '(buffer-read-only))

;; Garbage collector - decrease threshold to 5 MB
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 5 1024 1024))))

(message ">>>>> Emacs startup time: %d seconds."
         (time-to-seconds (time-since emacs-load-start-time)))

(message "***** >>>>> [ Loading my Emacs Init File Completed!! ] <<<<< *****")
