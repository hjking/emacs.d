
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
;; Reference:       Emacs document
;; Keywords:        emacs, dotfile, config
;; Copyright:       (C) 2010, Hong Jin
;; License:         This program is free software: you can redistribute it and/or modify
;;                  it under the terms of the GNU General Public License as published by
;;                  the Free Software Foundation, either version 3 of the License, or
;;                  (at your option) any later version.
;;
;;                  This program is distributed in the hope that it will be useful,
;;                  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;                  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;                  GNU General Public License for more details.
;;
;;                  You should have received a copy of the GNU General Public License
;;                  along with this program. If not, see <http://www.gnu.org/licenses/>.
;; Revision History:
;; +------------+------------+----------------------------------------+
;; |  Date      |  Revision  |  Description                           |
;; +------------+------------+----------------------------------------+
;; | 2010-04-22 |  0.9.0     | Initial version                        |
;; | 2010-04-23 |  0.9.1     | Add some key bindings                  |
;; | 2010-04-25 |  0.9.2     | Add some plugins                       |
;; | 2010-04-30 |  1.0.0     | First version                          |
;; | 2010-04-30 |  1.1.0     | Change structure of plugin config      |
;; | 2010-04-30 |  1.1.1     | Little change about structure          |
;; | 2010-05-03 |  1.2.0     | Second version                         |
;; | 2010-05-14 |  1.2.1     | Add clean-up-buffer-or-region function |
;; | 2010-05-17 |  1.2.2     | Add delete-current-file function       |
;; | 2010-05-21 |  1.2.3     | Change spell mode to off               |
;; | 2010-05-31 |  1.2.4     | Add isearch-forward-at-point           |
;; | 2010-06-01 |  1.2.5     | Add ivan-etags-bookmark                |
;; | 2010-10-23 |  1.2.6     | Add auto-header                        |
;; | 2010-10-23 |  1.2.7     | Delete auto-header; Add autoinsert     |
;; | 2010-12-13 |  1.3.0     | Restructure the init file              |
;; | 2011-09-26 |  1.4.0     | Restructure the init file              |
;; | 2011-12-07 |  1.5.0     | Restructure the init file              |
;; |            |            | Add some plugins                       |
;; | 2013-06-17 |  1.6.0     | Add use-package module                 |
;; +------------+------------+----------------------------------------+
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Let's Rock and Roll
;;
(message "")
(message "***** >>>>> [ Loading Emacs Startup File ], Be patient!")
(setq step_no 1)
;;;; Debugging
(setq
  eval-expression-debug-on-error t       ; debugger on errors in eval-expression
  stack-trace-on-error nil               ; backtrace of error on debug
  ;; debug-on-error t
  debug-on-error nil                     ; debugger on errors
  debug-on-quit nil                      ; debug when C-g is hit
  debug-on-signal nil)                   ; debug any/every error

;; uptimes
(setq emacs-load-start-time (current-time))

;; turn on Common Lisp support
(require 'cl)
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
(defvar section-registers nil)  ; no
(defvar section-display nil)  ; no
(defvar section-search nil)
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
(defvar section-etask nil)
(defvar section-eproject nil)
(defvar section-ecb nil)
(defvar section-sessions nil)
(defvar section-desktop nil)
(defvar section-muse nil)
(defvar section-pcvs nil)
(defvar section-psvn nil)
(defvar section-git nil)
(defvar section-emms t)
(defvar section-vm nil)
(defvar section-ac nil)
(defvar section-company t)
(defvar section-helm t)
(defvar section-icicles nil)
(defvar section-scratch t)
(defvar section-c-mode t)
(defvar section-cc-mode t)
(defvar section-elisp-mode t)
(defvar section-shell-mode t)
(defvar section-defuns t)
(defvar section-alias t)
(defvar section-vi nil)
(defvar section-artist nil)
(defvar section-yasnippet nil)
(defvar section-cygwin t)
(defvar section-elpa t)
(defvar section-tramp nil)
(defvar section-cedet t)
(defvar section-cedet-1.1 nil)
(defvar section-drag-stuff t)
(defvar section-epresent nil)
(defvar section-guess-style t)
(defvar section-less t)
(defvar section-mmm-mode t)
(defvar section-table nil)
(defvar section-undo t)
(defvar section-header t)
(defvar section-ergoemacs-keybindings nil)
(defvar section-irc nil)
(defvar section-w3m nil)
(defvar section-smex t)
(defvar section-slime t)
(defvar section-cscope t)
(defvar section-wl nil)
(defvar section-color-theme t)
(defvar section-weibo t)
(defvar section-workgroups t)
(defvar section-powerline t)

;;;###autoload
(defmacro define-kbd  (keymap key def) `(define-key ,keymap (kbd ,key) ,def))
;;;###autoload
(defmacro local-set-kbd  (key command)    `(local-set-key (kbd ,key) ,command))
;;;###autoload
(defmacro global-set-kbd (key command)    `(global-set-key (kbd ,key) ,command))

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
    (when linuxp
        (message "We are in Linux Platform")
        (setq my-home "/home/jinhong")
    )

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
  (defvar my-site-lisp-dir (concat my-emacs-dir "plugin/")
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
  (defmacro add-site-lisp-load-path (path)
      `(setq load-path (append (list (concat my-site-lisp-dir, path)) load-path)))
  (defmacro add-site-lisp-info-path (path)
      `(setq Info-default-directory-list (append (list (concat my-site-lisp-dir, path)) Info-default-directory-list)))
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

(when section-environment
  (load "env-conf")
)
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
      )
    )
  )
)
;; --------------------------------------------------------------------[ End ]--


;; use eval-after-load to speed up the startup
;; http://emacser.com/eval-after-load.htm
(require 'eval-after-load)


;; --[ Personal ]---------------------------------------------------------------
(message "%d: >>>>> Loading [ Personal Profile ] ...." step_no)
(setq step_no (1+ step_no))
;;
;; my TODO
;;todo_path: ~/.emacs.d/todo/
(setq my-todo-dir (concat my-personal-dir "todo/"))
(setq todo-file-do (concat my-todo-dir "do"))
(setq todo-file-done (concat my-todo-dir "done"))
(setq todo-file-top (concat my-todo-dir "top"))

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
(message "%d: >>>>> Loading [ Basic Customization ] ...." step_no)
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
(eval-after-load "faces" '(require 'faces+))

(when section-scratch
    (load "scratch-conf"))

;; use clipboard, share with other applications
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t)

(message "%d: >>>>> Loading [ Misc ] Customizations ...." step_no)
(setq step_no (1+ step_no))

;; make the help, apropos and completion windows the right height for their contents
;; (temp-buffer-resize-mode t)
(temp-buffer-resize-mode 1)

;; enable the use of the command `narrow-to-region' without confirmation
(put 'narrow-to-region 'disable nil)

;; enable the use of the commands `downcase-region' and `upcase-region'
;; without confirmation
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disable nil)

;; highlight current line
(require 'hl-line)
(global-hl-line-mode 1)

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

(message ">>>>> Loading [ Misc ] Customizations Done")

;; NEW @  2010-10-23-23:08
(add-hook 'message-mode-hook (lambda ()
    (setq fill-column 80)
    (turn-on-auto-fill)))

(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)

;;use proxy server
;;(setq url-proxy-services '(("http" . "proxy.km.local:8080")))

;; Don't break lines for me, please
(setq-default truncate-lines t)

(setq completion-ignored-extensions (remove ".pdf" completion-ignored-extensions))
(setq completion-ignored-extensions (remove ".dvi" completion-ignored-extensions))

;;; Library

;; dash
(add-site-lisp-load-path "dash/")
(eval-after-load "dash" '(dash-enable-font-lock))

;; sams-lib
(when (try-require 'sams-lib))

;; --[ Basic ]---------------------------------------------------------[ End ]--


;; --[ Frame Display ]----------------------------------------------------------
(when section-ui
    (load "ui-conf"))
;; --[ Frame Display ]-------------------------------------------------[ End ]--


;; --[ Register ]---------------------------------------------------------------
(when section-registers
;;; Better registers!
  (require 'better-registers)
  (better-registers-install-save-registers-hook)
  (load better-registers-save-file))
;; --[ Register ]------------------------------------------------------[ End ]--


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
  (message "%d: >>>>> Loading [ Minibuffer Customization ] ...." step_no)
  (setq step_no (1+ step_no))
  (setq read-file-name-completion-ignore-case t)
  ;; If non-`nil', resize the minibuffer so its entire contents are visible.
  (setq resize-minibuffer-mode t)
  ;; Enable recursive minibuffer
  (setq enable-recursive-minibuffers t)
  ;; auto-complete on in minibuffer
  (unless is-after-emacs-23
      partial-completion-mode 1)
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
  (am-add-hooks 'completion-setup-hook 'completion-faces)

)
;; --[ Minibuffer ]----------------------------------------------------[ End ]--


;; --[ mark and region ]--------------------------------------------------------
(when section-mark
    (message "%d: >>>>> Loading [ Mark and Region Customization ] ...." step_no)
    (setq step_no (1+ step_no))
    ;; highlight marked region
    (transient-mark-mode 1)    ; highlight text selection
    (delete-selection-mode 1) ; delete seleted text when typing
    ;; C-u C-SPC C-SPC ... cycles through the buffer local mark ring
    (setq set-mark-command-repeat-pop t)

)
;; --[ mark and region ]-----------------------------------------------[ End ]--


;; --[ killing ]----------------------------------------------------------------
(when section-killing
    (message "%d: >>>>> Loading [ Killing Customization ] ...." step_no)
    (setq step_no (1+ step_no))
    ;; use a bigger kill ring
    (setq kill-ring-max (* 20 kill-ring-max))
    ;; C-k delete a whole line
    (setq-default kill-whole-line t)
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
    (message "%d: >>>>> Loading [ Yanking Customization ] ...." step_no)
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


;; --[ Bookmark ]---------------------------------------------------------------
(message "%d: >>>>> Loading [ Bookmark Customization ] ...." step_no)
(setq step_no (1+ step_no))
(require 'bookmark)
;; set bookmark file: ~/.emacs.d/emacs_bookmarks
(setq bookmark-default-file (concat my-emacs-dir "emacs_bookmarks"))
;; each command that sets a bookmark will also save your bookmarks
(setq bookmark-save-flag t)
;; (switch-to-buffer "*Bookmark List*")
;; --[ Bookmark ]------------------------------------------------------[ End ]--


;; --[ Scrolling ]--------------------------------------------------------------
(message "%d: >>>>> Loading [ Scrolling Customization ] ...." step_no)
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
(setq hscroll-step 2)
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq redisplay-dont-pause t)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; don't add new lines when scrolling point
(setq next-line-add-newlines nil)
;; <pager> provides a better scrolling in Emacs

;; Keep cursor away from edges when scrolling up/down
(add-site-lisp-load-path "smooth-scrolling/")
(require 'smooth-scrolling)
;; --[ Scrolling ]-----------------------------------------------------[ End ]--


;; --[ Rectangles ]-------------------------------------------------------------
(when section-rectangles
    (message "%d: >>>>> Loading [ Rectangles Customization ] ...." step_no)
    (setq step_no (1+ step_no))
;; `kill-rectangle' (C-x r k) and `yank-rectangle' (C-x r y) can be very
;; useful for shifting cells up/down within a column while leaving remaining
;; columns intact.
)
;; --[ Rectangles ]----------------------------------------------------[ End ]--


;; --[ search and replace ]-----------------------------------------------------
(when section-search
    (load "search-conf")
)
;; --[ Search and Replace ]--------------------------------------------[ End ]--


;; --[ Font Lock ]--------------------------------------------------------------
(message "%d: >>>>> Loading [ Font Lock Customization ] ...." step_no)
(setq step_no (1+ step_no))
;; syntax highlight everywhere
;; (global-font-lock-mode t)
(if (fboundp 'global-font-lock-mode)
     (global-font-lock-mode 1)          ; GNU Emacs
     (setq font-lock-auto-fontify t))   ; XEmacs'))
(setq font-lock-maximum-decoration t)
;;(setq font-lock-global-modes '(not text-mode))
;;(setq font-lock-verbose t)
;;(setq font-lock-maximum-size '((t . 1048576) (vm-mode . 5250000)))

;; Get more highlight
(require 'generic-x)
;; --[ Font Lock ]-----------------------------------------------------[ End ]--


;; --[ FontLock Keywords]-------------------------------------------------------
;; special words
(setq keywords-critical-pattern
      "\\(BUGS\\|FIXME\\|TODO\\|todo\\|XXX\\|[Ee][Rr][Rr][Oo][Rr]\\|[Mm][Ii][Ss][Ss][Ii][Nn][Gg]\\|[Ii][Nn][Vv][Aa][Ll][Ii][Dd]\\|[Ff][Aa][Ii][Ll][Ee][Dd]\\|[Cc][Oo][Rr][Rr][Uu][Pp][Tt][Ee][Dd]\\)")
(make-face 'keywords-critical)
(GNUEmacs (set-face-attribute 'keywords-critical nil
                              :foreground "red" :background "yellow"
                              :weight 'bold))

(setq keywords-normal-pattern "\\([Ww][Aa][Rr][Nn][Ii][Nn][Gg]\\)")
(make-face 'keywords-normal)
(GNUEmacs (set-face-attribute 'keywords-normal nil
                              :foreground "magenta2" :background "yellow"))

;; set up highlighting of special words for proper selected major modes only
(dolist (mode '(fundamental-mode
                svn-log-view-mode
                text-mode
                c-mode
                java-mode
                verilog-mode
                vlog-mode
                python-mode
                cperl-mode
                html-mode-hook
                css-mode-hook
                emacs-lisp-mode))  ; no interference with Org-mode (which derives from text-mode)
  (font-lock-add-keywords mode
    `((,keywords-critical-pattern 1 'keywords-critical prepend)
      (,keywords-normal-pattern 1 'keywords-normal prepend))))

;; add fontification patterns (even in comments) to a selected major mode
;; *and* all major modes derived from it
(defun fontify-keywords ()
  (interactive)
;;;   (font-lock-mode -1)
;;;   (font-lock-mode 1)
  (font-lock-add-keywords nil
    `((,keywords-critical-pattern 1 'keywords-critical prepend)
      (,keywords-normal-pattern 1 'keywords-normal prepend))))
;; FIXME                        0                  t

;; set up highlighting of special words for selected major modes *and* all
;; major modes derived from them
(dolist (hook '(c++-mode-hook
                c-mode-hook
                change-log-mode-hook
                cperl-mode-hook
                css-mode-hook
                emacs-lisp-mode-hook
                html-mode-hook
                java-mode-hook
                latex-mode-hook
                lisp-mode-hook
                makefile-mode-hook
                message-mode-hook
                php-mode-hook
                python-mode-hook
                sh-mode-hook
                shell-mode-hook
                verilog-mode-hook
                ssh-config-mode-hook))
  (add-hook hook 'fontify-keywords))
;; --[ FontLock Keywords]----------------------------------------------[ End ]--


;; --[ Mode Line ]--------------------------------------------------------------
(load "mode-line")

;; [ powerline ]
(when section-powerline
    (add-site-lisp-load-path "powerline/")
    (load "powerline-conf"))
;; --[ Mode Line ]-----------------------------------------------------[ End ]--


;; --[ Cursor and Point ]-------------------------------------------------------
(message "%d: >>>>> Loading [ Cursor and Point Customization ] ...." step_no)
(setq step_no (1+ step_no))
;; move cursor when point is coming
(mouse-avoidance-mode 'animate)
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


;; --[ Emacs Init File ]--------------------------------------------------------
;;  reload-dotemacs
;;  my-open-dot-emacs
;;  my-autocompile-dotemacs
;; --[ Emacs Init File ]-----------------------------------------------[ End ]--


;; --[ Saving File ]------------------------------------------------------------
(message "%d: >>>>> Loading [ Saving File Customization ] ...." step_no)
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
;; --[ Saving File ]---------------------------------------------------[ End ]--


;; --[ Time Stamp ]-------------------------------------------------------------
(message "%d: >>>>> Loading [ Time Stamp Customization ] ...." step_no)
(setq step_no (1+ step_no))
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
;; when save file, save time-stamp
(add-hook 'write-file-hooks 'time-stamp)
;; --[ Time Stamp ]----------------------------------------------------[ End ]--


;; --[ Backup ]-----------------------------------------------------------------
(message "%d: >>>>> Loading [ Backup Customization ] ...." step_no)
(setq step_no (1+ step_no))
;; auto save interval:every 100 input event
(setq auto-save-interval 100)
;; auto save after 20 senconds idle time
(setq auto-save-timeout 20)
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
(message "%d: >>>>> Loading [ Compare File Customization ] ...." step_no)
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
(message "%d: >>>>> Loading [ Buffer Handling Customization ] ...." step_no)
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

(when (try-require 'saveplace)
  (message "%d: >>>>> Loading [ saveplace ] Customization ...." step_no)
  (setq step_no (1+ step_no))
  ;; automatically save place in each file
  (setq-default save-place t)
  ;; name of the file that records `save-place-alist' value
  (setq save-place-file (concat my-cache-dir "emacs.places"))
  ;; do not make backups of master save-place file
  (setq save-place-version-control "never")
)
;; --------------------------------------------------------------------[ End ]--


;; --[ Window ]-----------------------------------------------------------------
;; use "C-c <--" back to previous window layout
(when section-windows
    (load "window-conf")
)
;; --------------------------------------------------------------------[ End ]--


;; --[ Indentation ]------------------------------------------------------------
(when section-indentation
    (message "%d: >>>>> Loading [ Indentation Customization ] ...." step_no)
    (setq step_no (1+ step_no))
    ;; Tab width
    (setq default-tab-width 4)
    (setq tab-width 4)
    ;; Use spaces, not tabs
    (setq indent-tabs-mode nil)
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
)
;; --[ Indentation ]---------------------------------------------------[ End ]--


;; --[ Documentation ]----------------------------------------------------------
(message "%d: >>>>> Loading [ Documentation Customization ] ...." step_no)
(setq step_no (1+ step_no))
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
;; --[ Documentation ]-------------------------------------------------[ End ]--


;; --[ Compilation ]------------------------------------------------------------
(message "%d: >>>>> Loading [ Compilation Customization ] ...." step_no)
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
    (load "calendar-conf")
)
;; --[ Calendar ]------------------------------------------------------[ End ]--


;; --[ Printer ]----------------------------------------------------------------
(message "%d: >>>>> Loading [ Printer Customization ] ...." step_no)
(setq step_no (1+ step_no))
;; in case of press "print" of menu bar
(fset 'print-buffer 'ignore)
(setq lpr-command "")
(setq printer-name "")
;; --[ Printer ]-------------------------------------------------------[ End ]--


;; --[ Game ]-------------------------------------------------------------------
(message "%d: >>>>> Loading [ Game ] Customizations ...." step_no)
(setq step_no (1+ step_no))
;; get rid of the Games in the Tools menu
(define-key menu-bar-tools-menu [games] nil)
;; --[ Game ]----------------------------------------------------------[ End ]--


;; --[ Parentheses ]------------------------------------------------------------
(add-site-lisp-load-path "smartparens/")
(load "parens-conf")
;; --[ Parentheses ]---------------------------------------------------[ End ]--


;; --[ Highlight Hex ]----------------------------------------------------------
(defvar hexcolor-keywords
    '(("#[ABCDEFabcdef[:digit:]]\\{6\\}"
        (0 (put-text-property (match-beginning 0)
                              (match-end 0)
                              'face (list :background
                                          (match-string-no-properties 0)))))))

(defun hexcolor-add-to-font-lock ()
    (interactive)
    (font-lock-add-keywords nil hexcolor-keywords))

(defun add-to-hooks (action &rest hooks)
    (mapc #'(lambda (x)
              (add-hook x action)) hooks))

(add-to-hooks 'hexcolor-add-to-font-lock
              'css-mode-hook
              'php-mode-hook
              'html-mode-hook
              'shell-script-mode
              'shell-mode-hook
              'emacs-lisp-mode-hook
              'text-mode-hook
              'haskell-mode-hook)

;; --[ Highlight Hex ]-------------------------------------------------[ End ]--


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
(message "%d: >>>>> Loading [ TAGS ] Customizations ...." step_no)
(setq step_no (1+ step_no))
;;; ctags -e -R *.cpp *.h
;;; M-. : find-tag ;
;;; M-* : jump back ;
;;; M-x tags-search : regexp-search
;;; M-, : resume 'tags-search'
;;; M-x tags-apropos : list all tags in a tag file that match a regexp
;;; M-x list-tags : list all tags defined in a source file
;;; C-x 4 . tag : Find first definition of tag, but display it in another window (find-tag-other-window).
;;; C-x 5 . tag : Find first definition of tag, and create a new frame to select the buffer (find-tag-other-frame).
;; (setq path-to-ctags "/usr/bin/ctags")
(setq path-to-ctags "ctags")
;; set search dirs
(setq tags-table-list '("./TAGS" "../TAGS" "../.."))
(defun my-create-tags (dir-name)
    "Create tags file."
    (interactive "DDirectory: ")
    (shell-command
      (format "%s -f %s/TAGS -e -R %s" path-to-ctags dir-name dir-name)
    )
)
;; put etags information in bookmark
(defun ivan-etags-bookmark ()
  (bookmark-set tagname))
(bookmark-bmenu-list)
(add-hook 'find-tag-hook 'ivan-etags-bookmark)
;; [ TAG ]-------------------------------------------------------------[ End ]--


;; [ Auto-Fill ]----------------------------------------------------------------
(message "%d: >>>>> Loading [ Auto-Fill Mode ] Customizations ...." step_no)
(setq step_no (1+ step_no))
;; turn on word wrap
(auto-fill-mode 1)
(setq default-justification 'full)
(setq adaptive-fill-mode nil)
(setq default-fill-column 80)
;; --------------------------------------------------------------------[ End ]--


;; [ Dired ]--------------------------------------------------------------------
(when section-dired
  (add-site-lisp-load-path "dired/")
  (load "dired-conf"))
;; [ Dired ]-----------------------------------------------------------[ End ]--


;; [ ibuffer ]------------------------------------------------------------------
;; buffer switch
(when section-ibuffer
  (add-site-lisp-load-path "ibuffer-vc/")
  (load "ibuffer-conf"))
;; [ ibuffer ]---------------------------------------------------------[ End ]--


;; [ ido ]----------------------------------------------------------------------
;; start from Emacs 22
(when section-ido
    (add-site-lisp-load-path "ido-hacks/")
    (add-site-lisp-load-path "ido-ubiquitous/")
    (add-site-lisp-load-path "flx-ido/")
    (add-site-lisp-load-path "/")
    (load "ido-conf"))
;; [ ido ]-------------------------------------------------------------[ End ]--


;; [ Table ]--------------------------------------------------------------------
(when section-table
    (message "%d: >>>>> Loading [ table ] Customizations ...." step_no)
    (setq step_no (1+ step_no))
    (try-require 'table)
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


;; [ line-number ]--------------------------------------------------------------
;; display line number at left window
;;  (autoload 'wb-line-number-toggle "wb-line-number" nil t)

;; Enable linum-mode for all modes unless we think the buffer is special.
;; (add-hook 'change-major-mode-hook
;;           (lambda ()
;;             (unless (string-match "^\*.+\*$" (buffer-name))
;;               (linum-mode t))))
;; (setq linum-format "%3d ")
;; (global-set-key "\M-n" 'linum-mode)
;; [ line-number ]-----------------------------------------------------[ End ]--


;; [ multi-term ]---------------------------------------------------------------
;; available for Emacs 23
; (message "%d: >>>>> Loading [ multi-term ] Customizations ...." step_no)
; (load "multi-term-conf")
;; [ multi-term ]------------------------------------------------------[ End ]--


;; [ Version Control ]----------------------------------------------------------
(message "%d: >>>>> Loading [ Version Control ] Customizations ...." step_no)
(setq step_no (1+ step_no))

;; Don't show whitespace in diff, but show context
(setq vc-diff-switches '("-b" "-B" "-u"))

;; *** --- PCL-CVS
(when section-pcvs
  (when (try-require 'pcvs)
    (eval-after-load 'pcvs
      (load "pcvs-conf"))
))

;; *** --- Subversion
(when section-psvn
  (when (try-require 'psvn)
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

  (when linuxp
    (load "git-conf")
  ))

;; [ Version Control ]-------------------------------------------------[ End ]--


;; [ smart-compile ]------------------------------------------------------------
(load "smart-compile-conf")
;; [ smart-compile ]---------------------------------------------------[ End ]--


;; [ highlight-symbol ]---------------------------------------------------------
(add-site-lisp-load-path "highlight-symbol")
(message "%d: >>>>> Loading [ highlight-symbol ] Customizations ...." step_no)
(setq step_no (1+ step_no))
(require 'highlight-symbol)
(highlight-symbol-mode 1)
(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key [f3]           'highlight-symbol-next)
(global-set-key [(shift f3)]   'highlight-symbol-prev)
(global-set-key [(meta f3)]    'highlight-symbol-query-replace)
;;(global-set-key [(shift f3)]    'highlight-symbol-prev)
;; [ highlight-symbol ]------------------------------------------------[ End ]--


;; [ auto-header ]--------------------------------------------------------------
(when section-header
    ;; auto-insert
    ;; (load "auto-insert-conf")

    ;; header2
    (load "header2-conf")
)
;; [ auto-header ]----------------------------------------------------[ End ]---



;; [ goto change ]--------------------------------------------------------------
(message ">>>>> Loading [ goto-change ] Customizations ....")
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
    (add-site-lisp-load-path "helm/")
    (add-site-lisp-info-path "helm/doc/")
    (load "helm-conf")
)
;; [ helm ]-------------------------------------------------------------[ End ]--


;;;; ================ CategoryCompletion ================
;; [ icicles ]------------------------------------------------------------------
(when section-icicles
    (add-site-lisp-load-path "icicles/")
    (load "icicles-conf")
)
;; --------------------------------------------------------------------[ End ]--

;; [ auto-complete ]------------------------------------------------------------
;; available for Emacs 22/23
(when section-ac
    (setq auto-comp-load-path (concat my-site-lisp-dir "auto-complete-1.3.1/"))
    (add-site-lisp-load-path "auto-complete-1.3.1/")
    (add-site-lisp-info-path "auto-complete-1.3.1/doc/")
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
    (load "hippie-exp-conf")
)
;; --[ Abbrevs ]-------------------------------------------------------[ End ]--


;; [ yasnippet ]----------------------------------------------------------------
;; available for Emacs 22/23
(when section-yasnippet
    (add-site-lisp-load-path "yasnippet/")
    (add-site-lisp-info-path "yasnippet/doc/")
    (load "yasnippet-conf")
)
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

  (load "vm-conf")
)
;; [ VM ]--------------------------------------------------------------[ End ]--


;; [ Hide-Show ]----------------------------------------------------------------
(require 'hideshow nil t)
(when (featurep 'hideshow)
  (message "%d: >>>>> Loading [ hide-show ] Customizations ...." step_no)
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
;; [ Hide-Show ]-------------------------------------------------------[ End ]--


;; [ hide ]---------------------------------------------------------------------
;; hide region
(require 'hide-region)

;; hide lines
(require 'hide-lines)
;; --------------------------------------------------------------------[ End ]--


;; [ tabbar ]-------------------------------------------------------------------
(when (try-require 'tabbar)
    (tabbar-mode t)
)
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


;; [ frame-cmds ]---------------------------------------------------------------
;;  (require 'frame-cmds)
;; --------------------------------------------------------------------[ End ]--


;; [ zoom-frm ]-----------------------------------------------------------------
;;  (require 'zoom-frm)
;;  (global-set-key (if (boundp 'mouse-wheel-down-event) ; Emacs 22+
;;                      (vector (list 'control mouse-wheel-down-event))
;;                    [C-mouse-wheel])    ; Emacs 20, 21
;;                  'zoom-in
;;  )
;;  (when (boundp 'mouse-wheel-up-event) ; Emacs 22+
;;    (global-set-key (vector (list 'control mouse-wheel-up-event))
;;          'zoom-out
;;    )
;;  )
;;  (global-set-key [S-mouse-1]    'zoom-in)
;;  (global-set-key [C-S-mouse-1]  'zoom-out)
;;  ;; Get rid of `mouse-set-font' or `mouse-appearance-menu':
;;  (global-set-key [S-down-mouse-1] nil)
;; --------------------------------------------------------------------[ End ]--


;; [ org ]----------------------------------------------------------------------
(when section-org
  (add-site-lisp-load-path "org/lisp/")
  (add-site-lisp-load-path "org/contrib/lisp/")
  (add-site-lisp-info-path "org/doc/")
  (load "org-conf"))
;; [ org ]-------------------------------------------------------------[ End ]--


;; [ etask ]--------------------------------------------------------------------
(when section-etask
  (add-site-lisp-load-path "etask/")
  (load "etask-conf"))
;; [ etask ]-----------------------------------------------------------[ End ]--


;; [ eproject ]-----------------------------------------------------------------
(when section-eproject
  (load "eproject-conf"))
;; --------------------------------------------------------------------[ End ]--


;; [ selftest ]-----------------------------------------------------------------
;; (load "self-test-conf")
;; --------------------------------------------------------------------[ End ]--


;; [ ledger ]--------------------------------------------------------------------
;; personal finance
;; (when (locate-library "ledger")
;;   (message ">>>>> Loading [ ledger ] Customizations ....")
;;   (autoload 'ledger-mode "ledger" nil t))
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
    (load "eshell-conf")
)
;; --------------------------------------------------------------------[ End ]--


;; [ erc ]-----------------------------------------------------------------------
(when section-irc
    (load "erc-conf")
    (erc :server "irc.freenode.net" :port 6667 :nick "hjking")
)
;; --------------------------------------------------------------------[ End ]--


;; [ longlines ]-----------------------------------------------------------------
(when (load "longlines" t)
    (message ">>>>> Loading [ longlines ] Customizations ....")
    (setq longlines-show-hard-newlines t)
    (add-to-list 'auto-mode-alist '("\\.ll\\'" . longlines-mode))
    )
;; --------------------------------------------------------------------[ End ]--


;; [ tramp ]---------------------------------------------------------------------
(when section-tramp
    (load "tramp-conf"))
;; --------------------------------------------------------------------[ End ]---


;; [ rot13 ]---------------------------------------------------------------------
;; perform Caesar ciphers
(when (try-require 'rot13)
    (load "rot13-conf")
)
;; --------------------------------------------------------------------[ End ]--


;; [ doc-view ]-----------------------------------------------------------------
(when section-document-view
    (if is-after-emacs-23
        (load "doc-view-conf"))
)
;; --------------------------------------------------------------------[ End ]--


;; [ artist ]-------------------------------------------------------------------
(when section-artist
    (require 'artist-conf)
)
;; --------------------------------------------------------------------[ End ]--


;; [ muse ]---------------------------------------------------------------------
(when section-muse
    (add-site-lisp-load-path "muse/lisp/")
    (load "muse-conf")
)
;; --------------------------------------------------------------------[ End ]--


;; [ ecb ]----------------------------------------------------------------------
(when section-ecb
    (add-site-lisp-load-path "ecb-2.40/")
    (add-site-lisp-info-path "ecb-2.40/info-help/")
    (load "ecb-conf")
)
;; --------------------------------------------------------------------[ End ]--


;; [ drag-stuff ]---------------------------------------------------------------
(when section-drag-stuff
    (add-site-lisp-load-path "drag-stuff/")
    (require 'drag-stuff)
    (drag-stuff-mode t)
)
;; --------------------------------------------------------------------[ End ]--


;; [ eproject ]-----------------------------------------------------------------
(when section-epresent
    (add-site-lisp-load-path "epresent/")
    (require 'epresent)
    (require 'epresent-autoloads)
)
;; --------------------------------------------------------------------[ End ]--


;; [ show-help ]-----------------------------------------------------------------
(when (try-require 'show-help))
;; --------------------------------------------------------------------[ End ]--


;; [ guess-style ]--------------------------------------------------------------
(when section-guess-style
    (add-site-lisp-load-path "guess-style/")
    (require 'guess-style)
    (require 'guess-style-autoloads))
;; --------------------------------------------------------------------[ End ]--


;; [ less ]---------------------------------------------------------------------
(when section-less
    (add-site-lisp-load-path "less/")
    (require 'less)
    (require 'less-autoloads))
;; --------------------------------------------------------------------[ End ]--


;; [ mmm-mode ]-----------------------------------------------------------------
(when section-mmm-mode
    (add-site-lisp-load-path "mmm-mode/")
    (add-site-lisp-info-path "mmm-mode/")
    (load "mmm-mode-conf"))
;; --------------------------------------------------------------------[ End ]--


;; [ tooltip-help ]-------------------------------------------------------------
(when (try-require 'tooltip-help))
;; --------------------------------------------------------------------[ End ]--


;; [ folding ]------------------------------------------------------------------
(load "folding" 'nomessage 'noerror)
(folding-mode-add-find-file-hook)

(folding-add-to-marks-list 'ruby-mode "#{{{" "#}}}" nil t)
(folding-add-to-marks-list 'php-mode    "//{"  "//}"  nil t)
(folding-add-to-marks-list 'html-mode   "<!-- {{{ " "<!-- }}} -->" " -->" nil t)
(folding-add-to-marks-list 'verilog-mode "// {"  "// }"  nil t)
(folding-add-to-marks-list 'sh-mode "#{{{" "#}}}" nil t)
(folding-add-to-marks-list 'emacs-lisp-mode ";;{"  ";;}"  nil t)
(folding-add-to-marks-list 'c-mode "/* {{{ " "/* }}} */" " */" t)

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
         ;; Markdown
         ("\\.md$"                        . markdown-mode)
         ("\\.markdown$"                  . markdown-mode)
         ("\\.mkdn$"                      . markdown-mode)
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
         ("\\.*rc$"                        . conf-unix-mode)
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
     )
)


;; [ CSV Mode ]-----------------------------------------------------------------
(message "%d: >>>>> Loading [ CSV Mode ] Customizations ...." step_no)
(setq step_no (1+ step_no))
;; major mode for editing comma-separated value files
(require 'csv-mode nil t)
;; field separators: a list of *single-character* strings
(setq csv-separators '("," ";"))
;; --------------------------------------------------------------------[ End ]--


;; [ Text Mode ]----------------------------------------------------------------
(message "%d: >>>>> Loading [ Text Mode ] Customizations ...." step_no)
(setq step_no (1+ step_no))
;; default mode is Text Mode
(setq default-major-mode 'text-mode)
(defun my-textmode-startup ()
  (interactive)
;;  (filladapt-mode t)
;;  (flyspell-mode t)
  (setq tab-width 4)
)
(add-hook 'text-mode-hook 'my-textmode-startup)
; (add-hook 'text-mode-hook 'turn-on-auto-fill)
; (add-hook 'text-mode-hook 'visual-line-mode)
(remove-hook 'text-mode-hook #'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; (add-hook 'text-mode-hook 'longlines-mode)
;; --------------------------------------------------------------------[ End ]--


;; [ Debug Mode ]---------------------------------------------------------------
;;  (define-key debugger-mode-map "o" 'other-window)
;; --------------------------------------------------------------------[ End ]--


;; [ Occur Mode ]---------------------------------------------------------------
(add-hook 'occur-mode-hook (lambda () (setq truncate-lines t)))
;; [ Occur Mode ]------------------------------------------------------[ End ]--


;; [ View Mode ]----------------------------------------------------------------
(message "%d: >>>>> Loading [ View Mode ] Customizations ...." step_no)
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
(message ">>>>> Loading [ Shell Scripting Mode ] Customizations ....")
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
(message ">>>>> Loading [ Makefile Mode ] Customizations ....")
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
    (setq cc-mode-load-path (concat my-site-lisp-dir "cc-mode/"))
    (add-site-lisp-load-path "cc-mode/")
    (add-site-lisp-info-path "cc-mode/")
    (load "c-mode-conf"))
;; --------------------------------------------------------------------[ End ]--


;; [ Emacs Lisp Mode ]----------------------------------------------------------
(when section-elisp-mode
    (load "elisp-mode-conf"))

;; Edebug
(setq edebug-trace t)

;; Eldoc: provides minibuffer hints when working with Emacs Lisp
(autoload 'turn-on-eldoc-mode "eldoc" nil t)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
;; --------------------------------------------------------------------[ End ]--


;; [ Comint Mode ]--------------------------------------------------------------
(message ">>>>> Loading [ Comint Mode ] Customizations ....")
(setq comint-input-ignoredups t)
(setq comint-input-ring-size 64)
(setq comint-buffer-maximum-size (expt 2 16))
(add-hook 'comint-mode-hook
          '(lambda ()
             (setq comint-scroll-show-maximum-output t)))
;; --------------------------------------------------------------------[ End ]--

;;;; ================ ProgrammingModes End ================

;; [ ELPA ]---------------------------------------------------------------------
;; Packages managment
(when section-elpa
    (load "elpa-conf"))
;; --------------------------------------------------------------------[ End ]--


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

;; [ redo+ ]--------------------------------------------------------------------
;;  (message ">>>>> Loading [ redo+ ] Customizations ....")
;;  (when (try-require 'redo+)
;;  )
;; [ redo+ ]-----------------------------------------------------------[ End ]--


;; [ ace-jump ]-----------------------------------------------------------------
(setq my-ace-jump-mode-load-path (concat my-site-lisp-dir "ace-jump-mode/"))
(add-site-lisp-load-path "ace-jump-mode/")
(load "ace-jump-conf")
;; --------------------------------------------------------------------[ End ]--

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Coding setting
(when section-coding
    (load "encoding-conf"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; [ EMMS ]---------------------------------------------------------------------
;; Music player
(when section-emms
    (setq my-emms-load-path (concat my-site-lisp-dir "emms/"))
    (add-site-lisp-load-path "emms/")
    (add-site-lisp-info-path "emms/")
    (load "emms-conf"))
;; [ EMMS ]------------------------------------------------------------[ End ]--


;; --[ tree ]-------------------------------------------------------------------
(setq my-tree-path (concat my-site-lisp-dir "tree/"))
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
    (require 'smex)
    (smex-initialize)
    (setq smex-save-file (concat my-cache-dir ".smex-items"))
    (global-set-key (kbd "M-x") 'smex)
    (global-set-key (kbd "M-X") 'smex-major-mode-commands)
    ;; This is your old M-x.
    (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))
;; --------------------------------------------------------------------[ End ]--


;; --[ slime ]------------------------------------------------------------------
(when section-slime
    (setq my-slime-path (concat my-site-lisp-dir "slime/"))
    (add-site-lisp-load-path "slime/")
    (add-site-lisp-info-path "slime/doc")
    (load "slime-conf"))
;; --------------------------------------------------------------------[ End ]--


;; --[ xcscope ]----------------------------------------------------------------
(when section-cscope
    (add-site-lisp-load-path "xcscope/")
    (load "xcscope-conf")    )
;; --------------------------------------------------------------------[ End ]--


;; --[ wl ]---------------------------------------------------------------------
;; wanderlust
(when section-wl
    (add-site-lisp-load-path "wl/wl/")
    (add-site-lisp-info-path "wl/doc/")
    (require 'wl))
;; --------------------------------------------------------------------[ End ]--


;;  (require 'highlight-tail)
;;  (highlight-tail-mode)

;; --[ Color Theme ]------------------------------------------------------------
(when section-color-theme
    (load "color-theme-conf"))
;; --[ Color Theme ]---------------------------------------------------[ End ]--


;; [ weibo ]--------------------------------------------------------------------
(when section-weibo
    (add-site-lisp-load-path "weibo/")
    (load "weibo-conf"))
;; [ weibo ]-----------------------------------------------------------[ End ]--


;; [ workgroups2 ]--------------------------------------------------------------
(when section-workgroups
    (add-site-lisp-load-path "workgroups2/src/")
    (load "workgroups-conf"))
;; [ workgroups2 ]-----------------------------------------------------[ End ]--


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


;; [ diminish ]-----------------------------------------------------------------
;; diminish keeps the modeline tidy
(require 'diminish)
(diminish 'abbrev-mode "Abv")
(diminish 'undo-tree-mode "Undo")
(diminish 'pabbrev-mode "Pabv")
;;  (diminish 'wrap-region-mode)
;;  (diminish 'yas/minor-mode)
;; [ diminish ]--------------------------------------------------------[ End ]--


;; [ sr-speedbar ]--------------------------------------------------------------
(load "sr-speedbar-conf")
;; [ sr-speedbar ]-----------------------------------------------------[ End ]--


;; [ popwin ]-------------------------------------------------------------------
(add-site-lisp-load-path "popwin/")
(load "popwin-conf")
;; [ popwin ]-----------------------------------------------------------[ End ]--


;; --[ Font ]-------------------------------------------------------------------
(message "%d: >>>>> Setting [ Font ] Customizations ...." step_no)
(setq step_no (1+ step_no))
;; (set-default-font "clR8x14")
;; (set-default-font "-Misc-Fixed-Medium-R-Normal--12-100-75-75-C-60-ISO8859-1")
;; (set-default-font "Vera Sans Mono-14")
;; (set-default-font "-*-Monaco-normal-r-*-*-17-102-120-120-c-*-iso8859-1")
;; (set-default-font "Monospace-10")
;; (set-default-font "-adobe-courier-medium-r-normal--18-180-75-75-m-110-iso8859-1")
;; (set-default-font "-b&h-lucidatypewriter-bold-r-normal-sans-14-140-75-75-m-90-iso8859-1")
;; (set-default-font "-misc-fixed-medium-r-normal--15-140-75-75-c-90-iso8859-1")
;; --[ Font ]----------------------------------------------------------[ End ]--


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
  (load "info-look-conf"))
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
(when section-ergoemacs-keybindings
    (setq ergoemacs-keybindings-load-path (concat my-site-lisp-dir "ergoemacs-mode/"))
    (add-site-lisp-load-path "ergoemacs-mode/")
    (add-site-lisp-info-path "ergoemacs-mode/")

    ;; load ErgoEmacs keybinding
    (require 'ergoemacs-mode)

    ;; turn on minor mode ergoemacs-mode
    (setq ergoemacs-theme nil)  ;; Uses standard ergoemacs keyboard theme
    (setq ergoemacs-keyboard-layout "us") ;; Assumes QWERTY keyboard layout
    (ergoemacs-mode 1))
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
(when section-sessions
    (add-site-lisp-load-path "session/lisp/")
    (load "session-conf"))
;; --------------------------------------------------------------------[ End ]--


;; [ desktop ]------------------------------------------------------------------
(when section-desktop
    (load "desktop-conf"))
;; --------------------------------------------------------------------[ End ]--

(load custom-file 'noerror)


;; (setq debug-on-error nil)

(message "***** >>>>> [ Loading my Emacs Init File Finished ] <<<<< *****")
