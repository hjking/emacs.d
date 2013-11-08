
;; Semantic
;; (1)
;; (semantic-load-enable-minimum-features)
;; include semantic-idle-scheduler-mode,
;; semanticdb-minor-mode, semanticdb-load-ebrowse-caches

;; (2)
;; Enable prototype help and smart completion
;; (semantic-load-enable-code-helpers)
;; include semantic-load-enable-minimum-features, imenu,
;; semantic-idle-summary-mode, senator-minor-mode, semantic-mru-bookmark-mode

;; (3)
;; enables even more coding tools such as intellisense mode,
;; decoration mode, and stickyfunc mode
;; (semantic-load-enable-gaudy-code-helpers)
;; include following
;; 在类/函数等tag上方加一条蓝色的线
;; (global-semantic-decoration-mode 1)
;; 把当前函数名显示在buffer顶上
;; (global-semantic-stickyfunc-mode 1)
;; (semantic-idle-completions-mode 1)

;; (4)
;; enables which-func-mode, that shows name of current function in status line
(semantic-load-enable-excessive-code-helpers)
;; include semantic-highlight-func-mode, semantic-idle-tag-highlight-mode,
;; semantic-decoration-on-*-members, which-func-mode
;; 用灰的底色把光标所在函数名高亮显示
;; (global-semantic-highlight-func-mode 1)
;; 光标停留在一个变量上，整个函数内部用这个变量的地方都高亮了
;; (global-semantic-idle-tag-highlight-mode 1)

;; (5)
;; enables several modes, that are useful when you debugging Semantic
;; enable几个和调试semantic相关的特性
(if win32p
    (semantic-load-enable-semantic-debugging-helpers)
    ;; 把semantic解析不了的内容用红色下划线标识出来
    (progn (global-semantic-show-unmatched-syntax-mode 1)
    ;; 在modeline上显示出当前解析状态
           (global-semantic-show-parser-state-mode 1)))

(setq semantic-idle-scheduler-idle-time 3)
;; (global-semantic-mru-bookmark-mode 1)
(setq semantic-imenu-auto-rebuild-directory-indexes nil)

;; Senator
(setq senator-minor-mode-name "SN")

;; semantic-ia
;; names completion, and displaying of information for tags & classes
(require 'semantic-ia nil 'noerror)

;; semantic-gcc
;; using GCC for programming in C & C++, access to system include files
;; (require 'semantic-gcc)

;; semanticdb
(setq semanticdb-default-save-directory (concat my-cache-dir ".semanticdb"))

;; ;; folding
(require 'semantic-tag-folding nil 'noerror)
(global-semantic-tag-folding-mode 1)
;; (global-set-key (kbd "C-?") 'global-semantic-tag-folding-mode)
;; (define-key semantic-tag-folding-mode-map (kbd "C-c , -") 'semantic-tag-folding-fold-block)
;; (define-key semantic-tag-folding-mode-map (kbd "C-c , +") 'semantic-tag-folding-show-block)
;; ;; fold all
;; (define-key semantic-tag-folding-mode-map (kbd "C-_") 'semantic-tag-folding-fold-all)
;; (define-key semantic-tag-folding-mode-map (kbd "C-+") 'semantic-tag-folding-show-all)

;;    ;; CC-mode
;;    (add-hook 'c-mode-hook '(lambda ()
;;            (setq ac-sources (append '(ac-source-semantic) ac-sources))
;;            (local-set-key (kbd "RET") 'newline-and-indent)
;;            (linum-mode t)
;;            (semantic-mode t)))

(defconst cedet-user-include-dirs
  (list ".." "../include" "../inc" "../common" "../public"
        "../.." "../../include" "../../inc" "../../common" "../../public"))

;; ;; Enable SRecode
(global-srecode-minor-mode 1)            ; Enable template insertion menu

;; (defun my-c-mode-cedet-hook ()
;;     (local-set-key "." 'semantic-complete-self-insert)
;;     (local-set-key ">" 'semantic-complete-self-insert)
;;     (local-set-key "\C-ct" 'eassist-switch-h-cpp)
;;     (local-set-key "\C-xt" 'eassist-switch-h-cpp)
;;     (local-set-key "\C-ce" 'eassist-list-methods)
;;     (local-set-key "\C-c\C-r" 'semantic-symref)
;;     )
;; (add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)

;; 在C++的头文件和实现文件间跳转
(require 'eassist nil 'noerror)

;;  ;; gnu global support
;;  (when (cedet-gnu-global-version-check t)
;;    (require 'semanticdb-global)
;;    (semanticdb-enable-gnu-global-databases 'c-mode)
;;    (semanticdb-enable-gnu-global-databases 'c++-mode))
;;
;;  ;; ctags
;;  (when (cedet-ectag-version-check t)
;;    (require 'semanticdb-ectag)
;;    (semantic-load-enable-primary-exuberent-ctags-support))
;;
;;  ;;; EDE customization
;;  (ede-enable-generic-projects)

;;--------------------------------------------------------------------
;; After 1.1
; ;; select which submodes we want to activate
; (add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
; (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
; (add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
; (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
; (add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode)
; (add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
; (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)

; ;; Activate semantic
; (semantic-mode 1)

; ;; load contrib library
; (require 'eassist)

; ;; customisation of modes
; (defun alexott/cedet-hook ()
;   (local-set-key [(control return)] 'semantic-ia-complete-symbol-menu)
;   (local-set-key "\C-c?" 'semantic-ia-complete-symbol)
;   ;;
;   (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
;   (local-set-key "\C-c=" 'semantic-decoration-include-visit)

;   (local-set-key "\C-cj" 'semantic-ia-fast-jump)
;   (local-set-key "\C-cq" 'semantic-ia-show-doc)
;   (local-set-key "\C-cs" 'semantic-ia-show-summary)
;   (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
;   )
; (add-hook 'c-mode-common-hook 'alexott/cedet-hook)
; (add-hook 'lisp-mode-hook 'alexott/cedet-hook)
; (add-hook 'scheme-mode-hook 'alexott/cedet-hook)
; (add-hook 'emacs-lisp-mode-hook 'alexott/cedet-hook)
; (add-hook 'erlang-mode-hook 'alexott/cedet-hook)

; (defun alexott/c-mode-cedet-hook ()
;   (local-set-key "\C-ct" 'eassist-switch-h-cpp)
;   (local-set-key "\C-xt" 'eassist-switch-h-cpp)
;   (local-set-key "\C-ce" 'eassist-list-methods)
;   (local-set-key "\C-c\C-r" 'semantic-symref)
;   )
; (add-hook 'c-mode-common-hook 'alexott/c-mode-cedet-hook)

; (semanticdb-enable-gnu-global-databases 'c-mode t)
; (semanticdb-enable-gnu-global-databases 'c++-mode t)

; (when (cedet-ectag-version-check t)
;   (semantic-load-enable-primary-ectags-support))

; ;; SRecode
; (global-srecode-minor-mode 1)

; ;; EDE
; (global-ede-mode 1)
; (ede-enable-generic-projects)
