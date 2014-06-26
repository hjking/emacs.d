;;
;; Filename: find-file-conf.el
;; Description: Setting for find-file
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2010-12-16 14:39:48

(message "%d: >>>>> Loading [ Find File ] Setup ...." step_no)
(setq step_no (1+ step_no))

;;
;; Find File at Point
;;
(require 'ffap)
(ffap-bindings)
;;  (setq ffap-c-path (append ffap-c-path system-head-file-dir user-head-file-dir))
;; function called to fetch an URL. could be `browse-url-emacs or w3m-browse-url
(setq ffap-url-fetcher 'browse-url)
(setq ffap-require-prefix t)
;; recognize Win path
(setq ffap-string-at-point-mode-alist
      '((file "--:\\\\$+<>@-Z_a-z~*?" "<@" "@>;.,!:")
        (url "--:=&?$+@-Z_a-z~#,%;*" "^A-Za-z0-9" ":;.,!?")
        (nocolon "--9$+<>@-Z_a-z~" "<@" "@>;.,!?")
        (machine "-a-zA-Z0-9." "" ".")
        (math-mode ",-:$+<>@-Z_a-z~`" "<" "@>;.,!?`:")))
;; visit a file
;;  (global-set-key (kbd "<f3>") 'find-file-at-point)))

;;
;; Find File in Project
;;
(add-site-lisp-load-path "find-file-in-project/")
(require 'find-file-in-project)

(defun my-ffip-project-root-function ()
 "Check for `ffip-project-file' and if no such, \
  return current directory."
 (let ((project-directory
        (if (listp ffip-project-file)
            (some (apply-partially 'locate-dominating-file
                                   default-directory)
                  ffip-project-file)
          (locate-dominating-file default-directory
                                  ffip-project-file))))
   (or project-directory default-directory)))

(setq-default
  ffip-project-root-function 'my-ffip-project-root-function
  ffip-find-options "-not -regex \".*\\(debug\\|release\\|svn\\|git\\).*\""
  ffip-limit 4096
  ffip-patterns (nconc '("*.cpp" "*.h" "*.hpp" "*.c") ffip-patterns))

;;
;; fiplr
;;
(add-site-lisp-load-path "fiplr/")
(add-site-lisp-load-path "grizzl/")
(require 'fiplr)
(setq fiplr-ignored-globs '((directories (".git" ".svn"))
                            (files ("*.jpg" "*.png" "*.zip" "*~"))))
(global-set-key (kbd "C-x f") 'fiplr-find-file)

;; Projectile
; (eval-after-load "projectile"
;   '(progn
    (require 'projectile)
    (projectile-global-mode)
    (setq projectile-enable-caching t)  ;; enable caching
    ;; restore the recent window configuration of the target project
    (setq projectile-remember-window-configs t)
    ;; Action after running projectile-switch-project (C-c p s)
    ;; default: projectile-find-file
    ;; once selected the project, the top-level directory of the project is opened in a dired buffer
    (setq projectile-switch-project-action 'projectile-dired)
    ;; projectile-find-dir : remain in Projectile's completion system to select a sub-directory of your project,
    ;; and then that sub-directory is opened for you in a dired buffer
    ;; probably also want to set
    ;; (setq projectile-find-dir-includes-top-level t)
    ; (defconst projectile-mode-line-lighter " P")
    ;; (diminish 'projectile-mode)

    ;; integrate perspective with projectile
    (require 'perspective)
    (persp-mode)
    (require 'persp-projectile)
    ; ))