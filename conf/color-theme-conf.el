;;
;; Filename: color-theme-conf.el
;; Description: Setting for color theme
;; Author: Hong Jin
;; Created: 2012-02-09 10:00
;; Last Updated: 2012-11-22 11:15:14

(message "%d: >>>>> Loading [ Color Theme ] Setup ...." step_no)
(setq step_no (1+ step_no))
;; color
(setq color-theme-path (concat my-site-lisp-dir "color_theme/"))
(add-to-list 'load-path color-theme-path)
(require 'color-theme)
(with-eval-after-load 'color-theme
     (color-theme-initialize)
     ;; (setq color-theme-is-global t)
     ;; (ignore-errors (color-theme-initialize))

     (setq color-theme-solarized-path (concat my-site-lisp-dir "color-theme-solarized/"))
     (add-to-list 'load-path color-theme-solarized-path)
     (require 'color-theme-solarized)

     (setq color-theme-molokai-path (concat my-site-lisp-dir "color-theme-molokai/"))
     (add-to-list 'load-path color-theme-molokai-path)
     (add-to-list 'custom-theme-load-path color-theme-molokai-path)

     ; (setq color-theme-twilight-path (concat my-site-lisp-dir "color-theme-twilight/"))
     ; (add-to-list 'load-path color-theme-twilight-path)
     ; (load-file (concat color-theme-twilight-path "color-theme-twilight.el"))

     (setq color-theme-zenburn-path (concat my-site-lisp-dir "color-theme-zenburn/"))
     (add-to-list 'load-path color-theme-zenburn-path)
     (add-to-list 'custom-theme-load-path color-theme-zenburn-path)

     (setq color-theme-cyberpunk-path (concat my-site-lisp-dir "color-theme-cyberpunk/"))
     (add-to-list 'load-path color-theme-cyberpunk-path)
     (add-to-list 'custom-theme-load-path color-theme-cyberpunk-path)

     (setq color-theme-tangotango-path (concat my-site-lisp-dir "color-theme-tangotango/"))
     (add-to-list 'load-path color-theme-tangotango-path)
     (add-to-list 'custom-theme-load-path color-theme-tangotango-path)

     (setq color-theme-wombat-path (concat my-site-lisp-dir "color-theme-wombat/"))
     (add-to-list 'load-path color-theme-wombat-path)
     (add-to-list 'custom-theme-load-path color-theme-wombat-path)

     (setq color-theme-mine-path (concat my-site-lisp-dir "color-theme-mine/"))
     (add-to-list 'load-path color-theme-mine-path)
     (require 'gentooish)
     (require 'hjking-color)

     (require 'color-theme-drr)
     (require 'color-theme-wombat)
     (setq color-theme-dawn-night-path (concat my-site-lisp-dir "color-theme-dawn-night/"))
     (add-to-list 'load-path color-theme-dawn-night-path)
     (require 'color-theme-dawn-night)

     (setq color-theme-material-path (concat my-site-lisp-dir "color-theme-material/"))
     (add-to-list 'load-path color-theme-material-path)
     (add-to-list 'custom-theme-load-path color-theme-material-path)

     ;;customize theme
     ; (setq color-theme-moe-path (concat my-site-lisp-dir "color-theme-moe/"))
     ; (add-to-list 'custom-theme-load-path color-theme-moe-path)
     ; (add-to-list 'load-path color-theme-moe-path)
     ; (require 'moe-theme)
     ; (setq moe-theme-mode-line-color 'orange)
     ;   ;; (Available colors: blue, orange, green ,magenta, yellow, purple, red, cyan, w/b.)

     ;;;; pick a color theme
     ;; (color-theme-solarized-dark)
     ;; (load-theme 'wombat t)
     ;; (load-theme 'tangotango t)
     (load-theme 'cyberpunk t)
     ;; (load-theme 'zenburn t)
     ;; (load-theme 'molokai t)
     ;; (load-theme 'material t)
     ; (moe-dark)          ;; (moe-light) OR jsut `(load-theme 'moe-dark t)'
     (use-package monokai-theme
     :disabled t
     :init
        (setq monokai-use-variable-pitch nil
              monokai-height-minus-1 1.0
              monokai-height-plus-1 1.0
              monokai-height-plus-2 1.0
              monokai-height-plus-3 1.0
              monokai-height-plus-4 1.0)
     :config
        (load-theme 'monokai t))
)

(provide 'color-theme-conf)