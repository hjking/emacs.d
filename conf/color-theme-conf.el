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
(color-theme-initialize)
;; (setq color-theme-is-global t)
;; (ignore-errors (color-theme-initialize))
(setq color-theme-solarized-path (concat my-site-lisp-dir "color-theme-solarized/"))
(add-to-list 'load-path color-theme-solarized-path)
(require 'color-theme-solarized)

(setq color-theme-monokai-path (concat my-site-lisp-dir "color-theme-monokai/"))
(add-to-list 'load-path color-theme-monokai-path)

(setq color-theme-twilight-path (concat my-site-lisp-dir "color-theme-twilight/"))
(add-to-list 'load-path color-theme-twilight-path)
(load-file (concat color-theme-twilight-path "color-theme-twilight.el"))

(setq color-theme-zenburn-path (concat my-site-lisp-dir "color-theme-zenburn/"))
(add-to-list 'load-path color-theme-zenburn-path)
(add-to-list 'custom-theme-load-path color-theme-zenburn-path)

(setq color-theme-mine-path (concat my-site-lisp-dir "color-theme-mine/"))
(add-to-list 'load-path color-theme-mine-path)
(require 'gentooish)
(require 'hjking-color)
(require 'color-theme-almost-monokai)
;; (require 'color-theme-djui-light)
;; (require 'color-theme-djui-dark)
(require 'color-theme-drr)
(require 'color-theme-wombat)
(setq color-theme-dawn-night-path (concat my-site-lisp-dir "color-theme-dawn-night/"))
(add-to-list 'load-path color-theme-dawn-night-path)
(require 'color-theme-dawn-night)

;;(color-theme-gentooish)
;;(color-theme-beauty-black)
;;(inspiration-630157)
;;(color-theme-arjen)
;;(color-theme-charcoal-black)
;;(color-theme-dark-blue2)
;;(color-theme-dark-laptop)
;;(color-theme-jsc-dark)
;;(color-theme-ld-dark)
;;(color-theme-ramangalahy)
;;(color-theme-vim-colors)
;;(color-theme-jedit-grey)
;;(color-theme-kingsajz)
;;(load-theme 'zenburn t)
(color-theme-solarized-dark)
;; (cl-night)
;; (require 'color-theme-random)
;; (color-theme-random)

(message ">>>>> [ Color Theme ] Setup Loaded")

