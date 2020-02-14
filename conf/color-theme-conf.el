;; -*- lexical-binding: t -*-

;; Filename: color-theme-conf.el
;; Description: Setting for color theme
;; Author: Hong Jin
;; Created: 2012-02-09 10:00
;; Last Updated: 2012-11-22 11:15:14

(message "%d: >>>>> Loading [ Color Theme ] Setup ...." step_no)
(setq step_no (1+ step_no))

(use-package molokai-theme
  :disabled t
  :config
  (load-theme 'molokai t))

(use-package monokai-theme
  :disabled t
  :config
  (setq monokai-use-variable-pitch nil
        monokai-height-minus-1 1.0
        monokai-height-plus-1 1.0
        monokai-height-plus-2 1.0
        monokai-height-plus-3 1.0
        monokai-height-plus-4 1.0)
  (load-theme 'monokai t))

(use-package color-theme-sanityinc-tomorrow
  :disabled t
  :config
  (load-theme 'sanityinc-tomorrow-night t))

(use-package dracula-theme
  :ensure t
  :config
  (load-theme 'dracula t))

(use-package solarized-theme
  :defer t
  :disabled t
  :config
  (setq solarized-high-contrast-mode-line t)
  (load-theme 'solarized-dark t))

(use-package zenburn
  :disabled t
  :config
  (load-theme 'zenburn t))

(provide 'color-theme-conf)