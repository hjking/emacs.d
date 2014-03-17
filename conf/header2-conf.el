;;
;; Filename: header2-conf.el
;; Description: Setting for header2.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2012-01-11 10:26:45
;;
(message "%d: >>>>> Loading [ header2 ] Customizations ...." step_no)
(setq step_no (1+ step_no))

(require 'header2)

(setq header-copyright-notice "Copyright (c) 2014, Fiberhome Telecommunication Technology Co., Ltd.\nMicroelectronics Dept. All rights reserved.\n")
(setq header-author 'user-full-name)
(setq header-file-name 'buffer-file-name)
(setq header-creation-date 'current-time-string)
(setq header-modification-author 'user-full-name)

(setq make-header-hook '(
                         header-blank
                         header-copyright
                         header-blank
                         header-file-name
                         header-author
                         header-creation-date
                         header-version
                         header-description
                         header-modification-date
                         header-end-line
                         header-history
                         header-code
                         header-eof))

;; add a file header whenever you create a new file in some mode
(autoload 'auto-make-header "header2")
(add-hook 'emacs-lisp-mode-hook 'auto-make-header)
(add-hook 'c-mode-hook          'auto-make-header)
(add-hook 'c-mode-common-hook   'auto-make-header)
(add-hook 'verilog-mode-hook    'auto-make-header)
(add-hook 'vlog-mode-hook       'auto-make-header)
(add-hook 'python-mode-hook     'auto-make-header)
(add-hook 'cperl-mode-hook      'auto-make-header)
(add-hook 'makefile-mode-hook   'auto-make-header)
(add-hook 'sh-mode-hook         'auto-make-header)

;; update file headers automatically whenever you save a file
(autoload 'auto-update-file-header "header2")
(add-hook 'write-file-hooks     'auto-update-file-header)
