;;
;; Filename: header2-conf.el
;; Description: Setting for header2.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2023-08-22 08:42:20
;;

(use-package header2
  :defer t
  :commands (auto-update-file-header auto-make-header)
  :init
  (progn
    ; (add-hook 'emacs-lisp-mode-hook 'auto-make-header)
    (defconst hjking/auto-headers-hooks '(verilog-mode-hook
                                          emacs-lisp-mode-hook
                                          c-mode-hook
                                          c-mode-common-hook
                                          vlog-mode-hook
                                          python-mode-hook
                                          makefile-mode-hook
                                          sh-mode-hook
                                          cperl-mode-hook)
      "List of hooks of major modes in which headers should be auto-inserted.")

    (defun hjking/turn-on-auto-headers ()
      "Turn on auto headers only for specific modes."
      (interactive)
      (dolist (hook hjking/auto-headers-hooks)
        (add-hook hook #'auto-make-header)))

    (defun hjking/turn-off-auto-headers ()
      "Turn off auto headers only for specific modes."
      (interactive)
      (dolist (hook hjking/auto-headers-hooks)
        (remove-hook hook #'auto-make-header)))

    (setq header-copyright-notice "XXX Technology Co., Ltd. All rights reserved.\n")
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
    (hjking/turn-on-auto-headers)

    ;; Update headers on save
    (add-hook 'write-file-hooks #'auto-update-file-header)

  ))

(provide 'header2-conf)
