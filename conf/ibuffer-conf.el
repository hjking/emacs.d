
;; Filename: ibuffer-conf.el
;; Description: Setting for ibuffer.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2013-12-31 15:33:39
;;

(use-package ibuffer
  :commands ibuffer
  :bind ("C-x C-b"  .  ibuffer)
  :init (progn
      (defalias 'list-buffers 'ibuffer)
      (setq ibuffer-default-sorting-mode 'major-mode)
      (setq ibuffer-elide-long-columns t)
      (setq ibuffer-eliding-string "&")
      (setq ibuffer-filter-group-name-face 'font-lock-doc-face)
      (setq ibuffer-delete-window-on-quit t)
      (setq ibuffer-expert t)
      (setq ibuffer-show-empty-filter-groups nil)
      (setq ibuffer-display-summary nil)
      (setq ibuffer-formats
            '((mark modified read-only
                    " "
                    (name 18 18 :left :elide)
                    " "
                    (size-h 9 -1 :right)
                    " "
                    (mode 16 16 :left :elide)
                    " "
                    ; (vc-status 16 16 :left)
                    ; " "
                    filename-and-process)
              (mark modified read-only
                    (name 45 -1 :left)
                    " "
                    filename-and-process)
              (mark modified read-only
                    filename-and-process)))
      ;; grouping
      (setq ibuffer-saved-filter-groups
            '(("default"
                ("emacs"      (name . "\\*.*\\*"))
                ("Dirs"       (mode . dired-mode))
                ("Shell"      (or (mode . term-mode)
                                  (mode . eshell-mode)
                                  (mode . shell-mode)))
                ("HDL"        (or (mode . verilog-mode)
                                  (mode . vhdl-mode)
                                  (mode . vlog-mode)))
                ("C"          (or
                               (mode . c-mode)
                               (mode . cc-mode)
                               (mode . c++-mode)))
                ("Elisp"      (or
                               (mode . emacs-lisp-mode)
                               (mode . lisp-interaction-mode)))
                ("Perl"       (mode . cperl-mode))
                ("Python"     (mode . python-mode))
                ("Org"        (or
                               (name . "^\\*Calendar\\*$")
                               (name . "^diary$")
                               (mode . org-mode)
                               (mode . org-agenda-mode)))
                ("Music"      (name . "^EMMS Music Playlist$"))
                ("Tags"       (name . "^TAGS\\(<[0-9]+>\\)?$"))
                ("IRC"        (mode . erc-mode))
                ("Markdown"
                             (or
                              (mode . markdown-mode)))
                ("Web"          (or
                               (mode . css-mode)
                               (mode . web-mode)))
                )))
  )
  :config (progn
    ;; Use human readable Size column instead of original one
    (define-ibuffer-column size-h
      (:name "Size" :inline t)
      (cond
       ((> (buffer-size) 1000000) (format "%7.3fM" (/ (buffer-size) 1000000.0)))
       ((> (buffer-size) 1000) (format "%7.3fk" (/ (buffer-size) 1000.0)))
       (t (format "%8d" (buffer-size)))))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'ibuffer-conf)